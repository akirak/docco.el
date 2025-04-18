;;; docco.el --- A framework for editing documentation comments -*- lexical-binding: t -*-

;; Copyright (C) 2024 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience
;; URL: https://github.com/akirak/docco.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the main library for docco, a framework for editing documentation
;; comments.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pcase)
(require 'map)

(declare-function docco-ts--locate "docco-ts")
(declare-function docco-ts--edit "docco-ts")
(declare-function docco-fallback--locate "docco-fallback")
(declare-function docco-fallback--edit "docco-fallback")

(defgroup docco nil
  "A framework for documentation comments."
  :group 'editing)

(defcustom docco-mode-alist
  `((elixir-ts-mode
     :patterns
     ((function
       :key ?f
       :above ,(rx symbol-start "def" (? "p") symbol-end)
       :limit ,(rx symbol-start "end" symbol-end)
       :match-regexp ,(rx "@doc \"\"\"")
       :default (lambda () (open-line 1))
       :in-comment-p
       (lambda ()
         (equal (treesit-node-type (treesit-node-at (point)))
                "quoted_content"))
       :skeleton (> "@doc \"\"\"" n _ n "\"\"\""))
      (module
       :key ?m
       :below ,(rx bol "defmodule " (+? anything) " do")
       :match-regexp ,(rx "@moduledoc \"\"\"")
       :default newline-and-indent
       :skeleton (> "@moduledoc \"\"\"" n _ n "\"\"\"" n n))))
    (rust-ts-mode
     :treesit t
     :treesit-patterns
     ((outer
       "line_comment"
       :key ?o
       :line-comment "///"
       :before ("type" "enum_item"
                "function_item" "function_signature_item"
                "trait_item" "impl_item"))
      (inner
       "line_comment"
       :key ?i
       :line-comment "//!"
       :before ("source_file"))))
    (gleam-ts-mode
     :treesit t
     :treesit-patterns
     ((function
       "statement_comment"
       :key ?f
       :before ("function" "type_definition")
       :line-comment "///")
      (module
       "module_comment"
       :key ?m
       :before "source_file"
       :line-comment "////")))
    ((typescript-ts-mode tsx-ts-mode)
     :treesit t
     :treesit-patterns
     ((comment
       "comment"
       :key ?c
       :before ("class_declaration"
                "export_statement"
                "type_alias_declaration"
                "lexical_declaration"
                "function_declaration")
       :comment-start-regexp ,(rx "/**" (? (* blank) "\n" (* blank) "*") (+ blank))
       :skeleton (> "/**" n " * " _ n " */")))))
  ""
  :type '(alist-get :key-type (symbol :tag "Major mode")
                    :value-type plist))

;;;; Infrastructure

(defun docco--current-settings ()
  (when-let* ((mode (apply #'derived-mode-p
                           (flatten-list (mapcar #'car docco-mode-alist)))))
    (seq-some `(lambda (cell)
                 (when (memq ',mode (ensure-list (car cell)))
                   (cdr cell)))
              docco-mode-alist)))

(defun docco-bindings ()
  "Return an alist of (KEY . SYMBOL) for the current mode."
  (pcase-exhaustive (docco--current-settings)
    ((and (map :treesit :treesit-patterns)
          (guard treesit))
     (mapcar (pcase-lambda (`(,type ,_node-type . ,plist))
               (cons (plist-get plist :key) type))
             treesit-patterns))
    ((and (map :patterns)
          (guard patterns))
     (mapcar (pcase-lambda (`(,type . ,plist))
               (cons (plist-get plist :key) type))
             patterns))))

(defun docco--get-mode-settings (type)
  (pcase-exhaustive (docco--current-settings)
    ((and (map :treesit :treesit-patterns)
          (guard treesit)
          (let `(,_ ,comment-node-type . ,plist) (assq type treesit-patterns)))
     (require 'docco-ts)
     (cons 'treesit (cons comment-node-type plist)))
    ((and (map :patterns)
          (guard patterns)
          (let `(,_ . ,plist) (assq type patterns)))
     (require 'docco-fallback)
     (cons 'fallback plist))))

(cl-defun docco--edit-comment (type)
  (pcase-exhaustive (docco--get-mode-settings type)
    (`(treesit ,comment-node-type . ,plist)
     (apply #'docco-ts--edit comment-node-type plist))
    (`(fallback . ,plist)
     (apply #'docco-fallback--edit plist))))

(cl-defun docco--has-comment-p (type)
  (pcase-exhaustive (docco--get-mode-settings type)
    (`(treesit ,comment-node-type . ,plist)
     (car (apply #'docco-ts--locate comment-node-type plist)))
    (`(fallback . ,plist)
     (car (apply #'docco-fallback--locate plist)))))

(defun docco--statuses ()
  (pcase (docco--current-settings)
    ((and (map :treesit :treesit-patterns)
          (guard treesit))
     (require 'docco-ts)
     (mapcar (pcase-lambda (`(,type ,comment-node-type . ,plist))
               (cons type
                     (car (apply #'docco-ts--locate comment-node-type plist))))
             treesit-patterns))
    ((and (map :patterns)
          (guard patterns))
     (require 'docco-fallback)
     (mapcar (pcase-lambda (`(,type . ,plist))
               (cons type
                     (car (apply #'docco-fallback--locate plist))))
             patterns))))

;;;; Helper functions

(defun docco--read-coment-type-by-char ()
  (let* ((alist (docco-bindings))
         (char (or (when (= 1 (length alist))
                     ;; If there is only one candidate for the mode, skip the
                     ;; completion and return the character.
                     (caar alist))
                   (read-char-choice-with-read-key
                    (format "Insert/edit a comment of type (%s): "
                            (mapconcat (pcase-lambda (`(,key . ,type))
                                         (format "%s: %s" (char-to-string key) type))
                                       alist
                                       ", "))
                    (mapcar #'car alist))
                   (error "Not matching a char"))))
    (alist-get char alist)))

(defun docco--beginning-of-line-comments (line-comment)
  "Go to the first line in continuous line comments."
  (let ((regexp (rx-to-string `(and (* blank) (group ,line-comment))))
        (pos (point)))
    (while (looking-at regexp)
      (setq pos (match-beginning 1))
      (beginning-of-line 0))
    (goto-char pos)))

;;;; Commands

;;;###autoload
(defun docco-edit-comment-of-type (type)
  "Insert a documentation comment of TYPE."
  (interactive (list (docco--read-coment-type-by-char)))
  (docco--edit-comment (cl-etypecase type
                         (string (intern type))
                         (symbol type))))

;;;###autoload
(defun docco-edit-module-comment ()
  "Insert a documentation comment for the module containing the point."
  (interactive)
  (docco--edit-comment 'module))

;;;###autoload
(defun docco-edit-function-comment ()
  "Insert a documentation comment for the function at point."
  (interactive)
  (docco--edit-comment 'function))

;;;; Functions (public API)

(defun docco-supported-p ()
  "Return non-nil if the current major mode is supported by Docco."
  (and (docco--current-settings)
       t))

(defun docco-has-module-comment-p ()
  "Return non-nil if the module has a documentation comment on it."
  (docco--has-comment-p 'module))

(defun docco-has-function-comment-p ()
  "Return non-nil if the function has a documentation comment on it."
  (docco--has-comment-p 'function))

(defun docco-comment-statuses ()
  "Return the statuses of comments."
  (docco--statuses))

(provide 'docco)
;;; docco.el ends here
