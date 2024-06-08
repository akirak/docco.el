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

(require 'pcase)
(require 'map)

(defcustom docco-mode-alist
  `((gleam-ts-mode
     :treesit t
     :treesit-patterns
     ((function
       "statement_comment"
       :before "function"
       :line-comment "///")
      (module
       "module_comment"
       :before "source_file"
       :line-comment "////"))))
  ""
  :type '(alist-get :key-type (symbol :tag "Major mode")
                    :value-type plist))

;;;; Infrastructure

(defun docco--current-settings ()
  (when-let (mode (apply #'derived-mode-p (mapcar #'car docco-mode-alist)))
    (cdr (assq mode docco-mode-alist))))

(defun docco--get-mode-settings (type)
  (pcase-exhaustive (docco--current-settings)
    ((and (map :treesit :treesit-patterns)
          (guard treesit)
          (let `(,_ ,comment-node-type . ,plist) (assq type treesit-patterns)))
     (cons 'treesit (cons comment-node-type plist)))))

(cl-defun docco--edit-comment (type)
  (pcase-exhaustive (docco--get-mode-settings type)
    (`(treesit ,comment-node-type . ,plist)
     (apply #'docco-ts--edit comment-node-type plist))))

(cl-defun docco--has-comment-p (type)
  (pcase-exhaustive (docco--get-mode-settings type)
    (`(treesit ,comment-node-type . ,plist)
     (car (apply #'docco-ts--locate comment-node-type plist)))))

(defun docco--statuses ()
  (pcase (docco--current-settings)
    ((and (map :treesit :treesit-patterns)
          (guard treesit))
     (mapcar (pcase-lambda (`(,type ,comment-node-type . ,plist))
               (cons type
                     (car (apply #'docco-ts--locate comment-node-type plist))))
             treesit-patterns))))

;;;; Commands

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
  (docco--has-comment-p 'function))

(defun docco-has-function-comment-p ()
  "Return non-nil if the function has a documentation comment on it."
  (docco--has-comment-p 'function))

(defun docco-comment-statuses ()
  "Return the statuses of comments."
  (docco--statuses))

(provide 'docco)
;;; docco.el ends here
