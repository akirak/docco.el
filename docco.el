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

(defun docco--edit-comment (type)
  (pcase-exhaustive (docco--current-settings)
    ((and (map :treesit :treesit-patterns)
          (guard treesit)
          (let `(,_ ,comment-node-type . ,plist) (assq type treesit-patterns)))
     (require 'treesit)
     (apply #'docco-ts--edit comment-node-type plist))))

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

(provide 'docco)
;;; docco.el ends here
