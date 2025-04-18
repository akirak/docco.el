;;; docco-ts.el --- Tree-sitter infrastructure for docco -*- lexical-binding: t -*-

;; Copyright (C) 2024 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
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

;; This library provides infrastructure for editing comments based on
;; the built-in support for tree-sitter.

;;; Code:

(require 'treesit)

(declare-function docco--beginning-of-line-comments "docco")
(declare-function docco--bol-or-indent-p "docco")
(declare-function docco--open-line-and-indent "docco")

(cl-defun docco-ts--edit (comment-node-type &key before line-comment
                                            comment-start-regexp
                                            skeleton anywhere
                                            &allow-other-keys)
  (if anywhere
      (cond
       (skeleton
        (skeleton-insert skeleton nil))
       (line-comment
        (insert line-comment " ")))
    (pcase (docco-ts--locate comment-node-type
                             :before before
                             :comment-start-regexp comment-start-regexp)
      (`nil
       (user-error "not effective from this location"))
      (`(,exists . ,node)
       (goto-char (treesit-node-start node))
       (when exists
         ;; There can be multiple continuous lines of comments, so try to locate
         ;; the first one.
         (when line-comment
           (docco--beginning-of-line-comments line-comment)))
       ;; Enter the comment body
       (cond
        ((and comment-start-regexp
              (looking-at comment-start-regexp))
         (goto-char (match-end 0)))
        ((and line-comment
              (looking-at (concat (regexp-quote line-comment) (rx (* blank)))))
         (goto-char (match-end 0)))
        (t
         (docco--open-line-and-indent)
         (cond
          (skeleton
           (skeleton-insert skeleton nil))
          (line-comment
           (insert line-comment " ")))))))))

(cl-defun docco-ts--locate (comment-node-type &key before comment-start-regexp
                                              &allow-other-keys)
  "Returns (EXISTING . NODE) to indicate what to do next."
  (let ((node (docco-ts--find-ancestor-or-self (cons comment-node-type
                                                     (ensure-list before)))))
    ;; Find a location that begins a comment or is suitable for inserting a
    ;; comment
    (cond
     ((equal (treesit-node-type node) comment-node-type)
      (cons t node))
     ((member (treesit-node-type node) (ensure-list before))
      (if-let* ((comment-node (docco-ts--find-previous-sibling
                               node
                               :comment-start-regexp comment-start-regexp
                               :target-type comment-node-type
                               :not-types (ensure-list before))))
          (cons t comment-node)
        (cons nil node)))
     (t
      nil))))

(defun docco-ts--find-ancestor-or-self (node-types)
  (let ((node (treesit-node-at (if (docco--bol-or-indent-p)
                                   (if (looking-at (rx (* blank)))
                                       (match-end 0)
                                     (point))
                                 (point)))))
    (catch 'find-ts-node
      (while node
        (when (and (member (treesit-node-type node) node-types)
                   ;; Require the node to start at bol. This is important for
                   ;; languages like JavaScript/TypeScript which has the
                   ;; optional export keyword.
                   (save-excursion
                     (goto-char (treesit-node-start node))
                     (docco--bol-or-indent-p)))
          (throw 'find-ts-node node))
        (setq node (treesit-node-parent node))))))

(cl-defun docco-ts--find-previous-sibling (start &key target-type not-types
                                                 comment-start-regexp)
  (let ((node start))
    (catch 'find-prev-sibling
      (while node
        (setq node (treesit-node-prev-sibling node))
        (when (and (or (not target-type)
                       (equal (treesit-node-type node) target-type))
                   (or (not comment-start-regexp)
                       (save-excursion
                         (goto-char (treesit-node-start node))
                         (looking-at comment-start-regexp))))
          (throw 'find-prev-sibling node))
        (when (member (treesit-node-type node) not-types)
          (throw 'find-prev-sibling nil))))))

(provide 'docco-ts)
;;; docco-ts.el ends here
