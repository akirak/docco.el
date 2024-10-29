;;; docco-fallback.el --- Fallback backend for docco -*- lexical-binding: t -*-

(require 'pcase)
(require 'cl-lib)

(cl-defun docco-fallback--edit (&key above below limit in-comment-p
                                     match-regexp default skeleton
                                     &allow-other-keys)
  (pcase-exhaustive (docco-fallback--locate :above above
                                            :below below
                                            :limit limit
                                            :in-comment-p in-comment-p
                                            :match-regexp match-regexp)
    (`nil
     (cond
      (above
       (docco-fallback--find-start-above above)
       (goto-char (match-beginning 0)))
      (below
       (re-search-backward below)
       (goto-char (match-end 0))))
     (pcase-exhaustive default
       ((cl-type function)
        (funcall default)))
     (when skeleton
       (skeleton-insert skeleton nil)))
    (`(,_exists . ,pos)
     (goto-char pos)
     (re-search-forward (rx word-start) (line-end-position) t))))

(cl-defun docco-fallback--locate (&key above below limit in-comment-p
                                       match-regexp &allow-other-keys)
  (cond
   ((and above below)
    (error "You can't specify both :above and :limit at the same time"))
   (above
    (save-excursion
      (and (prog1 (docco-fallback--find-start-above above)
             (goto-char (match-beginning 0)))
           (when (re-search-backward match-regexp
                                     (when limit
                                       (docco-fallback--find-limit-backward
                                        limit in-comment-p))
                                     t)
             (cons t (match-end 0))))))
   (below
    (save-excursion
      (and (prog1 (re-search-backward below nil t)
             (goto-char (match-end 0)))
           (when (re-search-forward match-regexp nil t)
             (cons t (point))))))
   (t
    (error "You have to specify either :above or :below"))))

(defun docco-fallback--find-start-above (regexp)
  (or (progn
        (when (looking-at (rx (* blank)))
          (goto-char (match-end 0)))
        (looking-at regexp))
      (re-search-backward regexp nil t)))

(defun docco-fallback--find-limit-backward (regexp &optional in-comment-p)
  (save-excursion
    (catch 'limit
      (while (and (> (point) (point-min))
                  (re-search-backward regexp nil t))
        (goto-char (match-end 0))
        (unless (if in-comment-p
                    (funcall in-comment-p)
                  (ppss-comment-or-string-start (syntax-ppss)))
          (throw 'limit (point)))))))

(provide 'docco-fallback)
;;; docco-fallback.el ends here
