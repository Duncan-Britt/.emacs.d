;;; --my-comment-eval-sexp__elisp_programming@@20260627T090545.el --- my-comment-eval-sexp -*- lexical-binding: t -*-

;;; Commentary:
;; title: my-comment-eval-sexp
;; keywords: :elisp:programming:
;; date: [2026-06-27 Sat 09:05]
;; identifier: 20260627T090545

;;; Code:
(defun my/comment-eval-sexp ()
  "Eval sexp before point and print result as comment.
Result is inserted below the current line, formatted as:
  ;;=> value
  ;;   (continued lines if multi-line)"
  (interactive)
  (let* ((sexp (save-excursion
                 (backward-sexp)
                 (read (current-buffer))))
         (result-str (condition-case err
                         (prin1-to-string (eval sexp lexical-binding))
                       (error (format "Error: %s" (error-message-string err)))))
         (lines (split-string result-str "\n"))
         (commented (string-join
                     (cons (concat ";;=> " (car lines))
                           (mapcar (lambda (l) (concat ";;   " l)) (cdr lines)))
                     "\n")))
    (end-of-line)
    (insert "\n" commented "\n")))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-j") #'my/comment-eval-sexp))

(provide '--my-comment-eval-sexp__elisp_programming@@20260627T090545)
;;; --my-comment-eval-sexp__elisp_programming@@20260627T090545.el ends here
