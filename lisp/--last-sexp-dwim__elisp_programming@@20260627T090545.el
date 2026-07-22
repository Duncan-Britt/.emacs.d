;;; --last-sexp-dwim__elisp_programming@@20260627T090545.el --- last-sexp-dwim -*- lexical-binding: t -*-

;;; Commentary:
;; title: last-sexp-dwim
;; keywords: :elisp:programming:
;; date: [2026-06-27 Sat 09:05]
;; identifier: 20260627T090545

;;; Code:
(defun dunc/last-sexp-dwim (&optional prefix-arg)
  "Either insert-eval-sexp or macroexpand sexp (with prefix arg)."
  (interactive "P")
  (if prefix-arg
      (dunc/insert-macro-expand-sexp)
    (dunc/insert-eval-sexp)))

(defun dunc/insert-eval-sexp ()
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

(defun dunc/insert-macro-expand-sexp ()
  "Macroexpand sexp before point and print result as comment.
Result is inserted below the current line, formatted like
`dunc/insert-eval-sexp'."
  (interactive)
  (let* ((sexp (save-excursion
                 (backward-sexp)
                 (read (current-buffer))))
         (result-str (condition-case err
                         (string-trim-right
                          (pp-to-string (macroexpand-1 sexp)))
                       (error (format "Error: %s" (error-message-string err)))))
         (lines (split-string result-str "\n"))
         (commented (string-join
                     (cons (concat ";;=> " (car lines))
                           (mapcar (lambda (l) (concat ";;   " l)) (cdr lines)))
                     "\n")))
    (end-of-line)
    (insert "\n" commented "\n")))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-j") #'dunc/last-sexp-dwim))

(provide '--last-sexp-dwim__elisp_programming@@20260627T090545)
;;; --last-sexp-dwim__elisp_programming@@20260627T090545.el ends here
