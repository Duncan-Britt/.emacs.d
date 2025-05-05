;;; --use-scratch-comment__elisp_programming@@20250505T090928.el --- use-scratch-comment -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-scratch-comment
;; keywords: :elisp:programming:
;; date: [2025-05-05 Mon 09:09]
;; identifier: 20250505T090928

;;; Code:
(use-package scratch-comment
  :ensure t
  :config
  (defun my/comment-eval-sexp ()
    "Eval sexp before point and print result as comment.
see `scratch-comment-eval-sexp'."
    (interactive)
    (save-excursion
      (let ((orig-buf (current-buffer)))
        (with-temp-buffer
          (let ((scratch-comment-buffer (current-buffer)))
            (with-current-buffer orig-buf (scratch-comment--eval-last-sexp t))
            (string-rectangle (point-min) (point-min) ";;=> ")
            (when (< (line-end-position) (point-max))
              (string-rectangle (progn (goto-char (point-min)) (line-beginning-position 2))
                                (progn (goto-char (point-max)) (line-beginning-position))
                                ";;   "))
            (let ((str (buffer-substring (point-min) (point-max))))
              (with-current-buffer orig-buf
                (end-of-line)
                (let ((eol (point)))
                  (skip-chars-backward " \t")
                  (let ((end-of-expr (point)))
                    (delete-region end-of-expr eol)))
                (insert " ")
                (insert str))))))))

  :bind ((:map emacs-lisp-mode-map
               ("C-j" . my/comment-eval-sexp))))

(provide '--use-scratch-comment__elisp_programming@@20250505T090928)
;;; --use-scratch-comment__elisp_programming@@20250505T090928.el ends here
