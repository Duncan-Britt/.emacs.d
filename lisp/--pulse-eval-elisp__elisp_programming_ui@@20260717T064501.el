;;; --pulse-eval-elisp__elisp_programming_ui@@20260717T064501.el --- pulse-eval-elisp -*- lexical-binding: t -*-

;;; Commentary:
;; title: pulse-eval-elisp
;; keywords: :elisp:programming:ui:
;; date: [2026-07-17 Fri 06:45]
;; identifier: 20260717T064501

;;; Code:
(require 'pulse)
(require 'elisp-mode)

(defun dunc/pulse-defun (&rest _)
  "Pulse defun at point."
  (save-excursion
    (let ((start (if (beginning-of-defun)
                     (point)
                   (user-error "Not in a defun")))
          (end (progn (end-of-defun) (point))))
      (pulse-momentary-highlight-region start end))))

(advice-add #'eval-defun :after #'dunc/pulse-defun)

(defun dunc/pulse-last-sexp (&rest _)
  "Pulse last sexp."
  (save-excursion
    (let ((end (point))
          (start (progn (backward-sexp) (point))))
      (pulse-momentary-highlight-region start end))))

(advice-add #'eval-last-sexp :before #'dunc/pulse-last-sexp)
(advice-add #'dunc/comment-eval-sexp :before #'dunc/pulse-last-sexp)

(defun dunc/pulse-buffer (&rest _)
  "Pulse buffer."
  (pulse-momentary-highlight-region
   (buffer-end 0)
   (buffer-end 1)))

(advice-add #'eval-buffer :before #'dunc/pulse-buffer)

(provide '--pulse-eval-elisp__elisp_programming_ui@@20260717T064501)
;;; --pulse-eval-elisp__elisp_programming_ui@@20260717T064501.el ends here
