;;; --use-transient__lib_ui@@20250503T065339.el --- use-transient -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-transient
;; keywords: :lib:ui:
;; date: [2025-05-03 Sat 06:53]
;; identifier: 20250503T065339

;;; Code:
(use-package transient
  :ensure t
  :custom ((transient-detect-key-conflicts t)))

;; Creating a transient menu
;; (cl-flet ((get-summary (str) (replace-regexp-in-string "\\(.*\\)\n.*" "\\1" str)))
;;    (let (elm info)
;;      (with-current-buffer "elisp-mode.el"  ; some target package (with the file open in a buffer)
;;        (save-excursion
;;          (goto-char (point-min))
;;          (while (setq elm (ignore-errors (read (current-buffer))))
;;            (pcase elm
;;              (`(,(or 'defun 'cl-defun) ,fn ,args (interactive) . ,body)
;;               (push `(,fn . "") info))
;;              (`(,(or 'defun 'cl-defun) ,fn ,args (interactive . ,_) . ,body)
;;               (push `(,fn . "") info))
;;              (`(,(or 'defun 'cl-defun) ,fn ,args ,docstring (interactive) . ,body)
;;               (push `(,fn . ,(get-summary docstring)) info))
;;              (`(,(or 'defun 'cl-defun) ,fn ,args ,docstring (interactive . ,_) . ,body)
;;               (push `(,fn . ,(get-summary docstring)) info))))))
;;      (mapcar (lambda (elm) `("" ,(cdr elm) ,(car elm))) (nreverse info))))
;; Do pp-eval-last-sexp. See https://github.com/conao3/transient-dwim.el?tab=readme-ov-file

(provide '--use-transient__lib_ui@@20250503T065339)
;;; --use-transient__lib_ui@@20250503T065339.el ends here
