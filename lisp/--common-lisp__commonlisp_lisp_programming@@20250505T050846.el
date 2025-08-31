;;; --common-lisp__commonlisp_lisp_programming@@20250505T050846.el --- common-lisp -*- lexical-binding: t -*-

;;; Commentary:
;; title: common-lisp
;; keywords: :commonlisp:lisp:programming:
;; date: [2025-05-05 Mon 05:08]
;; identifier: 20250505T050846

;;; Code:
(use-package lisp-mode
  :after slime
  :ensure nil  ; lisp-mode is built-in, so we don't need to ensure it
  :hook ((lisp-mode . prettify-symbols-mode))
  :config
  ;; (when (file-directory-p "~/quicklisp")
  ;;   (load (expand-file-name "~/quicklisp/slime-helper.el")))
  )

(provide '--common-lisp__commonlisp_lisp_programming@@20250505T050846)
;;; --common-lisp__commonlisp_lisp_programming@@20250505T050846.el ends here
