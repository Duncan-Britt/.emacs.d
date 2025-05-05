;;; --use-sweeprolog__programming_prolog@@20250505T050502.el --- use-sweeprolog -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-sweeprolog
;; keywords: :programming:prolog:
;; date: [2025-05-05 Mon 05:05]
;; identifier: 20250505T050502

;; A mode for SWI Prolog specifically.
;;; Code:
(use-package sweeprolog
  :ensure t
  ;; :mode ("\\.pl\\'" . sweeprolog-mode)
  :hook ((sweeprolog-mode . sweeprolog-electric-layout-mode)
         ;; (sweeprolog-mode . (lambda ()
         ;;                     (push '(":-" . "←") prettify-symbols-alist)
         ;;                     (prettify-symbols-mode 1)))
         )
  :config
  (setq sweeprolog-enable-flymake nil))

(provide '--use-sweeprolog__programming_prolog@@20250505T050502)
;;; --use-sweeprolog__programming_prolog@@20250505T050502.el ends here
