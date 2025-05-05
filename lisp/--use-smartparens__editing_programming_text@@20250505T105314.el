;;; --use-smartparens__editing_programming_text@@20250505T105314.el --- use-smartparens -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-smartparens
;; keywords: :editing:programming:text:
;; date: [2025-05-05 Mon 10:53]
;; identifier: 20250505T105314

;;; Code:
(use-package smartparens
  :ensure t
  :config
  (sp-local-pair '(emacs-lisp-mode lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode lisp-mode) "`" "`" :actions nil)
  :hook
  ((prog-mode . smartparens-mode)))

(provide '--use-smartparens__editing_programming_text@@20250505T105314)
;;; --use-smartparens__editing_programming_text@@20250505T105314.el ends here
