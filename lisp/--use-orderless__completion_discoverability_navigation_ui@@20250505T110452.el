;;; --use-orderless__completion_discoverability_navigation_ui@@20250505T110452.el --- use-orderless -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-orderless
;; keywords: :completion:discoverability:navigation:ui:
;; date: [2025-05-05 Mon 11:04]
;; identifier: 20250505T110452

;;; Code:
;; TODO: On move to Emacs 30, replace with built-ins: https://www.youtube.com/watch?v=-ZeoGVtMLaU
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles orderless)))))

(provide '--use-orderless__completion_discoverability_navigation_ui@@20250505T110452)
;;; --use-orderless__completion_discoverability_navigation_ui@@20250505T110452.el ends here
