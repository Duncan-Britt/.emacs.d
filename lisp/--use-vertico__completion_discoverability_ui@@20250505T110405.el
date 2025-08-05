;;; --use-vertico__completion_discoverability_ui@@20250505T110405.el --- use-vertico -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-vertico
;; keywords: :completion:discoverability:ui:
;; date: [2025-05-05 Mon 11:04]
;; identifier: 20250505T110405

;;; Code:
;; TODO: On move to Emacs 30, replace with built-ins: https://www.youtube.com/watch?v=-ZeoGVtMLaU
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  :config
  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(provide '--use-vertico__completion_discoverability_ui@@20250505T110405)
;;; --use-vertico__completion_discoverability_ui@@20250505T110405.el ends here
