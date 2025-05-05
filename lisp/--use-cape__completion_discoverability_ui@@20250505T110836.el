;;; --use-cape__completion_discoverability_ui@@20250505T110836.el --- use-cape -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-cape
;; keywords: :completion:discoverability:ui:
;; date: [2025-05-05 Mon 11:08]
;; identifier: 20250505T110836

;;; Code:
;; TODO: On move to Emacs 30, replace with built-ins: https://www.youtube.com/watch?v=-ZeoGVtMLaU
(use-package cape
  :ensure t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-line)
  :config
  (setq cape-dabbrev-min-length 2)
  (setq cape-dabbrev-check-other-buffers nil)
  :bind ("C-'" . completion-at-point))

(provide '--use-cape__completion_discoverability_ui@@20250505T110836)
;;; --use-cape__completion_discoverability_ui@@20250505T110836.el ends here
