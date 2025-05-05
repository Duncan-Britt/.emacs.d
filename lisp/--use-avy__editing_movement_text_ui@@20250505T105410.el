;;; --use-avy__editing_movement_text_ui@@20250505T105410.el --- use-avy -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-avy
;; keywords: :editing:movement:text:ui:
;; date: [2025-05-05 Mon 10:54]
;; identifier: 20250505T105410

;;; Code:
(use-package avy
  :ensure t
  :bind
  (("C-;" . avy-goto-word-1))
  :config
  (setq avy-all-windows 'all-frames))

(provide '--use-avy__editing_movement_text_ui@@20250505T105410)
;;; --use-avy__editing_movement_text_ui@@20250505T105410.el ends here
