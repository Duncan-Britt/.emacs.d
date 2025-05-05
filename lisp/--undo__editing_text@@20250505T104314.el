;;; --undo__editing_text@@20250505T104314.el --- undo -*- lexical-binding: t -*-

;;; Commentary:
;; title: undo
;; keywords: :editing:text:
;; date: [2025-05-05 Mon 10:43]
;; identifier: 20250505T104314

;;; Code:
(use-package undo-fu
  :ensure t
  :custom
  (undo-fu-ignore-keyboard-quit t)
  (undo-fu-allow-undo-in-region t)
  :bind
  (("s-z" . undo-fu-only-undo)
   ("s-Z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :init
  (undo-fu-session-global-mode))

(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-ascii-symbols)
  :bind (("C-x u" . vundo)))

(provide '--undo__editing_text@@20250505T104314)
;;; --undo__editing_text@@20250505T104314.el ends here
