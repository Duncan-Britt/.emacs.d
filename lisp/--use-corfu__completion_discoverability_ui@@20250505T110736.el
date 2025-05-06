;;; --use-corfu__completion_discoverability_ui@@20250505T110736.el --- use-corfu -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-corfu
;; keywords: :completion:discoverability:ui:
;; date: [2025-05-05 Mon 11:07]
;; identifier: 20250505T110736

;;; Code:
(use-package corfu
  :ensure t
  :hook
  ((prog-mode . corfu-mode)
   (shell-mode . corfu-mode)
   (eshell-mode . corfu-mode))
  :config
  (setq corfu-auto nil)
  (setq corfu-auto-prefix 2) ;; NOTE These only apply if corfu-auto is t
  (setq corfu-auto-delay 0.1)
  (setq corfu-echo-documentation 0.25)
  (global-corfu-mode 1)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

(provide '--use-corfu__completion_discoverability_ui@@20250505T110736)
;;; --use-corfu__completion_discoverability_ui@@20250505T110736.el ends here
