;;; --use-dired-narrow__dired_navigation_organization@@20250505T124959.el --- use-dired-narrow -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-dired-narrow
;; keywords: :dired:navigation:organization:
;; date: [2025-05-05 Mon 12:49]
;; identifier: 20250505T124959

;;; Code:
(use-package dired-narrow
  :ensure t
  :bind ((:map dired-mode-map
               ("C-s" . dired-narrow))))

(provide '--use-dired-narrow__dired_navigation_organization@@20250505T124959)
;;; --use-dired-narrow__dired_navigation_organization@@20250505T124959.el ends here
