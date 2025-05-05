;;; --use-outline-indent__appearance_navigation_programming_ui@@20250505T104814.el --- use-outline-indent -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-outline-indent
;; keywords: :appearance:navigation:programming:ui:
;; date: [2025-05-05 Mon 10:48]
;; identifier: 20250505T104814

;;; Code:
(use-package outline-indent
  :ensure t
  :custom
  (outline-indent-ellipsis " ▼ ")
  :hook ((prog-mode . outline-indent-minor-mode))
  :bind (:map prog-mode-map
              ("C-<tab>" . outline-cycle)
              ("C-S-<tab>" . outline-cycle-buffer)))

(provide '--use-outline-indent__appearance_navigation_programming_ui@@20250505T104814)
;;; --use-outline-indent__appearance_navigation_programming_ui@@20250505T104814.el ends here
