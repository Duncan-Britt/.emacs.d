;;; --use-modus-themes__appearance_color_theme@@20250913T145822.el --- use-modus-themes -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-modus-themes
;; keywords: :appearance:color:theme:
;; date: [2025-09-13 Sat 14:58]
;; identifier: 20250913T145822

;;; Code:
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-headings
        '((0 variable-pitch light 1.5)
          (1 variable-pitch light 1.4)
          (2 variable-pitch regular 1.3)
          (3 variable-pitch regular 1.2)
          (4 variable-pitch regular 1.1)
          (5 variable-pitch 1.1) ; absence of weight means `bold'
          (6 variable-pitch 1.1)
          (7 variable-pitch 1.1)
          (t variable-pitch 1.1))))

(provide '--use-modus-themes__appearance_color_theme@@20250913T145822)
;;; --use-modus-themes__appearance_color_theme@@20250913T145822.el ends here
