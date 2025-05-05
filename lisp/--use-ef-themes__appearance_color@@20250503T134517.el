;;; --use-ef-themes__appearance_color@@20250503T134517.el --- color -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-ef-themes
;; keywords: :appearance:color:
;; date: [2025-05-03 Sat 13:45]
;; identifier: 20250503T134517

;; Start up theme:
;; (mapc #'disable-theme custom-enabled-themes) ; Disable all other themes to avoid awkward blending:
;; (load-theme 'ef-dream :no-confirm)
;; (ef-themes-select 'ef-dream)

;; use this to load the theme which also calls `ef-themes-post-load-hook':
;; (ef-themes-select 'ef-winter)

;; The themes we provide are recorded in the `ef-themes-dark-themes' and `ef-themes-light-themes'.

;; We also provide these commands, but do not assign them to any key:
;;
;; - `ef-themes-toggle'
;; - `ef-themes-select'
;; - `ef-themes-select-dark'
;; - `ef-themes-select-light'
;; - `ef-themes-load-random'
;; - `ef-themes-preview-colors'
;; - `ef-themes-preview-colors-current'
;;; Code:
(use-package ef-themes
  ;; Make customisations that affect Emacs faces BEFORE loading a theme
  ;; (any change needs a theme re-load to take effect).
  :after (fontaine org-bullets) ;; If I ever switch back to org bullets (from org modern, load this after org-bullets)
  :ensure t
  :config
  (setq ef-themes-headings
        '((0 variable-pitch light 1.5)
          (1 variable-pitch light 1.4)
          (2 variable-pitch regular 1.3)
          (3 variable-pitch regular 1.2)
          (4 variable-pitch regular 1.1)
          (5 variable-pitch 1.1) ; absence of weight means `bold'
          (6 variable-pitch 1.1)
          (7 variable-pitch 1.1)
          (t variable-pitch 1.1)))
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t))

(provide '--use-ef-themes__appearance_color@@20250503T134517)
;;; --use-ef-themes__appearance_color@@20250503T134517.el ends here
