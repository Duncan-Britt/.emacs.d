;;; --color__appearance@@20250503T134517.el --- color -*- lexical-binding: t -*-

;;; Commentary:
;; title: color
;; keywords: :appearance:
;; date: [2025-05-03 Sat 13:45]
;; identifier: 20250503T134517

;;; Code:
(require 'portable)

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
  ;; They are nil by default...
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  ;; Start up theme:
  ;; (mapc #'disable-theme custom-enabled-themes) ; Disable all other themes to avoid awkward blending:
  ;; (load-theme 'ef-dream :no-confirm)
  ;; (ef-themes-select 'ef-dream)
  (defun load-last-theme ()
    "Load the last used theme from file."
    (if (file-exists-p theme-state-file)
        (progn
          (mapc #'disable-theme custom-enabled-themes) ; Disable all other themes to avoid awkward blending:
          (let ((last-theme (with-temp-buffer
                              (insert-file-contents theme-state-file)
                              (intern (buffer-string)))))
            (ef-themes-select last-theme)))
      (ef-themes-select 'ef-dream)))

  (load-last-theme)
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
  )

(use-package-local-or-remote
 theme-switcher
 "~/code/my-emacs-packages/theme-switcher/"
 "Duncan-Britt/theme-switcher"
 :after (org ef-themes)
 :demand t
 :init
 (setq *theme-switcher-themes-dark* (mapcar #'symbol-name ef-themes-dark-themes))
 (setq *theme-switcher-themes-light* (mapcar #'symbol-name ef-themes-light-themes))
 :bind
 (:map org-mode-map
       ("C-c C-x C-v" . ts-toggle-inline-images)))

(use-package-local-or-remote
 theme-switcher-consult
 "~/code/my-emacs-packages/theme-switcher-consult/"
 "Duncan-Britt/theme-switcher-consult"
 :after (fontaine theme-switcher ef-themes consult)
 :bind
 ("C-t" . theme-switcher-consult-choose-theme)
 :config
 (defvar theme-state-file (expand-file-name "last-theme" user-emacs-directory)
   "File to save the last used theme.")

 (defun save-current-theme (&rest _)
   "Save the current theme to a file."
   (when-let ((current-theme (car custom-enabled-themes)))
     (with-temp-file theme-state-file
       (insert (symbol-name current-theme)))))

 (advice-add 'ef-themes-select :after #'save-current-theme)
 (advice-add 'theme-switcher-choose-theme :after #'save-current-theme)
 (advice-add 'theme-switcher-consult--apply-theme :after #'save-current-theme))

(use-package hl-todo
  :ensure t
  :config
  (require 'theme-switcher)
  (require 'theme-switcher-consult)
  (require 'ef-themes)
  (require 'hl-todo)
  (defun set-hl-todo-faces-according-to-ef-theme ()
    "Sets the faces of different TODO-esq keywords for the hl-todo package.
Done in accordance with the currently loaded ef-theme."
    (let ((theme (car custom-enabled-themes)))
      (when (and (boundp 'ef-themes-collection)
	     (member theme ef-themes-collection))
	(let* ((palette-var (intern (concat (symbol-name theme) "-palette")))
	       (palette (symbol-value palette-var))
	       (red (cadr (assoc 'red palette)))
	       (green (cadr (assoc 'green palette)))
	       (yellow (cadr (assoc 'yellow palette)))
	       (cyan (cadr (assoc 'cyan palette))))
	  (setq hl-todo-keyword-faces
		`(("TODO" . ,red)
		  ("DONE" . ,green)
		  ("NOTE" . ,yellow)
		  ("FIXME" . ,red)
		  ("OKAY" . ,cyan)))))))

  (with-eval-after-load 'appearance
    (run-with-timer 5 nil (lambda ()
                            (set-hl-todo-faces-according-to-ef-theme)
                            (global-hl-todo-mode))))

  (when (fboundp 'theme-switcher-choose-theme)
    (set-hl-todo-faces-according-to-ef-theme)
    (advice-add 'theme-switcher-choose-theme :after (lambda (&rest _)
						      (set-hl-todo-faces-according-to-ef-theme)
						      (global-hl-todo-mode))))
  (when (fboundp 'theme-switcher-consult-choose-theme)
    (advice-add 'theme-switcher-consult-choose-theme :after (lambda (&rest _)
							    (set-hl-todo-faces-according-to-ef-theme)
							    (global-hl-todo-mode)))))

(provide '--color__appearance@@20250503T134517)
;;; --color__appearance@@20250503T134517.el ends here
