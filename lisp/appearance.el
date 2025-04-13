;;; appearance.el --- Themes, Fonts, etc... -*- lexical-binding: t; -*-

;;; Commentary:
;; ┌────────────┐
;; │ Appearance │
;; └────────────┘
;;; Code:
(require 'portable)

(use-package emacs
  :ensure nil
  :config
  (setq-default truncate-lines t)
  (tool-bar-mode -1)    ; Remove toolbar
  (scroll-bar-mode -1)  ; Remove scroll bar
  (blink-cursor-mode)
  (global-hl-line-mode)
  (easy-menu-add-item global-map '(menu-bar edit)
                      ["Emoji & Symbols"
                       ns-do-show-character-palette
                       :help "Show macOS Character Palette."
                       :visible (eq window-system 'ns)])
  ;; ┌───────────┐
  ;; │ Mode Line │
  ;; └───────────┘
  (defvar-local my/modeline-major-mode
      '(:eval
        (when (mode-line-window-selected-p)
          (propertize (format " %s " (capitalize (symbol-name major-mode)))
                              'face 'normal
                              'mouse-face 'mode-line-highlight
                              'help-echo "Describe mode"
                              'local-map (let ((map (make-sparse-keymap)))
                                           (define-key map [mode-line mouse-1] 'describe-mode)
                                           map))))
    "Mode line construct to display the major mode.")

  (defvar prot-modeline-vc-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1] 'vc-diff)
      (define-key map [mode-line down-mouse-3] 'vc-root-diff)
      map)
    "Keymap to display on VC indicator.")

  (defun prot-modeline--vc-text (file branch &optional face)
    "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
    (concat
     (propertize (char-to-string #xE0A0) 'face 'shadow)
     " "
     (propertize branch
                 'face face
                 'mouse-face 'mode-line-highlight
                 'help-echo (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
                                    (vc-working-revision file))
                 'local-map prot-modeline-vc-map)
     "  "))

  (defun prot-modeline--vc-details (file branch &optional face)
    "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
    (prot-modeline--vc-text file branch face))

  (defvar prot-modeline--vc-faces
    '((added . vc-locally-added-state)
      (edited . vc-edited-state)
      (removed . vc-removed-state)
      (missing . vc-missing-state)
      (conflict . vc-conflict-state)
      (locked . vc-locked-state)
      (up-to-date . vc-up-to-date-state))
    "VC state faces.")

  (defun prot-modeline--vc-get-face (key)
    "Get face from KEY in `prot-modeline--vc-faces'."
    (alist-get key prot-modeline--vc-faces 'up-to-date))

  (defun prot-modeline--vc-face (file backend)
    "Return VC state face for FILE with BACKEND."
    (prot-modeline--vc-get-face (vc-state file backend)))

  (defun prot-modeline--vc-branch-name (file backend)
    "Return capitalized VC branch name for FILE with BACKEND."
    (when-let ((rev (vc-working-revision file backend))
               (branch (or (vc-git--symbolic-ref file)
                           (substring rev 0 7))))
      (capitalize branch)))

  (defvar-local prot-modeline-vc-branch
      '(:eval
        (when-let* (((mode-line-window-selected-p))
                    (file (buffer-file-name))
                    (backend (vc-backend file))
                    (branch (prot-modeline--vc-branch-name file backend))
                    (face (prot-modeline--vc-face file backend)))
          (prot-modeline--vc-details file branch face)))
    "Mode line construct to return propertized VC branch.")

  (defvar-local my/modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (list
         (propertize " KMacro " 'face 'ansi-color-inverse)
         " ")))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

  (defvar-local my/modeline-meow-indicator
      '(:eval
        (when (mode-line-window-selected-p)
          (let* ((indicator (meow-indicator))
                 (mode-name (substring-no-properties indicator))
                 (padded-name (format " %-7s" (string-trim mode-name))))
            (list
             (cond
              ((string-equal (string-trim mode-name) "INSERT")
               (propertize padded-name 'face 'ansi-color-inverse))
              ((string-equal (string-trim mode-name) "NORMAL")
               (propertize padded-name 'face 'ansi-color-inverse))
              (t
               (propertize padded-name 'face 'ansi-color-inverse)))
             " "))))
    "Mode line construct to display meow mode, e.g. INSERT, NORMAL, etc.")

  (defvar-local my/modeline-input-method
    '(:eval
      (when current-input-method-title
        (propertize (format " %s " current-input-method-title)
                    'face 'ansi-color-inverse
                    'help-echo (format "Current input method: %s" current-input-method))))
    "Mode line construct to report the multilingual environment.")

  (defvar-local my/modeline-time
      '(:eval
        (when (mode-line-window-selected-p)
          (propertize (format-time-string " %I:%M %p ")
                      'face 'normal
                      'mouse-face 'mode-line-highlight
                      'help-echo "Show world clock"
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1] 'world-clock)
                                   (define-key map [header-line mouse-1] 'world-clock)
                                   map))))
    "Mode line construct to display the current time with a world-clock on click.")

  (defvar-local my/modeline-date
      '(:eval
        (when (mode-line-window-selected-p)
          (propertize (format-time-string " %a, %b %-d  ")
                      'face 'normal
                      'mouse-face 'mode-line-highlight
                      'help-echo (format-time-string "%A, %B %-d. Show calendar")
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1] 'calendar)
                                   (define-key map [header-line mouse-1] 'calendar)
                                   map))))
    "Mode line construct to display the current date with calendar on click.")


  (display-battery-mode 1)
  (defun my-battery-update-advice (orig-fun &rest args)
    "Add custom face to battery indicator when battery is normal."
    (let ((result (apply orig-fun args)))
      (when battery-mode-line-string
        (let ((len (length battery-mode-line-string))
              (percentage (and battery-status-function
                               (car (read-from-string
                                     (cdr (assq ?p (funcall battery-status-function))))))))
          (when (and (numberp percentage)
                     (>= percentage battery-load-low))
            (add-face-text-property 0 len 'normal t battery-mode-line-string))))
      result))
  (advice-add 'battery-update :around #'my-battery-update-advice)
  (setq battery-mode-line-format "【%b%p%%】")
  (defvar-local my/modeline-battery
      '(:eval
        (when (mode-line-window-selected-p)
          'battery-mode-line-string)))

  (defvar-local prot-modeline-eglot
      `(:eval
        (when (and (featurep 'eglot) (mode-line-window-selected-p))
          (list '(eglot--managed-mode eglot--mode-line-format)
                " ")))
    "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

  (defvar-local my/modeline-modified
      '(:eval
        (when (and (buffer-file-name) (buffer-modified-p))
          (propertize "●  " 'face 'warning)))
    "mode line construct to indicate unsaved changes.")

  (dolist (construct '(my/modeline-major-mode
                       prot-modeline-vc-branch
                       my/modeline-kbd-macro
                       my/modeline-meow-indicator
                       my/modeline-input-method
                       my/modeline-time
                       my/modeline-date
                       my/modeline-battery
                       my/modeline-modified
                       prot-modeline-eglot))
    (put construct 'risky-local-variable t))

  (setq-default mode-line-format
                '("%e"
                  "  "
                  my/modeline-meow-indicator
                  my/modeline-kbd-macro
                  mode-line-buffer-identification
                  "  "
                  prot-modeline-vc-branch
                  my/modeline-input-method
                  prot-modeline-eglot
                  my/modeline-major-mode
                  ; mode-line-format-right-align TODO uncomment when I move to Emacs 30:
                  my/modeline-battery
                  "  "
                  my/modeline-time
                  "  "
                  my/modeline-date
                  my/modeline-modified
                  ))
  :hook
  ((prog-mode . display-line-numbers-mode)))

;; ┌───────┐
;; │ Fonts │
;; └───────┘
(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'org-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                      ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                      "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                      "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                      "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                      "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                      "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                      "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                      ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                      "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                      "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                      "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                      "\\\\" "://"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (ligature-set-ligatures 'tla+-mode '("<>" "<=" ">=" "~>" "/=" "/\\" "\\/" "=>" "==" "->"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((regular
           :default-family "Iosevka Duncan"
           :default-height 130
           :fixed-pitch-family "Iosevka Duncan"
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-height 1.0
           :mode-line-active-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion"
           :header-line-height 1.0)
          (present
           :default-family "Iosevka Duncan"
           :default-height 150
           :fixed-pitch-family "Iosevka Duncan"
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-height 1.0
           :mode-line-active-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion"
           :header-line-height 1.0)
          (Adelle
           :default-family "Iosevka Duncan"
           :fixed-pitch-family "Iosevka Duncan"
           :fixed-pitch-height 1.0
           :org-level-1-family "Symbola" ;; <-- Requires `fontaine-org'.
           :org-level-2-family "Symbola"
           :org-level-3-family "Symbola"
           :org-level-4-family "Symbola"
           :org-level-5-family "Symbola"
           :org-level-6-family "Symbola"
           :org-level-7-family "Symbola"
           :org-level-8-family "Symbola"
           :variable-pitch-family  "Adelle" ;"Symbola" ;"Antykwa Poltawskiego"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :mode-line-active-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion"
           :header-line-height 1.0)
          (Athelas
           :default-family "Iosevka Duncan"
           :fixed-pitch-family "Iosevka Duncan"
           :fixed-pitch-height 1.0
           :org-level-1-family "Symbola" ;; <-- Requires `fontaine-org'.
           :org-level-2-family "Symbola"
           :org-level-3-family "Symbola"
           :org-level-4-family "Symbola"
           :org-level-5-family "Symbola"
           :org-level-6-family "Symbola"
           :org-level-7-family "Symbola"
           :org-level-8-family "Symbola"
           :variable-pitch-family  "Athelas" ;"ETBembo"
           :variable-pitch-height 1.0
           :mode-line-active-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion"
           :header-line-height 1.0)))
  (fontaine-mode 1))

(use-package-local-or-remote
 fontaine-org
 "~/code/my-emacs-packages/fontaine-org/"
 "Duncan-Britt/fontaine-org"
 :after fontaine
 :config
 (defvar font-state-file (expand-file-name "last-font" user-emacs-directory)
   "File to save the last used theme.")

 (defun load-last-font ()
   "Load the last used theme from file."
   (if (file-exists-p font-state-file)
       (progn
         (let ((last-font (with-temp-buffer
                            (insert-file-contents font-state-file)
                            (intern (buffer-string)))))
           (fontaine-set-preset last-font)))
     (fontaine-set-preset 'regular)))

 (defun save-current-font (&rest _)
   "Save the current theme to a file."
   (when-let ((current-font-preset (car fontaine-preset-history)))
     (with-temp-file font-state-file
       (insert current-font-preset))))

 (fontaine-org-mode 1)
 (load-last-font)
 (advice-add 'fontaine-set-preset :after #'save-current-font))

;; ┌──────────────┐
;; │ Color Themes │
;; └──────────────┘
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

;; ┌─────────────────┐
;; │ Misc Appearance │
;; └─────────────────┘
(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode))

(use-package anzu
  :ensure t
  :init
  (global-anzu-mode 1))

(use-package org
  :ensure nil
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
  (setq org-image-actual-width nil)
  (setq org-preview-latex-default-process 'dvisvgm) ; Better latex rendering
  (defun my/resize-org-latex-overlays () ; auto resize latex when resizing text
    (cl-loop for o in (car (overlay-lists))
             if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
             do (plist-put (cdr (overlay-get o 'display))
		           :scale (expt text-scale-mode-step
				        text-scale-mode-amount))))
  (plist-put org-format-latex-options :foreground nil) ; latex previews match theme when switching themes.
  (plist-put org-format-latex-options :background nil)

  ;; The following was added because using certain latex packages
  ;; caused problems for latex previews, even when export was fine.
  ;; As I recall, this was a problem:
  ;;     #+latex_header: \hypersetup{linktoc = all, colorlinks = true, linkcolor = blue, citecolor = blue}
  ;; Perhaps also this, I can't remember
  ;;    #+latex_header: \usepackage{fontspec}
  ;;    #+latex_header: \setmainfont{Athelas}
  (add-to-list 'org-preview-latex-process-alist
               '(my-header
                 :programs ("latex" "dvisvgm")
                 :description "Custom process ignoring document headers"
                 :message "you need to install the programs: latex and dvisvgm"
                 :image-input-type "dvi"
                 :image-output-type "svg"
                 :image-size-adjust (1.7 . 1.5)
                 :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
                 :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O")
                 :latex-header "\\documentclass{article}
  \\usepackage[usenames]{color}
  \\usepackage{amsmath,amssymb,amsthm}
  \\pagestyle{empty}
  \\begin{document}"))
  (setq org-preview-latex-default-process 'my-header)

  (add-to-list 'org-entities-user '("yhat" "$\\hat{y}$" nil "&#375;" "yhat" "yhat" "ŷ")) ; FIXME Not sure if I'm dealing with latex in a smart way.
  (add-to-list 'org-entities-user '("vdash" "$\\vdash$" nil "&vdash;" "vdash" "vdash" "⊢"))
  ;; Latin-1 Table: https://cs.stanford.edu/people/miles/iso8859.html
  ;; C-h v org-entities-user RET
  :custom
  (org-hide-emphasis-markers t) ; Hide /emphasis/ markers in org mode
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . variable-pitch-mode)
   (org-mode . visual-line-mode)
   (org-mode . pixel-scroll-precision-mode)
   (org-mode . (lambda () (display-line-numbers-mode 0)))
   (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'my/resize-org-latex-overlays nil t)))))

(use-package olivetti
  :ensure (:host github :repo "rnkn/olivetti")
  :custom (olivetti-body-width 140)
  :hook (org-mode . olivetti-mode))

(use-package org-bullets
  :after org
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; ALTERNATIVES TO ORG BULLETS - org-ibullets and org-modern
;; (use-package org-ibullets ; <- last time I tried this (4/2025), seemed to chop off the left side of the bullets
;;   :after org
;;   :ensure (:host github :repo "jamescherti/org-ibullets.el")
;;   :commands org-ibullets-mode
;;   :hook (org-mode . org-ibullets-mode))
;; (use-package org-modern ; <- source blocks don't play nicely with org-indent-mode
;;   :after org
;;   :ensure t
;;   :hook ((org-mode . org-modern-mode)))

(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t) ; <-- This doesn't work when hyperbole package is loaded.
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t))

(use-package org-fragtog
  ;; automatically toggle latex previews in org mode.
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(provide 'appearance)
