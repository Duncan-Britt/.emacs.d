;;; package --- init.el
;;; Commentary:
;;; Code:

;; ===============================================================
;; INSTALLATION
;; ===============================================================
;; brew install emacs-plus@29 --with-native-comp --with-xwidgets --with-imagemagick --with-modern-black-variant-icon
;; brew install emacs-plus@29 --with-xwidgets --with-imagemagick --with-modern-black-variant-icon

;; ===============================================================
;; PACKAGE MANAGEMENT: ELPACA (see early-init.el)
;; ===============================================================

;; Example Elpaca configuration -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
;; (use-package evil :ensure t :demand t)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
;; (use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

;; ===============
;; AUTOSAVE CRAP
;; ===============
;; Set up a directory for storing backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Set up a directory for storing auto-save files
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;; Create the auto-save directory if it doesn't exist
(make-directory (concat user-emacs-directory "auto-save/") t)

;; Configure auto-save behavior
(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

;; Don't create lock files
(setq create-lockfiles nil)

;; ===============
;; STARTUP
;; ===============

(use-package emacs
  :ensure nil
  :config
  ;; Initial Frame Size
  (setq initial-frame-alist
	(append initial-frame-alist
		'((left   . 20)
                  (top    . 0)
                  (width  . 240)
                  (height . 60))))
  ;; Open Agenda on startup
  (setq inhibit-startup-screen t)
  ;; (add-hook 'emacs-startup-hook
  ;;           (lambda ()
  ;;             (find-file "~/Dropbox/agenda/agenda.org") ;; <-- org file
  ;;             (org-agenda-list)))                       ;; <-- calendar
  )

;; ============================
;; APPEARANCE
;; ============================

(use-package emacs
  :ensure nil
  :config
  ;; Truncate lines by default
  (setq-default truncate-lines t)
  (tool-bar-mode -1)    ; Remove toolbar  
  ;; (scroll-bar-mode -1)  ; Remove scroll bar
  (blink-cursor-mode)
  :hook
  ((prog-mode . display-line-numbers-mode)))

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
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((regular
           :default-family "Iosevka Comfy"
           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Comfy Duo"
           :variable-pitch-height 1.0)
          (prose
           :default-family "Iosevka Comfy"
           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-height 1.0
           :variable-pitch-family "ETBembo"
           :variable-pitch-height 1.2)))
  (fontaine-mode 1)
  (fontaine-set-preset 'regular))

(use-package ef-themes
  ;; Make customisations that affect Emacs faces BEFORE loading a theme
  ;; (any change needs a theme re-load to take effect).
  :after fontaine
  :ensure t
  :config
  (setq ef-themes-headings
        '((0 . (variable-pitch light 1.5))
          (1 . (variable-pitch light 1.4))
          (2 . (variable-pitch regular 1.3))
          (3 . (variable-pitch regular 1.2))
          (4 . (variable-pitch regular 1.1))
          (5 . (variable-pitch 1.1)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.1))
          (7 . (variable-pitch 1.1))
          (t . (variable-pitch 1.1))))
  ;; They are nil by default...
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  ;; Start up theme:
  (mapc #'disable-theme custom-enabled-themes) ; Disable all other themes to avoid awkward blending:
  ;; (load-theme 'ef-dream :no-confirm)
  (ef-themes-select 'ef-dream)
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

(use-package theme-switcher
  :after (org ef-themes)
  :ensure (:repo "~/code/my-emacs-packages/theme-switcher/")
  :demand t
  :init
  (setq *theme-switcher-themes-dark*
        '("ef-trio-dark"
          "ef-rosa"
          "ef-winter"
          "ef-autumn"
          "ef-cherie"
          "ef-tritanopia-dark"
          "ef-elea-dark"
          "ef-dream"
          "ef-melissa-dark"
          "ef-owl"))
  (setq *theme-switcher-themes-light*
        '("ef-day"
          "ef-light"
          "ef-kassio"
          "ef-frost"
          "ef-arbutus"
          "ef-melissa-light"
          "ef-maris-light"
          "ef-elea-light"
          "ef-summer"
          "ef-cyprus"
          "ef-reverie"))
  :bind
  (:map org-mode-map
        ("C-c C-x C-v" . ts-toggle-inline-images)))

(use-package theme-switcher-consult
  :after (theme-switcher consult)
  :ensure (:repo "~/code/my-emacs-packages/theme-switcher-consult/")
  :bind
  ("C-t" . theme-switcher-consult-choose-theme))

(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)

  (defun my-spaceline-theme ()
    "My custom Spaceline theme."
    (setq-default mode-line-format
                  '("%e"
                    mode-line-front-space
                    ;; evil-mode-line-tag
                    mode-line-mule-info
                    mode-line-client
                    mode-line-modified
                    mode-line-remote
                    mode-line-frame-identification
                    mode-line-buffer-identification
                    "   "
                    "   "
                    ;; mode-line-position
                    (vc-mode vc-mode) ;; Remove this line to exclude Git branch
                    "  "
                    ;; mode-line-modes
                    mode-line-misc-info
                    mode-line-end-spaces)))

  ;; Apply your custom theme
  (my-spaceline-theme)
  ;; display time in mode line
  (setq display-time-day-and-date t
        display-time-24hr-format nil)
  (display-time)

  (display-battery-mode 1))

(use-package hl-todo
  :ensure t
  :config 
  (require 'theme-switcher)
  (require 'theme-switcher-consult)
  (require 'ef-themes)
  (require 'hl-todo)
  (global-hl-todo-mode)
  (defun set-hl-todo-faces-according-to-ef-theme ()
    "Provided that ef-themes is loaded, set HL-TODO-KEYWORD-FACES according to the loaded ef-theme."
    (let ((theme (car custom-enabled-themes)))
      (when (string-prefix-p "ef-" (symbol-name theme))
	(let* ((palette-var (intern (concat (symbol-name theme)
					"-palette")))
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
  (set-hl-todo-faces-according-to-ef-theme)
  (advice-add 'theme-switcher-choose-theme :after (lambda (&rest _)
							    (set-hl-todo-faces-according-to-ef-theme)
							    (global-hl-todo-mode)))
  (advice-add 'theme-switcher-consult-choose-theme :after (lambda (&rest _)
							    (set-hl-todo-faces-according-to-ef-theme)
							    (global-hl-todo-mode))))

;; =====================
;; NAVIGATION
;; =====================

(use-package projectile
  :ensure t
  :configure
  (projectile-mode +1)
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

(use-package savehist
  :ensure nil  ; This tells use-package not to try to install savehist
  :init
  (savehist-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  :init
  (setq completion-category-defaults nil))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t)

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; ========================
;; TEXT EDITING & MOVEMENT
;; =======================

(setq next-line-add-newlines t) ;; c-n adds newlines

;; MAKE C-s search case-insensitive:
;; (setq case-fold-search t)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/code/yasnippets"))
  (yas-global-mode 1))

(use-package avy
  :ensure t
  :bind
  (("C-;" . avy-goto-word-1)))

(defun entire-buffer-replace (from to)
  "Do search and replace on entire buffer without moving point.
Display the number of replacements made."
  (interactive "MReplace: \nMWith: ")
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      (while (search-forward from nil t)
        (replace-match to t t)
        (setq count (1+ count)))
      (message "Replaced %d occurrences of '%s'." count from))))

;; ========================
;; WINDOW MANAGEMENT
;; ========================

(use-package popper
  :after projectile
  :ensure t
  :bind (("C-c C-'"   . popper-toggle)
         ("s-'"   . popper-cycle))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Org Agenda\\*"
          "\\*slime-repl sbcl\\*"
	  "\\*Claude\\*"
          help-mode
          compilation-mode))
  (setq popper-group-function #'popper-group-by-projectile)
  
  (defun my/popper-display-popup (buffer &optional alist)
    "Display popup-buffer BUFFER based on the number of windows in the frame."
    (let ((window-count (length (window-list))))
      (if (= window-count 1)
          ;; If there's only one window, display on the right
          (display-buffer-in-side-window
           buffer
           (append alist
                   `((side . right)
                     (window-width . 0.4))))
        ;; If there are multiple windows, display at the bottom
        (display-buffer-in-side-window
         buffer
         (append alist
                 `((side . bottom)
                   (window-height . 0.5)))))))

  (setq popper-display-function #'my/popper-display-popup)
  
  (popper-mode +1)
  (popper-echo-mode +1))

;; ==================
;; ORG MODE
;; ==================

(defun my-org-syntax-table-modify ()
  "Modify `org-mode-syntax-table' for the current org buffer."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(use-package org
  :config
  (setq org-agenda-files (list (expand-file-name "~/Dropbox/agenda/agenda.org")))
  ;; (setq org-archive-location "~/Dropbox/agenda/agenda_archive.org::%s_archive") ;; <-- unused? Org Archiver has it's own location.
  ;; (setq org-plantuml-jar-path (expand-file-name "~/plantuml-1.2024.4.jar")) ;; <-- doesn't exist on my new mac
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages ;; Org source block execution
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (ruby . t)
     (js . t)
     (C . t)
     (octave . t)
     (latex . t)
     (lisp . t)
     (dot . t)
     (matlab . t)
     (sql . t)
     (plantuml . t)
     (shell . t)
     ))
  ;; Needed to run mysql in org babel
  (add-to-list 'exec-path "/usr/local/mysql-8.3.0-macos14-x86_64/bin") ;; <-- doesn't exist on new mac
  (setq org-babel-python-command "python3")
  (setq org-log-note-clock-out t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (setq org-image-actual-width nil)
  (setq org-list-allow-alphabetical t)
  (setq org-latex-listings 'minted ;; Export to LateX PDF using minted package
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-export-backends '(ascii html icalendar latex md))
  (require 'ox-gfm nil t) ;; <-- For github flavored markdown export
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode)
         (org-mode . pixel-scroll-precision-mode)
         (org-mode . my-org-syntax-table-modify)
         (org-mode . (lambda () (display-line-numbers-mode 0)))))

(use-package mw-thesaurus
  :ensure t
  :after org
  :bind
  (:map org-mode-map
        ("C-c t" . mw-thesaurus-lookup-dwim)))

(use-package ox-gfm
  :ensure t
  :after org)

(use-package olivetti
  :load-path "~/code/vendored-emacs-packages/olivetti"
  :custom (olivetti-body-width 140)
  :hook (org-mode . olivetti-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)
                             (org-indent-mode 1))))

(use-package org-appear
  :after hyperbole
  :hook (org-mode . org-appear-mode)
  :custom
  (org-hide-emphasis-markers t) ; Hide /emphasis/ markers in org mode
  (org-appear-autolinks t) ; <-- This doesn't work when hyperbole package is loaded.
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t))

(use-package org-cmenu
  :load-path "~/code/vendored-emacs-packages/org-cmenu/"
  :after org
  :config (require 'org-cmenu-setup)
  :bind
  (:map org-mode-map
        ("M-n" . org-cmenu)))

(use-package archiver
  :load-path "~/code/my-emacs-packages/archiver/"
  :after org
  :init
  (setq *archiver-agenda-archive-location*
        (expand-file-name "~/Dropbox/agenda/agenda_archive.org"))
  :bind
  (:map org-mode-map
        ("C-c C-x C-a" . archiver-archive-heading)))

;; automatically toggle latex previews in org mode.
(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(use-package org-download
  :ensure t
  :custom
  (org-download-image-attr-list '("#+attr_org: :width 600")))

;; =====================
;; PROGRAMMING
;; =====================

(use-package sql-indent
  :ensure t
  :hook ((sql-mode . sqlind-minor-mode)
         (cql-mode . sqlind-minor-mode))
  :config
  ;; You can further customize indentation or align rules here if needed
  )

(use-package company
  :ensure t
  :hook
  ((prog-mode . company-mode)))

(use-package smartparens
  :ensure t
  :config
  (sp-local-pair '(emacs-lisp-mode lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode lisp-mode) "`" "`" :actions nil)
  :hook
  ((prog-mode . smartparens-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (lisp-mode . rainbow-delimiters-mode)))

(use-package flycheck
  :ensure t
  :hook
  ((prog-mode . flycheck-mode)))

(use-package magit
  :ensure t)

;; =====================
;; MISCELLANEOUS
;; =====================
(use-package emacs
  :ensure nil
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

(defun calendar-insert-date ()
  "Capture the date at point, exit the Calendar, insert the date."
  (interactive)
  (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
    (calendar-exit)
    (insert (format "<%d-%02d-%02d>" year month day))))

(use-package transient
  :ensure t)

(use-package safe
  :load-path "~/.safe/")

(use-package rotor
  :after safe
  :load-path "~/code/my-emacs-packages/rotor/")

(use-package gptel
  :after safe
  :ensure t
  :config
  (setq
   gptel-model 'claude-3-5-sonnet-20240620 ;  'claude-3-opus-20240229 also available
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t :key *api-token*))
  ;; (setq
  ;; gptel-model 'aya:latest
  ;; gptel-backend (gptel-make-ollama "Ollama"   ;Any name of your choosing
  ;;                 :host "localhost:11434"     ;Where it's running
  ;;                 :stream t                   ;Stream responses
  ;;                 :models '(aya:latest)))     ;List of models
  (setq gptel-default-mode 'org-mode))

;;; init.el ends here
