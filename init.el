;;; package --- init.el
;;; Commentary:
;;; Code:

;; ===============================================================
;; INSTALLATION
;; ===============================================================
;; brew install emacs-plus@29 --with-native-comp --with-xwidgets --with-imagemagick --with-modern-black-variant-icon

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

;; ============
;; LOAD PATH
;; ============
(add-to-list 'load-path "~/.emacs.d/lisp/") ;; NOTE: Use (require 'package) to use the code in the lisp directory
(load "~/.safe/safe.el")
(with-eval-after-load 'safe
  (load "~/code/my-emacs-packages/rotor/rotor.el"))

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
  (add-hook 'emacs-startup-hook
            (lambda ()
              (find-file "~/Dropbox/agenda/agenda.org") ;; <-- org file
              (org-agenda-list)))                       ;; <-- calendar
  )

;; ============================
;; APPEARANCE
;; ============================

(use-package emacs
  :ensure nil
  :config
  (setq-default truncate-lines t)
  (tool-bar-mode -1)    ; Remove toolbar
  (scroll-bar-mode -1)  ; Remove scroll bar
  (blink-cursor-mode)
  (global-hl-line-mode)
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
           :default-family "Iosevka"
           :fixed-pitch-family "Iosevka"
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Comfy Duo"
           :variable-pitch-height 1.0)
          (prose
           :default-family "Iosevka Comfy"
           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-height 1.0
           :variable-pitch-family "ETBembo"
           :variable-pitch-height 1.2)
          (etoile
           :default-family "Iosevka Comfy"
           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Etoile"
           :variable-pitch-height 1.2)))
  (fontaine-mode 1)
  (fontaine-set-preset 'regular))

(use-package ef-themes
  ;; Make customisations that affect Emacs faces BEFORE loading a theme
  ;; (any change needs a theme re-load to take effect).
  :after (fontaine org-bullets)
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
  :after (org ef-themes modus-themes)
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
          "ef-owl"
          "modus-vivendi"))
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
  (when (fboundp 'theme-switcher-choose-theme)
    (set-hl-todo-faces-according-to-ef-theme)
    (advice-add 'theme-switcher-choose-theme :after (lambda (&rest _)
						      (set-hl-todo-faces-according-to-ef-theme)
						      (global-hl-todo-mode))))
  (when (fboundp 'theme-switcher-consult-choose-theme)
    (advice-add 'theme-switcher-consult-choose-theme :after (lambda (&rest _)
							    (set-hl-todo-faces-according-to-ef-theme)
							    (global-hl-todo-mode)))))

(use-package anzu
  :ensure t
  :init
  (global-anzu-mode 1))

;; =================================
;; NAVIGATION/MINIBUFFER COMPLETION
;; =================================

(use-package projectile
  :ensure t
  :init
  (when (eq system-type 'darwin)
    (setq projectile-use-git-grep t)) ; macOS-specific setting
  :config
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
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t)

(use-package consult
  :ensure t)

(use-package dired-preview
  :ensure t
  :hook ((dired-mode . dired-preview-mode)))

(use-package casual
  :after calc
  :ensure t
  :bind
  (:map calc-mode-map
   ("C-o" . casual-calc-tmenu)))

;; ========================
;; TEXT EDITING & MOVEMENT
;; =======================

(setq next-line-add-newlines t) ;; c-n adds newlines

;; MAKE C-s search case-insensitive:
;; (setq case-fold-search t)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history")))
  :bind (:map undo-tree-visualizer-mode-map
              ;; go to selected undo state
              ("<return>" . undo-tree-visualizer-quit)
              ;; cancel (return to state before calling undo-tree-visualizer)
              ("q" . undo-tree-visualizer-abort)))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/code/yasnippets"))
  (yas-global-mode 1))

(use-package avy
  :ensure t
  :bind
  (("C-;" . avy-goto-word-1))
  :config
  (setq avy-all-windows 'all-frames))

(use-package smartparens
  :ensure t
  :config
  (sp-local-pair '(emacs-lisp-mode lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode lisp-mode) "`" "`" :actions nil)
  :hook
  ((prog-mode . smartparens-mode)))

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
          sldb-mode
          "\\*Claude\\*"
          "\\*Ollama\\*"
	  "\\*ChatGPT\\*"
	  "\\*Warnings\\*"
	  "\\*compilation\\*"
	  "\\Backtrace\\*"
	  "\\*ruby\\*"
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
                     (window-width . 0.6))))
        ;; If there are multiple windows, display at the bottom
        (display-buffer-in-side-window
         buffer
         (append alist
                 `((side . bottom)
                   (window-height . 0.5)))))))

  (setq popper-display-function #'my/popper-display-popup)

  (popper-mode +1)
  (popper-echo-mode +1))

(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; ==================
;; ORG MODE
;; ==================

(defun my-org-syntax-table-modify ()
  "Modify `org-mode-syntax-table' for the current org buffer.
This fixes the issue where, in org source blocks, < matches )."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(use-package org
  :config
  (add-to-list 'org-entities-user '("yhat" "$\\hat{y}$" nil "&#375;" "yhat" "yhat" "ŷ")) ; TODO Not sure if I'm dealing with latex in a smart way.
  ;; Latin-1 Table: https://cs.stanford.edu/people/miles/iso8859.html
  ;; C-h v org-entities-user RET
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
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.0))
  (setq org-image-actual-width nil)
  (setq org-list-allow-alphabetical t)
  (setq org-latex-listings 'minted ;; Export to LateX PDF using minted package
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-export-backends '(ascii html icalendar latex md))
  (require 'ox-gfm nil t) ;; <-- For github flavored markdown export
  (require 'blog-publishing)
  (require 'ut-table-manager)
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

(use-package ox-epub
  :ensure t
  :after org)

(use-package olivetti
  :ensure (:repo "~/code/vendored-emacs-packages/olivetti")
  :custom (olivetti-body-width 140)
  :hook (org-mode . olivetti-mode))

(use-package org-bullets
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)
                             (org-indent-mode 1))))

(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-hide-emphasis-markers t) ; Hide /emphasis/ markers in org mode
  (org-appear-autolinks t) ; <-- This doesn't work when hyperbole package is loaded.
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t))

(use-package org-cmenu
  :ensure (:repo "~/code/vendored-emacs-packages/org-cmenu/")
  :after org
  :config (require 'org-cmenu-setup)
  :bind
  (:map org-mode-map
        ("M-n" . org-cmenu)))

(use-package archiver
  :after org
  :ensure (:repo "~/code/my-emacs-packages/archiver/")
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

(use-package ob-async
  :ensure t
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("ipython")))

(use-package paste-img
  :after (org org-download)
  :ensure (:repo "~/code/my-emacs-packages/paste-img/")
  :hook
  (org-mode . paste-img-mode))

;; =======================================
;; PROGRAMMING/IDE/IN BUFFER COMPLETION
;; =======================================

(setq-default indent-tabs-mode nil)

(use-package markdown-mode
  :ensure t)

;; Enable built-in tree-sitter support
(use-package treesit
  :ensure nil  ; built-in package
  :config
  (setq treesit-language-source-alist
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (c "https://github.com/tree-sitter/tree-sitter-c")
	  (c++ "https://github.com/tree-sitter/tree-sitter-cpp")
	  (cmake "https://github.com/uyha/tree-sitter-cmake")
	  (common-lisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Enable tree-sitter for languages with available grammars
  (setq major-mode-remap-alist
	'((bash-mode . bash-ts-mode)
	  (c-mode . c-ts-mode)
	  ;; (c++-mode . c++-ts-mode) OKAY I don't use c++-ts-mode because << on new lines don't indent correctly.
          (css-mode . css-ts-mode)
	  (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
	  ;; (ruby-mode . ruby-ts-mode) OKAY I don't use ruby-ts-mode because C-M-f doesn't work in it - Oct. 2024
	  (yaml-mode . yaml-ts-mode)))

  (dolist (mapping major-mode-remap-alist)
    (let* ((ts-mode (cdr mapping))
           (lang (intern (string-remove-suffix "-ts-mode" (symbol-name ts-mode)))))
      (when (and (not (treesit-ready-p lang t))
		 (not (eq lang 'c++))) ; Do c++ manually, interactively.
        (treesit-install-language-grammar lang))))

  (defun check-treesit-grammar-installation ()
    "Check and report the installation status of tree-sitter grammars.
Note that it may show that C++ is not installed even when it is. Check with `M-x c++-ts-mode'"
    (let (installed not-installed)
      (dolist (mapping major-mode-remap-alist)
	(let* ((ts-mode (cdr mapping))
               (lang (intern (string-remove-suffix "-ts-mode" (symbol-name ts-mode)))))
          (if (treesit-ready-p lang t)
              (push lang installed)
            (push lang not-installed))))

      (with-current-buffer (get-buffer-create "*Tree-sitter Status*")
	(erase-buffer)
	(insert "Tree-sitter Grammar Installation Status:\n\n")
	(insert "Note that it may show that C++ is not installed even when it is. Check with `M-x c++-ts-mode`")
	(insert "Installed grammars:\n")
	(dolist (lang installed)
          (insert (format "  - %s\n" lang)))
	(insert "\nNot installed grammars:\n")
	(dolist (lang not-installed)
          (insert (format "  - %s\n" lang)))
	(insert "\nTo install missing grammars, you can use:\n")
	(insert "(treesit-install-language-grammar 'language-name)\n")
	(display-buffer (current-buffer)))))
  ;; (check-treesit-grammar-installation)
  )

;; corfu + orderless + cape + eglot = in buffer completion framework
;; corfu is like vertico
;; cape is like consult

(use-package corfu
  :ensure t
  :hook
  ((prog-mode . corfu-mode)
   ;; (shell-mode . corfu-mode)
   ;; (eshell-mode . corfu-mode)
   )
  :config
  (setq corfu-auto nil)
  (setq corfu-auto-prefix 2) ;; NOTE These only apply if corfu-auto is t
  (setq corfu-auto-delay 0.1)
  (setq corfu-echo-documentation 0.25)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

(use-package cape
  :ensure t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-line)
  :config
  (setq cape-dabbrev-min-length 2)
  (setq cape-dabbrev-check-other-buffers nil))

(use-package eglot
  :ensure nil
  :hook ((c-mode . eglot-ensure)
	 (c-ts-mode . eglot-ensure)
	 (c++-mode . eglot-ensure)
	 (c++-ts-mode . eglot-ensure)
	 (ruby-mode . eglot-ensure)
	 (ruby-ts-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil))

(use-package flycheck
  :ensure t
  :hook
  ((prog-mode . flycheck-mode)))

(use-package emacs
  :ensure nil
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package magit
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (lisp-mode . rainbow-delimiters-mode)))

(use-package sql-indent
  :ensure t
  :hook ((sql-mode . sqlind-minor-mode)
         (cql-mode . sqlind-minor-mode))
  :config
  ;; You can further customize indentation or align rules here if needed
  )

(use-package simple-httpd
  :ensure t)

(use-package inf-ruby
  :ensure t)

;; ===============
;; C/C++ stuff
;; ===============

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-map
              ("C-c c" . recompile)
              :map c++-mode-map
              ("C-c c" . recompile))
  :config
  (setq-default c-basic-offset 4)
  (add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "stroustrup")
            (setq c-basic-offset 4)))
  (with-eval-after-load 'c-ts-mode
    (define-key c-ts-mode-map (kbd "C-c c") #'recompile))
  (with-eval-after-load 'c++-ts-mode
    (define-key c++-ts-mode-map (kbd "C-c c") #'recompile)))

(use-package realgud
  :ensure t)

(use-package realgud-lldb
  :after realgud
  :ensure t)

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (define-key c-mode-map (kbd "C-c c") 'recompile)))

;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (define-key c++-mode-map (kbd "C-c c") 'recompile)))

;; ==================
;; EMACS LISP
;; ==================

(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode) ;; lambda becomes λ.

;; ==================
;; COMMON LISP
;; ==================

(use-package lisp-mode
  :ensure nil  ; lisp-mode is built-in, so we don't need to ensure it
  :hook ((lisp-mode . prettify-symbols-mode)
	 ;; This could be useful to add custom keywords for syntax highlighting, and I could use .dir-locals.el to make it directory specific, I think.
         ;; (lisp-mode . (lambda ()
         ;;                (font-lock-add-keywords nil
         ;;                                        '(("\\<\\(val\\|λ\\)\\>" . font-lock-keyword-face)
         ;;                                          ("\\<\\(is\\|in\\)\\>" . font-lock-function-name-face)))))
	 ))
(setq inferior-lisp-program (executable-find "sbcl"))
(setq slime-lisp-implementations
      `((sbcl (,(executable-find "sbcl") "--dynamic-space-size" "4000"))))
;; For hacking on Nyxt:
;; (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
;; (setq slime-lisp-implementations '((nyxt ("/opt/homebrew/bin/sbcl" "--dynamic-space-size 3072")
;;                                          :env ("CL_SOURCE_REGISTRY=/Users/duncan/quicklisp/dists/quicklisp/software//:~/code/nyxt//:~/code/nyxt/_build//"))))
(load (expand-file-name "~/quicklisp/slime-helper.el"))

;; =====================
;; PYTHON
;; =====================

(use-package exec-path-from-shell ; Without this, eshell would use the homebrew version of python.
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package jupyter-ascending
  :ensure (:repo "~/code/my-emacs-packages/jupyter-ascending/")
  :custom
  (jupyter-ascending-python-command "python3")
  :bind (:map jupyter-ascending-mode-map
              ("C-c C-c" . jupyter-ascending-run-cell)
              ("C-c C-a" . jupyter-ascending-run-all-cells)))

;; =====================
;; MISCELLANEOUS
;; =====================

(use-package reverso ;; Translation, Thesaurus, Grammar Checking (Online only)
  :ensure t)

(defun calendar-insert-date ()
  "Capture the date at point, exit the Calendar, insert the date."
  (interactive)
  (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
    (calendar-exit)
    (insert (format "<%d-%02d-%02d>" year month day))))

(use-package calendar
  :ensure nil
  :config
  (define-key calendar-mode-map (kbd "RET") 'calendar-insert-date))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package emacs
  :ensure nil
  :config
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package transient
  :ensure t)

(use-package gptel
  :after transient
  :ensure t
  :bind (("C-c RET" . gptel-send))
  :config
  (require 'safe)
  ;; (setq gptel-api-key *gptel-token*)
  ;; (setq gptel-model 'gpt-4o)
  ;; (setq
  ;;  gptel-model 'claude-3-5-sonnet-20240620 ;  'claude-3-opus-20240229 also available
  ;;  gptel-backend (gptel-make-anthropic "Claude"
  ;;                  :stream t :key *api-token*))
  ;; (setq
  ;; gptel-model 'aya:latest
  ;; gptel-backend (gptel-make-ollama "Ollama"   ;Any name of your choosing
  ;;                 :host "localhost:11434"     ;Where it's running
  ;;                 :stream t                   ;Stream responses
  ;;                 :models '(aya:latest)))     ;List of models
  (setq
   gptel-model 'qwen2.5-coder:14b
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(qwen2.5-coder:14b qwen2.5-coder:32b aya:latest)))
  (setq gptel-default-mode 'org-mode))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c" "712dda0818312c175a60d94ba676b404fc815f8c7e6c080c9b4061596c60a1db" "d41229b2ff1e9929d0ea3b4fde9ed4c1e0775993df9d998a3cdf37f2358d386b" "937401a2e532f2c8c881b6b3f20d9d4b6b9405bccf72ea6289c9d3f4507eb1ab" "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7" "a75aff58f0d5bbf230e5d1a02169ac2fbf45c930f816f3a21563304d5140d245" "faf642d1511fb0cb9b8634b2070a097656bdb5d88522657370eeeb11baea4a1c" "7b602fe4a324dc18877dde647eb6f2ff9352566ce16d0b888bfcb870d0abfd70" default))
 '(package-selected-packages
   '(yasnippet vertico sql-indent smartparens rainbow-delimiters projectile popper pdf-tools orderless marginalia magit inf-ruby gptel fontaine flycheck ef-themes dired-preview consult breadcrumb ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
