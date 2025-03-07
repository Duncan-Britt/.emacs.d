;;; package --- init.el
;;; Commentary:
;;; Code:

;; ===============================================================
;; INSTALLATION
;; ===============================================================
;; For MacOS:
;; ./configure \
;; --with-modules \
;; --with-native-compilation=aot \
;; --with-toolkit-scroll-bars \
;; --with-tree-sitter \
;; --with-json \
;; --without-imagemagick \
;; --without-mailutils \
;; --without-dbus \
;; --with-xinput2 \
;; CFLAGS="-O2 -mtune=native -march=native -fomit-frame-pointer"

;; For Linux:
;; ./configure \
;; --with-modules \
;; --with-native-compilation=aot \
;; --with-x \
;; --with-x-toolkit=lucid \
;; --with-toolkit-scroll-bars \
;; --with-tree-sitter \
;; --with-json \
;; --without-imagemagick \
;; --without-mailutils \
;; --with-xinput2 \
;; --with-cairo \
;; --with-harfbuzz \ <-- Cairo & Harfbuzz combine to provide better font rendering on Linux.
;; CFLAGS="-O2 -mtune=native -march=native -fomit-frame-pointer"


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
(when (and (file-exists-p "~/.safe/safe.el")
           (file-exists-p "~/code/my-emacs-packages/rotor/rotor.el"))
  (load "~/.safe/safe.el")
  (with-eval-after-load 'safe
    (load "~/code/my-emacs-packages/rotor/rotor.el")))

;; ===========================
;; PORTABILITY/SYNCHRONIZATION
;; ===========================
(defmacro use-package-local-or-remote (package-name local-path remote-repo &rest use-package-args)
  `(if (file-directory-p ,local-path)
       (use-package ,package-name
         :ensure (:repo ,local-path)
         ,@use-package-args)
     (use-package ,package-name
         :ensure (:host github :repo ,remote-repo)
         ,@use-package-args)))

(defmacro use-package-when-local (package-name local-path &rest use-package-args)
  `(when (file-directory-p ,local-path)
     (use-package ,package-name
       :ensure (:repo ,local-path)
       ,@use-package-args)))

(global-auto-revert-mode 1)

;; ===============
;; STARTUP
;; ===============

(use-package emacs
  :ensure nil
  :config
  ;; Initial Frame Size
  (add-hook 'window-setup-hook 'toggle-frame-maximized t)
  ;; (setq initial-frame-alist
  ;;       (append initial-frame-alist
  ;;       	'((left   . 20)
  ;;                 (top    . 0)
  ;;                 (width  . 240)
  ;;                 (height . 60))))

  ;; Open Agenda on startup
  (setq inhibit-startup-screen t)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (find-file "~/Dropbox/agenda/agenda.org") ;; <-- org file
              (org-agenda-list) ;; <-- calendar
              (elpaca-log nil t)))
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
           :mode-line-active-family "Iosevka Comfy Motion Duo" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion Duo" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion Duo"
           :header-line-height 1.0)
          (present
           :default-family "Iosevka Duncan"
           :default-height 150
           :fixed-pitch-family "Iosevka Duncan"
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-height 1.0
           :mode-line-active-family "Iosevka Comfy Motion Duo" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion Duo" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion Duo"
           :header-line-height 1.0)
          (prose
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
           :variable-pitch-family  "ETBembo" ;"Symbola" ;"Antykwa Poltawskiego"
           :variable-pitch-height 1.2
           :mode-line-active-family "Iosevka Comfy Motion Duo" ; falls back to :default-family
           :mode-line-active-height 0.8
           :mode-line-inactive-family "Iosevka Comfy Motion Duo" ; falls back to :default-family
           :mode-line-inactive-height 0.8
           :header-line-family "Iosevka Comfy Motion Duo"
           :header-line-height 0.8)))
  (fontaine-mode 1)
  (fontaine-set-preset 'regular))

(use-package-local-or-remote fontaine-org
                             "~/code/my-emacs-packages/fontaine-org/"
                             "Duncan-Britt/fontaine-org"
                             :init
                             (fontaine-org-mode 1))

(use-package ef-themes
  ;; Make customisations that affect Emacs faces BEFORE loading a theme
  ;; (any change needs a theme re-load to take effect).
  :after (fontaine org-bullets)
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

(use-package-local-or-remote theme-switcher
                             "~/code/my-emacs-packages/theme-switcher/"
                             "Duncan-Britt/theme-switcher"
                             :after (org ef-themes)
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
                                     "ef-maris-dark"
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

(use-package-local-or-remote theme-switcher-consult
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

;; ================================================
;; NAVIGATION/DISCOVERABILITY + COMPLETION FRAMEWORK
;; ================================================

(use-package emacs
  :ensure nil
  :config
  (setq context-menu-mode 1))

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

(use-package casual
  :after (calc transient)
  :ensure t
  :bind
  (:map calc-mode-map
        ("C-o" . casual-calc-tmenu)))

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1))

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
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles orderless)))))

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
  (setq cape-dabbrev-check-other-buffers nil)
  :bind ("C-'" . completion-at-point))

(use-package eglot
  :ensure nil
  :hook ((c-mode . eglot-ensure)
	 (c-ts-mode . eglot-ensure)
	 (c++-mode . eglot-ensure)
	 (c++-ts-mode . eglot-ensure)
	 (ruby-mode . eglot-ensure)
	 (ruby-ts-mode . eglot-ensure)
         ;; (python-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (asm-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil)
  (add-to-list 'eglot-server-programs
               '(asm-mode . ("asm-lsp")))
  (add-to-list 'eglot-server-programs
               '(web-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(js-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(elixir-mode . ("~/.elixir-ls/language_server.sh")))
  (add-to-list 'eglot-server-programs
               '(erlang-mode . ("elp" "server")))

  (setq eglot-workspace-configuration ;; FIXME Still debugging these.
        '((harper-ls . (:spell_check t
                                     :sentence_capitalization t
                                     :userDictPath "~/dict.txt"
                                     :fileDictPath "~/.harper/"))))
  (add-to-list 'eglot-server-programs ;; FIXME Still debugging this.
               `((text-mode markdown-mode org-mode) .
                 ("nc" "localhost" "9000"
                  :initializationOptions
                  (:settings (:userDictPath ,(expand-file-name "~/dict.txt")
                                            :fileDictPath ,(expand-file-name "~/.harper/"))
                             :workspace (:spell_check t :sentence_capitalization t))))) ;; python3 lsp_proxy.py 9000 harper-ls --stdio
  )

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
  :bind (("C-c C-;"   . popper-toggle)
         ("s-;"   . popper-cycle))
  :init
  (setq popper-reference-buffers
        '("Output\\*$"
          "\\*Async Shell Command\\*"
          ;; "\\*Org Agenda\\*"
          "\\*slime-repl sbcl\\*"
          "\\*slime-inspector\\*"
          "\\*Ciao\\*"
          "\\*sweeprolog-top-level\\*"
          "\\*prolog\\*"
          "\\*erlang\\*"
          "\\*Python\\*"
          sldb-mode
          "\\*Claude\\*"
          "\\*DeepSeek\\*"
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
                     (window-width . 0.5))))
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
  :after (ob-prolog ob-elixir)
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
     (prolog . t)
     (elixir . t)
     ))
  ;; Needed to run mysql in org babel
  (add-to-list 'exec-path "/usr/local/mysql-8.3.0-macos14-x86_64/bin") ;; <-- doesn't exist on new mac
  (setq org-babel-python-command "python3")
  (setq org-log-note-clock-out t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
  (setq org-image-actual-width nil)
  (setq org-list-allow-alphabetical t)
  (setq org-latex-listings 'minted ;; Export to LateX PDF using minted package
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-export-backends '(ascii html icalendar latex md))

  (setq org-preview-latex-default-process 'dvisvgm) ; Better latex rendering
  (defun my/resize-org-latex-overlays () ; auto resize latex when resizing text
    (cl-loop for o in (car (overlay-lists))
             if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
             do (plist-put (cdr (overlay-get o 'display))
		           :scale (expt text-scale-mode-step
				        text-scale-mode-amount))))
  (plist-put org-format-latex-options :foreground nil) ; latex previews match theme when switching themes.
  (plist-put org-format-latex-options :background nil)

  (setq org-cite-csl-styles-dir "~/code/citation-styles/") ; <-- for Bibtex Bibliography styles on latex export

  (require 'ox-gfm nil t) ;; <-- For github flavored markdown export
  (require 'blog-publishing)
  (require 'ut-table-manager)

  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode)
         (org-mode . pixel-scroll-precision-mode)
         (org-mode . my-org-syntax-table-modify)
         (org-mode . (lambda () (display-line-numbers-mode 0)))
         (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'my/resize-org-latex-overlays nil t)))))

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
  :ensure (:host github :repo "rnkn/olivetti")
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
  :ensure (:host github :repo "misohena/org-cmenu")
  :after org
  :config (require 'org-cmenu-setup)
  :bind
  (:map org-mode-map
        ("M-n" . org-cmenu)))

(use-package-local-or-remote archiver
                             "~/code/my-emacs-packages/archiver/"
                             "Duncan-Britt/emacs-archiver"
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

(use-package ob-async
  :ensure t
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("ipython")))

(use-package-local-or-remote paste-img
                             "~/code/my-emacs-packages/paste-img/"
                             "Duncan-Britt/paste-img"
                             :after (org org-download)
                             :hook
                             (org-mode . paste-img-mode))

;; =======================================
;; LINTING, SPELLCHECK
;; =======================================

;; Spell checker
(use-package jinx ;; NOTE Custom variable `jinx-include-faces' can be used to add spell checking to comments and string in programming modes. Also see `jinx-exclude-faces'.
  :ensure t
  :hook
  ((org-mode . jinx-mode)
   (text-mode . jinx-mode))
  :bind (("M-$" . jinx-correct))) ;; M-x jinx-languages for other languages.

;; =======================================
;; PROGRAMMING
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
	  ;; (python "https://github.com/tree-sitter/tree-sitter-python")
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
          ;; (python-mode . python-ts-mode)
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

(use-package magit
  :after transient
  :ensure t
  :config
  ;; (defun my/is-text-file-using-file-cmd (filename)
  ;;   "Use external 'file' command to detect if FILENAME is a text file."
  ;;   (when (and filename
  ;;              (file-exists-p filename)
  ;;              (file-readable-p filename))
  ;;     (if-let ((file-cmd (executable-find "file")))
  ;;         (with-temp-buffer
  ;;           (when (zerop (call-process file-cmd nil t nil "--brief" "--mime-type"
  ;;                                      (expand-file-name filename)))
  ;;             (goto-char (point-min))
  ;;             (looking-at "text/")))
  ;;       (progn
  ;;         (lwarn 'file-detection :warning
  ;;                "'file' command not found in %s at line %d"
  ;;                (or load-file-name buffer-file-name)
  ;;                (line-number-at-pos))
  ;;         nil))))

  ;; (defun my/delete-trailing-whitespace-in-repo (&rest _args)
  ;;   "Delete trailing whitespace in the current git repository."
  ;;   (when (and (buffer-file-name)
  ;;              (vc-git-root default-directory)
  ;;              (executable-find "file"))
  ;;     (save-excursion
  ;;       (let* ((initially-open-buffers (mapcar #'buffer-file-name (buffer-list)))
  ;;              (default-directory (vc-git-root (buffer-file-name)))
  ;;              (modified-buffer-files (mapcar #'buffer-file-name
  ;;                                             (seq-filter (lambda (buf)
  ;;                                                           (and (buffer-modified-p buf)
  ;;                                                                (buffer-file-name buf)))  ; Check buffer has a file
  ;;                                                         (buffer-list))))
  ;;              (changed-files-in-repo (mapcar (lambda (x) (expand-file-name x))
  ;;                                             (magit-unstaged-files)))
  ;;              (changed-text-files-in-repo (seq-filter (lambda (file-path)
  ;;                                                        (and (my/is-text-file-using-file-cmd file-path)
  ;;                                                             (not (seq-some
  ;;                                                                   (lambda (buf-file)
  ;;                                                                     (file-equal-p (expand-file-name file-path)
  ;;                                                                                   buf-file))
  ;;                                                                   modified-buffer-files))))
  ;;                                                      changed-files-in-repo)))
  ;;         (dolist (file changed-text-files-in-repo)
  ;;           (with-current-buffer (find-file-noselect file)
  ;;             (delete-trailing-whitespace)
  ;;             (save-buffer)
  ;;             (unless (seq-some
  ;;                      (lambda (initially-open-buffer-file)
  ;;                        (file-equal-p (buffer-file-name)
  ;;                                      initially-open-buffer-file))
  ;;                      initially-open-buffers)
  ;;               (kill-buffer))))))))
  ;; (advice-add 'magit-status :before #'my/delete-trailing-whitespace-in-repo)
  )

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

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode))

;; ======
;; ELIXIR
;; ======

(use-package elixir-mode
  :ensure t)

(use-package inf-elixir
  :ensure t)

(use-package mix
  :ensure t)

(use-package ob-elixir
  :ensure t)

;; ======
;; ERLANG
;; ======

(use-package erlang ;; https://www.erlang.org/docs/24/man/erlang.el
  :ensure t)

;; LSP: https://whatsapp.github.io/erlang-language-platform/
;; edts           Erlang Development Tool Suite
;; erlstack-mode  Minor mode for analysing Erlang stacktraces
;; lfe-mode       Flavoured Erlang mode
;; earl           Erlang distribution protocol implementation

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
    (setq c-ts-mode-indent-offset 4)
    (setq c-ts-mode-indent-style 'bsd)
    (define-key c-ts-mode-map (kbd "C-c c") #'recompile))

  (with-eval-after-load 'c++-ts-mode
    (define-key c++-ts-mode-map (kbd "C-c c") #'recompile)))

;; (use-package realgud
;;   :ensure t)

;; (use-package realgud-lldb
;;   :after realgud
;;   :ensure t)

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
(use-package pp
  :ensure nil  ; built-in package
  :bind
  ("C-c C-p" . pp-eval-last-sexp))

;; ==================
;; COMMON LISP
;; ==================

(use-package lisp-mode
  :ensure nil  ; lisp-mode is built-in, so we don't need to ensure it
  :hook ((lisp-mode . prettify-symbols-mode)))
(setq inferior-lisp-program (executable-find "sbcl"))
(setq slime-lisp-implementations
      `((sbcl (,(executable-find "sbcl") "--dynamic-space-size" "4000"))))
;; For hacking on Nyxt:
;; (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
;; (setq slime-lisp-implementations '((nyxt ("/opt/homebrew/bin/sbcl" "--dynamic-space-size 3072")
;;                                          :env ("CL_SOURCE_REGISTRY=/Users/duncan/quicklisp/dists/quicklisp/software//:~/code/nyxt//:~/code/nyxt/_build//"))))
(when (file-directory-p "~/quicklisp")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

;; ======
;; PROLOG
;; ======

(use-package prolog
  :ensure nil  ; prolog is built-in, so we don't need to ensure it
  :mode ("\\.pl\\'" . prolog-mode)
  :config
  (setq prolog-electric-if-then-else-flag t)
  (setq prolog-program-name "scryer-prolog"))

(use-package ciao
  :ensure nil
  :hook ((ciao-mode . display-line-numbers-mode)
         (ciao-mode . my-ciao-remap-faces)
         (ciao-mode . hl-todo-mode))
  :config
  (defun my-ciao-remap-faces ()
    "Remap Ciao faces to standard font-lock faces using face-remapping-alist."
    (face-remap-add-relative 'ciao-face-variable 'font-lock-variable-name-face)
    (face-remap-add-relative 'ciao-face-string 'font-lock-string-face)
    (face-remap-add-relative 'ciao-face-comment 'font-lock-comment-face)
    (face-remap-add-relative 'ciao-face-clauseheadname 'font-lock-function-name-face)
    ;; (face-remap-add-relative 'ciao-face-quoted-atom 'font-lock-constant-face)
    ;; (face-remap-add-relative 'ciao-face-script-header 'font-lock-preprocessor-face)
    ;; (face-remap-add-relative 'ciao-face-concurrency-op 'font-lock-keyword-face)
    (face-remap-add-relative 'ciao-face-cut 'font-lock-negation-char-face)
    ;; (face-remap-add-relative 'ciao-face-funexp-atom 'font-lock-function-name-face)
    ;; (face-remap-add-relative 'ciao-face-builtin-directive 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-predicate-directive 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-module-directive 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-condcode-directive 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-library-directive 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-user-directive 'font-lock-builtin-face)

    ;; (face-remap-add-relative 'ciao-face-checked-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-true-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-false-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-trust-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-entry-assrt 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-check-assrt 'font-lock-preprocessor-face)
    ;; (face-remap-add-relative 'ciao-face-prop-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-test-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-texec-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-type-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-modedef-assrt 'font-lock-builtin-face)

    (face-remap-add-relative 'ciao-face-prompt 'font-lock-keyword-face)
    ;; (face-remap-add-relative 'ciao-face-answer-var 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-answer-val 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-yes-answer 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-no-answer 'font-lock-keyword-face)

    ;; (face-remap-add-relative 'ciaopp-option 'font-lock-builtin-face)

    ;; (face-remap-add-relative 'ciao-face-startup-message 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-error-mess 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-warning-mess 'font-lock-warning-face)
    ;; (face-remap-add-relative 'ciao-face-note-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-passed-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-failed-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-aborted-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-other-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-highlight-code 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-selection-5-face 'font-lock-builtin-face)

    ;; (face-remap-add-relative 'ciao-face-lpdoc-command 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-lpdoc-verbatim 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-lpdoc-comment 'font-lock-doc-face)
    ;; (face-remap-add-relative 'ciao-face-lpdoc-bug-comment 'font-lock-builtin-face)

    ;; (face-remap-add-relative 'ciao-face-debug-call 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-exit 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-fail 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-redo 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-expansion 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-breakpoint 'font-lock-builtin-face)
    ))

(use-package sweeprolog
  :ensure t
  ;; :mode ("\\.pl\\'" . sweeprolog-mode)
  :hook ((sweeprolog-mode . sweeprolog-electric-layout-mode)
         ;; (sweeprolog-mode . (lambda ()
         ;;                     (push '(":-" . "←") prettify-symbols-alist)
         ;;                     (prettify-symbols-mode 1)))
         )
  :config
  (setq sweeprolog-enable-flymake nil))

(use-package ob-prolog
  :ensure t)

;; =====================
;; PYTHON
;; =====================

(use-package exec-path-from-shell ; Without this, eshell would use the homebrew version of python.
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package python
  :ensure nil
  :config
  (when (and (eq system-type 'gnu/linux)
             (string= (system-name) "duncans-macbookpro"))
    (setq python-shell-interpreter "python3.10")))

(use-package lark-mode
  :ensure t)

;; (use-package jupyter-ascending
;;   :ensure (:repo "~/code/my-emacs-packages/jupyter-ascending/")
;;   :custom
;;   (jupyter-ascending-python-command "python3")
;;   :bind (:map jupyter-ascending-mode-map
;;               ("C-c C-c" . jupyter-ascending-run-cell)
;;               ("C-c C-a" . jupyter-ascending-run-all-cells)))

;; =========
;; ASSEMBLY
;; =========

(add-hook 'asm-mode-hook
          (lambda ()
            (setq comment-start "#")
            (setq comment-end "")))

;; =====
;; TLA+
;; =====
;; See lisp/tla+-mode.el
(require 'tla+-mode)

(use-package tla+-mode
  :ensure nil
  :mode ("\\.tla\\'" . tla+-mode)
  :hook (tla+-mode . (lambda ()
                       (tla+/load-symbols)
                       (prettify-symbols-mode 1))))

;; ========
;; PlantUML
;; ========

(use-package plantuml-mode
  :ensure t
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/Downloads/installers/plantuml-mit-1.2025.1.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; =====================
;; RESEARCH & STUDYING
;; =====================

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package org-pdftools
  :ensure t
  :hook (org-mode . org-pdftools-setup-link))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(use-package anki-editor
  :ensure t)

;; =====================
;; MISCELLANEOUS
;; =====================

(require 'morning-pages)

(use-package time
  :ensure nil
  :config
  (setq world-clock-list
        '(("America/Denver" "Denver")
          ("Asia/Saigon" "Saigon")
          ("America/New_York" "Princeton")))
  (setq world-clock-time-format "%t%A %B %_d%n%l:%M %p%n===============================")
  ;; C-h f format-time-string
  ;; %d <- day of month, e.g. `03'
  ;; %A <- week day, e.g. `Monday'
  ;; %B <- month, e.g. `March'
  ;; %R <- military time, e.g. `22:35'
  ;; %Z <- time zone, e.g. `+07' or `EST'
  ;; %I <- 12 hour clock hour, %l is blank padded version
  )

(use-package djvu
  :ensure t)

(use-package reverso ;; Translation, Thesaurus, Grammar Checking (Online only)
  :ensure t)

(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/Dropbox/notes/"))
  :hook (dired-mode . denote-dired-mode))

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

(use-package emacs
  :ensure nil
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq large-file-warning-threshold 30000000)
  (save-place-mode 1))

(use-package-when-local directory-slideshow
                        "~/code/my-emacs-packages/directory-slideshow/")

(use-package transient
  :ensure t)

(use-package gptel
  :after transient
  :ensure t
  :bind (("C-c RET" . gptel-send))
  :config
  (require 'safe)
  ;; (setq
  ;; gptel-model 'aya:latest
  ;; gptel-backend (gptel-make-ollama "Ollama"   ;Any name of your choosing
  ;;                 :host "localhost:11434"     ;Where it's running
  ;;                 :stream t                   ;Stream responses
  ;;                 :models '(aya:latest)))     ;List of models
  ;; (setq
  ;;  gptel-model 'qwen2.5-coder:14b
  ;;  gptel-backend (gptel-make-ollama "Ollama"
  ;;                  :host "localhost:11434"
  ;;                  :stream t
  ;;                  :models '(qwen2.5-coder:14b qwen2.5-coder:32b aya:latest)))

  ;; (setq
  ;;  gptel-model 'claude-3-5-sonnet-20240620 ;  'claude-3-opus-20240229 also available
  ;;  gptel-backend (gptel-make-anthropic "Claude"
  ;;                  :stream t :key *a-gptel-token*))
  (gptel-make-anthropic "Claude"
    :stream t :key *a-gptel-token*)

  (setq gptel-model   'deepseek-coder
        gptel-backend
        (gptel-make-openai "DeepSeek" ; Any name you want
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key *deep-token* ; can be a function that returns the key
          :models '(deepseek-chat deepseek-coder)))

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
   '(yasnippet vertico sql-indent smartparens rainbow-delimiters projectile popper pdf-tools orderless marginalia magit inf-ruby gptel fontaine flycheck ef-themes dired-preview consult breadcrumb ace-window))
 '(safe-local-variable-values
   '((eval progn
           (add-to-list 'load-path
                        (expand-file-name "dev"
                                          (locate-dominating-file default-directory ".dir-locals.el")))
           (require 'compyle))
     (eval progn
           (add-to-list 'load-path
                        (expand-file-name "dev"
                                          (locate-dominating-file default-directory ".dir-locals.el")))
           (require 'define-course)
           (require 'planner-slime))
     (eval progn
           (add-to-list 'load-path
                        (expand-file-name "dev"
                                          (locate-dominating-file default-directory ".dir-locals.el")))
           (require 'define-course)
           (require 'describe))))
 '(tla+-tlatools-path "~/Downloads/installers/tla2tools.jar"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; @begin(36059606)@ - Do not edit these lines - added automatically!
;; (if (file-exists-p "/home/duncan/.ciaoroot/v1.24.0-m1/ciao_emacs/elisp/ciao-site-file.el")
;;   (load-file "/home/duncan/.ciaoroot/v1.24.0-m1/ciao_emacs/elisp/ciao-site-file.el"))
; @end(36059606)@ - End of automatically added lines.
