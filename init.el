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
  (setq inhibit-startup-screen t))

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

;; (use-package ef-themes
;;   ;; Make customisations that affect Emacs faces BEFORE loading a theme
;;   ;; (any change needs a theme re-load to take effect).
;;   :after (fontaine org-bullets)
;;   :ensure t
;;   :config
;;   (setq ef-themes-headings
;;         '((0 variable-pitch light 1.5)
;;           (1 variable-pitch light 1.4)
;;           (2 variable-pitch regular 1.3)
;;           (3 variable-pitch regular 1.2)
;;           (4 variable-pitch regular 1.1)
;;           (5 variable-pitch 1.1) ; absence of weight means `bold'
;;           (6 variable-pitch 1.1)
;;           (7 variable-pitch 1.1)
;;           (t variable-pitch 1.1)))
;;   ;; They are nil by default...
;;   (setq ef-themes-mixed-fonts t
;;         ef-themes-variable-pitch-ui t)
;;   ;; Start up theme:
;;   ;; (mapc #'disable-theme custom-enabled-themes) ; Disable all other themes to avoid awkward blending:
;;   ;; (load-theme 'ef-dream :no-confirm)
;;   ;; (ef-themes-select 'ef-dream)
;;   (defun load-last-theme ()
;;     "Load the last used theme from file."
;;     (if (file-exists-p theme-state-file)
;;         (progn
;;           (mapc #'disable-theme custom-enabled-themes) ; Disable all other themes to avoid awkward blending:
;;           (let ((last-theme (with-temp-buffer
;;                               (insert-file-contents theme-state-file)
;;                               (intern (buffer-string)))))
;;             (ef-themes-select last-theme)))
;;       (ef-themes-select 'ef-dream)))

;;   (load-last-theme)
;;   ;; use this to load the theme which also calls `ef-themes-post-load-hook':
;;   ;; (ef-themes-select 'ef-winter)

;;   ;; The themes we provide are recorded in the `ef-themes-dark-themes' and `ef-themes-light-themes'.

;;   ;; We also provide these commands, but do not assign them to any key:
;;   ;;
;;   ;; - `ef-themes-toggle'
;;   ;; - `ef-themes-select'
;;   ;; - `ef-themes-select-dark'
;;   ;; - `ef-themes-select-light'
;;   ;; - `ef-themes-load-random'
;;   ;; - `ef-themes-preview-colors'
;;   ;; - `ef-themes-preview-colors-current'
;;   )

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
  :after calc
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
         (python-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil)

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

;; write a use package declaration for dabbrev

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
          "\\*Org Agenda\\*"
          "\\*slime-repl sbcl\\*"
          "\\*slime-inspector\\*"
          "\\*Ciao\\*"
          "\\*sweeprolog-top-level\\*"
          "\\*prolog\\*"
          "\\*Python\\*"
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

;; =======================================
;; PROGRAMMING
;; =======================================

(setq-default indent-tabs-mode nil)

(use-package markdown-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (lisp-mode . rainbow-delimiters-mode)))

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

;; =====================
;; PYTHON
;; =====================

(use-package exec-path-from-shell ; Without this, eshell would use the homebrew version of python.
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; =====================
;; MISCELLANEOUS
;; =====================

(use-package djvu
  :ensure t)

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
           (require 'define-course)
           (require 'planner-slime))
     (eval progn
           (add-to-list 'load-path
                        (expand-file-name "dev"
                                          (locate-dominating-file default-directory ".dir-locals.el")))
           (require 'define-course)
           (require 'describe)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
