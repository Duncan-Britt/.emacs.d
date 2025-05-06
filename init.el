;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; ┌──────────────┐
;; │ Installation │
;; └──────────────┘

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

;;; Code:
;; ┌────────────────────────────────────────────────┐
;; │ Package Management: Elpaca (see early-init.el) │
;; └────────────────────────────────────────────────┘

(defvar elpaca-installer-version 0.10)
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

;; ┌──────────────────────┐
;; │ Basic Emacs Settings │
;; └──────────────────────┘
(use-package emacs
  :ensure nil
  :config
  ;; ┌──────────────────────────────┐
  ;; │ Autosave, Backups, Lockfiles │
  ;; └──────────────────────────────┘

  ;; Set up a directory for storing backup files
  (setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))

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

  ;; ┌─────────┐
  ;; │ Startup │
  ;; └─────────┘
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

  ;; ┌────┐
  ;; │ UI │
  ;; └────┘
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq large-file-warning-threshold 30000000)
  (save-place-mode 1)
  (easy-menu-add-item global-map '(menu-bar edit)
                      ["Emoji & Symbols"
                       ns-do-show-character-palette
                       :help "Show macOS Character Palette."
                       :visible (eq window-system 'ns)])
  (context-menu-mode)
  (savehist-mode)
  ;; MAKE C-s search case-insensitive:
  ;; (setq case-fold-search t)

  ;; ┌────────────┐
  ;; │ Appearance │
  ;; └────────────┘
  (setq-default truncate-lines t)
  (tool-bar-mode 1)
  (scroll-bar-mode -1)
  (blink-cursor-mode)
  (global-hl-line-mode)
  (when (eq 'ns window-system) ;;=> t MacOS
    (setq default-frame-alist '((ns-transparent-titlebar . t))))

  :hook ((prog-mode . display-line-numbers-mode)
         (dired-mode . dired-hide-details-mode))
  ;; ┌─────────────┐
  ;; │ Keybindings │
  ;; └─────────────┘
  :config
  (global-unset-key (kbd "C-x C-d")) ;; default is `list-directory'.
  :bind (("s-f" . rgrep)
         ("M-SPC" . execute-extended-command) ;; Swap M-x and M-SPC
         ("M-x" . cycle-spacing))
  ;; ┌─────────────────────────┐
  ;; │ Text Editing & Movement │
  ;; └─────────────────────────┘
  :config
  (setq-default indent-tabs-mode nil)
  (setq next-line-add-newlines t) ;; c-n adds newlines
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

;; ┌────────────────────┐
;; │ Load Elisp Modules │
;; └────────────────────┘
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) ;; NOTE: Probably redundant, see below.
(when (and (file-exists-p "~/.safe/safe.el")
           (file-exists-p "~/code/my-emacs-packages/rotor/rotor.el"))
  (load "~/.safe/safe.el")
  (with-eval-after-load 'safe
    (load "~/code/my-emacs-packages/rotor/rotor.el")))

(defun require-directory (dir)
  "Load all elisp files in directory DIR."
  (interactive "DDirectory: ")
  (let ((files (directory-files dir t "\\.el$")))
    (dolist (file files)
      (let* ((filename (file-name-nondirectory file))
             (module (file-name-sans-extension filename)))
        ;; Skip backup files and custom.el
        (unless (or (string-prefix-p "." filename)
                    (string= filename "custom.el"))
          (message "Loading %s..." module)
          (require (intern module)))))))

(require-directory (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path "~/Dropbox/private/")
(when (file-exists-p "~/Dropbox/private/private.el")
  (require 'private))
;;; init.el ends here
