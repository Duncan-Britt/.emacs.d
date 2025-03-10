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

;; ┌──────────────────────────────┐
;; │ Autosave, Backups, Lockfiles │
;; └──────────────────────────────┘

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

;; ┌─────────┐
;; │ Startup │
;; └─────────┘

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
              (elpaca-log nil t))))

;; ┌───────────┐
;; │ Load Path │
;; └───────────┘

(add-to-list 'load-path "~/.emacs.d/lisp/") ;; NOTE: Use (require 'package) to use the code in the lisp directory
(when (and (file-exists-p "~/.safe/safe.el")
           (file-exists-p "~/code/my-emacs-packages/rotor/rotor.el"))
  (load "~/.safe/safe.el")
  (with-eval-after-load 'safe
    (load "~/code/my-emacs-packages/rotor/rotor.el")))

(require 'portable)
(require 'appearance)
(require 'discoverable)
(require 'editing)
(require 'thinking)
(require 'programming)

;; ┌───────────────┐
;; │ Miscellaneous │
;; └───────────────┘

(use-package emacs
  :ensure nil
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq large-file-warning-threshold 30000000)
  (save-place-mode 1))

(use-package transient
  :ensure t)

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

(use-package calendar
  :ensure nil
  :config
  (defun calendar-insert-date ()
    "Capture the date at point, exit the Calendar, insert the date."
    (interactive)
    (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
      (calendar-exit)
      (insert (format "<%d-%02d-%02d>" year month day))))
  (define-key calendar-mode-map (kbd "RET") 'calendar-insert-date))

(use-package-when-local
 directory-slideshow
 "~/code/my-emacs-packages/directory-slideshow/")

(use-package djvu
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
   '((allout-layout . t)
     (eval progn
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
