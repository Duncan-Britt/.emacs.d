;;; discoverable.el --- Anything to help find what you're looking for. -*- lexical-binding: t; -*-

;;; Commentary:
;; ┌─────────────────┐
;; │ Discoverability │
;; └─────────────────┘
;;; Code:
(use-package emacs
  :ensure nil
  :config
  (context-menu-mode)
  (savehist-mode))

(use-package projectile
  ;; TODO upgrade to Emacs 30 and remove this in favor of `project.el'
  :ensure t
  :init
  (when (eq system-type 'darwin)
    (setq projectile-use-git-grep t)) ; macOS-specific setting
  :config
  (projectile-mode +1)
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

(use-package casual
  :after (calc transient)
  :ensure t
  :bind
  (:map calc-mode-map
        ("C-o" . casual-calc-tmenu)))

(use-package org-cmenu
  :ensure (:host github :repo "misohena/org-cmenu")
  :after org
  :config (require 'org-cmenu-setup)
  :bind
  (:map org-mode-map
        ("M-n" . org-cmenu)))

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
               '(erlang-mode . ("elp" "server"))))

(provide 'discoverable)
