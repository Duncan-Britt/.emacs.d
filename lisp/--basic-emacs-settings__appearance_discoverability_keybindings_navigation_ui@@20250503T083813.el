;;; --basic-emacs-settings__appearance_discoverability_keybindings_navigation_ui@@20250503T083813.el --- basic-emacs-settings -*- lexical-binding: t -*-

;;; Commentary:
;; title: basic-emacs-settings
;; keywords: :appearance:discoverability:keybindings:navigation:ui:
;; date: [2025-05-03 Sat 08:38]
;; identifier: 20250503T083813

;;; Code:

(use-package emacs
  :ensure nil
  :config
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
  ;; ┌────────────┐
  ;; │ Appearance │
  ;; └────────────┘
  (setq-default truncate-lines t)
  (tool-bar-mode -1)    ; Remove toolbar
  (scroll-bar-mode -1)  ; Remove scroll bar
  (blink-cursor-mode)
  (global-hl-line-mode)
  :hook ((prog-mode . display-line-numbers-mode)
         (dired-mode . dired-hide-details-mode))
  ;; ┌─────────────┐
  ;; │ Keybindings │
  ;; └─────────────┘
  :config
  (global-unset-key (kbd "C-x C-d")) ;; default is `list-directory'.
  :bind (("s-f" . rgrep)))

(provide '--basic-emacs-settings__appearance_discoverability_keybindings_navigation_ui@@20250503T083813)

;;; --basic-emacs-settings__appearance_discoverability_keybindings_navigation_ui@@20250503T083813.el ends here
