;;; editing.el --- Text editing, movement, window management. -*- lexical-binding: t; -*-

;;; Commentary:
;; ┌───────────────────────────────────────────┐
;; │ Text Editing, Movement, Window Management │
;; └───────────────────────────────────────────┘
;;; Code:
;; ┌─────────────────────────┐
;; │ Text Editing & Movement │
;; └─────────────────────────┘
(use-package emacs
  :ensure nil
  :config ;; c-n adds newlines
  (setq next-line-add-newlines t)
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
        (message "Replaced %d occurrences of '%s'." count from)))))

;; MAKE C-s search case-insensitive:
;; (setq case-fold-search t)

(use-package undo-fu
  :ensure t
  :custom
  (undo-fu-ignore-keyboard-quit t)
  (undo-fu-allow-undo-in-region t)
  :bind
  (("s-z" . undo-fu-only-undo)
   ("s-Z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :init
  (undo-fu-session-global-mode))

(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-ascii-symbols)
  :bind (("C-x u" . vundo)))

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

;; ┌───────────────────┐
;; │ Window Management │
;; └───────────────────┘

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

(use-package outline-indent
  :ensure t
  :custom
  (outline-indent-ellipsis " ▼ ")
  :hook ((python-mode . outline-indent-minor-mode))
  :bind (:map python-mode-map
              ("C-<tab>" . outline-cycle)
              ("C-S-<tab>" . outline-cycle-buffer)))

(provide 'editing)
