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
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  ;; Swap C-a and M-m
  ;; (global-unset-key (kbd "C-a"))
  ;; (global-unset-key (kbd "M-m"))
  ;; (global-set-key (kbd "C-a") 'back-to-indentation)
  ;; (global-set-key (kbd "M-m") 'move-beginning-of-line)
  )

;; MAKE C-s search case-insensitive:
;; (setq case-fold-search t)

(use-package stripspace
  :ensure t
  :commands stripsace-local-mode
  :hook ((prog-mode . stripspace-local-mode)))

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

(use-package tex-mode
  :ensure nil
  :config
  (defun toggle-word-mathrm ()
    "Toggle wrapping the word at point in \\mathrm{...}"
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
        (let* ((start (car bounds))
               (end (cdr bounds))
               (word (buffer-substring-no-properties start end))
               (mathrm-prefix "\\mathrm{")
               (mathrm-suffix "}")
               (prefix-len (length mathrm-prefix))
               (suffix-len (length mathrm-suffix)))
          ;; Check if previous chars are \mathrm{ and next chars are }
          (if (and (>= start prefix-len)
                   (>= (point-max) (+ end suffix-len))
                   (string= (buffer-substring-no-properties
                             (- start prefix-len) start)
                            mathrm-prefix)
                   (string= (buffer-substring-no-properties
                             end (+ end suffix-len))
                            mathrm-suffix))
              ;; Remove \mathrm{...}
              (progn
                (delete-region (- start prefix-len) (+ end suffix-len))
                (insert word))
            ;; Add \mathrm{...}
            (delete-region start end)
            (insert mathrm-prefix word mathrm-suffix))))))
  :bind (:map tex-mode-map
              ("C-c m" . toggle-word-mathrm)))

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
          ;; help-mode
          ;; compilation-mode
          ))
  (setq popper-group-function #'popper-group-by-project)

  (defun my/popper-group-function ()
    ""
    )
  (setq popper-group-function #'my/popper-group-function)

  (defun my/popper-display-popup (buffer &optional alist)
    "Display popup-buffer BUFFER based on the number of windows in the frame."
    (display-buffer-in-side-window
     buffer
     (append alist
             `((side . right)
               (window-width . 0.5)))))
  ;; (defun my/popper-display-popup (buffer &optional alist)
  ;;   "Display popup-buffer BUFFER based on the number of windows in the frame."
  ;;   (let ((window-count (length (window-list))))
  ;;     (if (= window-count 1)
  ;;         ;; If there's only one window, display on the right
  ;;         (display-buffer-in-side-window
  ;;          buffer
  ;;          (append alist
  ;;                  `((side . right)
  ;;                    (window-width . 0.5))))
  ;;       ;; If there are multiple windows, display at the bottom
  ;;       (display-buffer-in-side-window
  ;;        buffer
  ;;        (append alist
  ;;                `((side . bottom)
  ;;                  (window-height . 0.5)))))))

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
  :hook ((prog-mode . outline-indent-minor-mode))
  :bind (:map prog-mode-map
              ("C-<tab>" . outline-cycle)
              ("C-S-<tab>" . outline-cycle-buffer)))

(provide 'editing)
