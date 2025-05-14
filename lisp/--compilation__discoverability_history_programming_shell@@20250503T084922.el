;;; --compilation__discoverability_history_programming_shell@@20250503T084922.el --- compilation -*- lexical-binding: t -*-

;;; Commentary:
;; title: compilation
;; keywords: :discoverability:history:programming:shell:
;; date: [2025-05-03 Sat 08:49]
;; identifier: 20250503T084922

;; ┌───────────────────────────────────────────────────────────────┐
;; │ Minibuffer completions for compile commands based on history. │
;; └───────────────────────────────────────────────────────────────┘
;;; Code:
;; (use-package emacs
;;   :ensure nil
;;   :config
;;   (require 'savehist)

;;   (add-to-list 'savehist-additional-variables 'compile-history)

;;   (defun my/compile (cmd)
;;     "Compile with completing read based on `compile-history'."
;;     (interactive
;;      (list (completing-read "Command: " compile-history)))
;;     (compile cmd)
;;     (push cmd compile-history))

;;   (defun my/project-compile ()
;;     "Run `compile' in the project root."
;;     (declare (interactive-only compile))
;;     (interactive)
;;     (let ((default-directory (project-root (project-current t)))
;;           (compilation-buffer-name-function
;;            (or project-compilation-buffer-name-function
;;                compilation-buffer-name-function)))
;;       (call-interactively #'my/compile)))

;;   (define-key project-prefix-map "c" #'my/project-compile)
;;   (global-set-key (kbd "s-c") #'my/compile))

(provide '--compilation__discoverability_history_programming_shell@@20250503T084922)
;;; --compilation__discoverability_history_programming_shell@@20250503T084922.el ends here
