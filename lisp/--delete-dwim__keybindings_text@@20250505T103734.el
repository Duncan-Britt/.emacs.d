;;; --delete-dwim__keybindings_text@@20250505T103734.el --- delete-dwim -*- lexical-binding: t -*-

;;; Commentary:
;; title: delete-dwim
;; keywords: :keybindings:text:
;; date: [2025-05-05 Mon 10:37]
;; identifier: 20250505T103734

;;; Code:
(use-package emacs
  :ensure nil
  :config
  (defun my/delete-dwim ()
    "Custom delete function to do what I mean based on context."
    (interactive)
    (cond ((region-active-p) (call-interactively 'delete-region))
          ((derived-mode-p 'org-mode) (call-interactively 'org-delete-char))
          (t (call-interactively 'delete-char))))
  :bind (("C-d" . my/delete-dwim)))

(provide '--delete-dwim__keybindings_text@@20250505T103734)
;;; --delete-dwim__keybindings_text@@20250505T103734.el ends here
