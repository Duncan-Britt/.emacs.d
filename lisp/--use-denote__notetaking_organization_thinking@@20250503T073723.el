;;; --use-denote__notetaking_organization_thinking@@20250503T073723.el --- use-denote -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-denote
;; keywords: :notetaking:organization:thinking:
;; date: [2025-05-03 Sat 07:37]
;; identifier: 20250503T073723

;;; Code:
(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/notes/"))
  (setq denote-file-type 'org)

  (defun my/denote-rename-file ()
    "Call denote-rename-file with local directory keywords."
    (interactive)
    (let ((denote-directory default-directory))
      (call-interactively 'denote-rename-file)))

  (defun my/denote-here ()
    "Call denote with local directory."
    (interactive)
    (let ((denote-directory (read-directory-name "Enter file path: "
                                                 (or (vc-git-root default-directory)
                                                     default-directory)
                                                 nil
                                                 t))
          (denote-prompts '(title keywords file-type)))
      (call-interactively 'denote)))
  :hook (dired-mode . denote-dired-mode))

(provide '--use-denote__notetaking_organization_thinking@@20250503T073723)
;;; --use-denote__notetaking_organization_thinking@@20250503T073723.el ends here
