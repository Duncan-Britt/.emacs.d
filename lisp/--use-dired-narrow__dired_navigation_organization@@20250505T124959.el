;;; --use-dired-narrow__dired_navigation_organization@@20250505T124959.el --- use-dired-narrow -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-dired-narrow
;; keywords: :dired:navigation:organization:
;; date: [2025-05-05 Mon 12:49]
;; identifier: 20250505T124959

;;; Code:
(use-package dired-narrow
  :ensure t
  :config
  (defun my/dired-narrow-exclude ()
    "Narrow a dired buffer to exclude files matching a string.
If the string contains spaces, each word must match for the file to be excluded."
    (interactive)
    (dired-narrow--internal
     (lambda (filter)
       (not (dired-narrow--string-filter filter)))))

  (defun my/dired-narrow (&optional exclude-p)
    "Narrow dired buffer to files matching string.
With prefix argument C-u, exclude files matching string instead."
    (interactive "P")
    (if exclude-p
        (my/dired-narrow-exclude)
      (dired-narrow)))

  :bind ((:map dired-mode-map
               ("C-s" . my/dired-narrow))))

(provide '--use-dired-narrow__dired_navigation_organization@@20250505T124959)
;;; --use-dired-narrow__dired_navigation_organization@@20250505T124959.el ends here
