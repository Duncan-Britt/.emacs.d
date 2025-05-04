;;; --use-dired-subtree__dired_navigation@@20250503T085331.el --- use-dired-subtree -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-dired-subtree
;; keywords: :dired:navigation:
;; date: [2025-05-03 Sat 08:53]
;; identifier: 20250503T085331

;;; Code:
(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("C-<tab>" . dired-subtree-cycle)
        ("<backtab>" . dired-subtree-remove)))

(provide '--use-dired-subtree__dired_navigation@@20250503T085331)
;;; --use-dired-subtree__dired_navigation@@20250503T085331.el ends here
