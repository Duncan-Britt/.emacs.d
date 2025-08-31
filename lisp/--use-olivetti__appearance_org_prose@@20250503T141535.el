;;; --use-olivetti__appearance_org_prose@@20250503T141535.el --- use-olivetti -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-olivetti
;; keywords: :appearance:org:prose:
;; date: [2025-05-03 Sat 14:15]
;; identifier: 20250503T141535

;;; Code:
(use-package olivetti
  :ensure (:host github :repo "rnkn/olivetti")
  :custom (olivetti-body-width 160)
  :hook (org-mode . olivetti-mode))

(provide '--use-olivetti__appearance_org_prose@@20250503T141535)
;;; --use-olivetti__appearance_org_prose@@20250503T141535.el ends here
