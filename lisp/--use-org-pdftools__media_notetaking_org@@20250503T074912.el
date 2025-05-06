;;; --use-org-pdftools__media_notetaking_org@@20250503T074912.el --- use-org-pdftools -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-org-pdftools
;; keywords: :media:notetaking:org:
;; date: [2025-05-03 Sat 07:49]
;; identifier: 20250503T074912

;;; Code:
(require '--portable__utility@@20250505T200406)

(use-package-local-or-remote
 org-pdftools
 "~/code/my-emacs-packages/org-pdftools/"
 "Duncan-Britt/org-pdftools"
 :hook (org-mode . org-pdftools-setup-link))

(provide '--use-org-pdftools__media_notetaking_org@@20250503T074912)
;;; --use-org-pdftools__media_notetaking_org@@20250503T074912.el ends here
