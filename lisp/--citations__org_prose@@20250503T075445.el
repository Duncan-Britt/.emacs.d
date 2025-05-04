;;; --citations__org_prose@@20250503T075445.el --- citations -*- lexical-binding: t -*-

;;; Commentary:
;; title: citations
;; keywords: :org:prose:
;; date: [2025-05-03 Sat 07:54]
;; identifier: 20250503T075445

;;; Code:
(use-package org
  :ensure nil
  :config
  ;; for Bibtex Bibliography styles on latex export
  (setq org-cite-csl-styles-dir "~/code/citation-styles/"))

(use-package citeproc
  :ensure t
  :after org
  :config
  ;; Optional basic configuration
  ;; (setq org-cite-export-processors '((t citeproc)))
  )

(provide '--citations__org_prose@@20250503T075445)
;;; --citations__org_prose@@20250503T075445.el ends here
