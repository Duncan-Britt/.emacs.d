;;; --use-mw-thesaurus__prose@@20250502T215140.el --- use-mw-thesaurus -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-mw-thesaurus
;; keywords: :prose:
;; date: [2025-05-02 Fri 21:51]
;; identifier: 20250502T215140

;; ┌───────────┐
;; │ Thesaurus │
;; └───────────┘
;;; Code:
(use-package mw-thesaurus
  :ensure t
  :after org
  :bind
  (:map org-mode-map
        ("C-c t" . mw-thesaurus-lookup-dwim)))

(provide '--use-mw-thesaurus__prose@@20250502T215140)
;;; --use-mw-thesaurus__prose@@20250502T215140.el ends here
