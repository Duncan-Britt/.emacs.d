;;; --use-org-cmenu__discoverability_org@@20250502T213349.el --- use-org-cmenu -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-org-cmenu
;; keywords: :discoverability:org:
;; date: [2025-05-02 Fri 21:33]
;; identifier: 20250502T213349

;;; Code:
(use-package org-cmenu
  :ensure (:host github :repo "misohena/org-cmenu")
  :after org
  :config (require 'org-cmenu-setup)
  :bind
  (:map org-mode-map
        ("M-n" . org-cmenu)))

(provide '--use-org-cmenu__discoverability_org@@20250502T213349)
;;; --use-org-cmenu__discoverability_org@@20250502T213349.el ends here
