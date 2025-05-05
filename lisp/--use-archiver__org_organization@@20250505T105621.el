;;; --use-archiver__org_organization@@20250505T105621.el --- use-archiver -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-archiver
;; keywords: :org:organization:
;; date: [2025-05-05 Mon 10:56]
;; identifier: 20250505T105621

;;; Code:
(require 'portable)

(use-package-local-or-remote
 archiver
 "~/code/my-emacs-packages/archiver/"
 "Duncan-Britt/emacs-archiver"
 :after org
 :init
 (setq *archiver-agenda-archive-location*
       (expand-file-name "~/Dropbox/agenda/agenda_archive.org"))
 :bind
 (:map org-mode-map
       ("C-c C-x C-a" . archiver-archive-heading)))

(provide '--use-archiver__org_organization@@20250505T105621)
;;; --use-archiver__org_organization@@20250505T105621.el ends here
