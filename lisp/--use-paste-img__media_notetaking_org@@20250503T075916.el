;;; --use-paste-img__media_notetaking_org@@20250503T075916.el --- use-paste-img -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-paste-img
;; keywords: :media:notetaking:org:
;; date: [2025-05-03 Sat 07:59]
;; identifier: 20250503T075916

;;; Code:
(require 'portable)

(use-package-local-or-remote
 paste-img
 "~/code/my-emacs-packages/paste-img/"
 "Duncan-Britt/paste-img"
 :after (org org-download)
 :hook
 (org-mode . paste-img-mode))

(provide '--use-paste-img__media_notetaking_org@@20250503T075916)
;;; --use-paste-img__media_notetaking_org@@20250503T075916.el ends here
