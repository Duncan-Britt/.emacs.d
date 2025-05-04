;;; --use-denote-dired-bookmarks__denote_dired_navigation_organization_ui_web@@20250503T074513.el --- use-denote-dired-bookmarks -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-denote-dired-bookmarks
;; keywords: :denote:dired:navigation:organization:ui:web:
;; date: [2025-05-03 Sat 07:45]
;; identifier: 20250503T074513

;;; Code:
(require 'portable)

(use-package-local-or-remote
 denote-dired-bookmarks
 "~/code/my-emacs-packages/denote-dired-bookmarks/"
 "Duncan-Britt/denote-dired-bookmarks"
 :hook (dired-mode . denote-dired-bookmarks-mode)
 :bind (("s-b" . denote-dired-bookmarks-create-bookmark)))

(provide '--use-denote-dired-bookmarks__denote_dired_navigation_organization_ui_web@@20250503T074513)
;;; --use-denote-dired-bookmarks__denote_dired_navigation_organization_ui_web@@20250503T074513.el ends here
