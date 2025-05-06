;;; --use-denote-dired-weblinks__denote_dired_navigation_organization_ui_web@@20250503T074513.el --- use-denote-dired-bookmarks -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-denote-dired-weblinks
;; keywords: :denote:dired:navigation:organization:ui:web:
;; date: [2025-05-03 Sat 07:45]
;; identifier: 20250503T074513

;;; Code:
(require '--portable__utility@@20250505T200406)

(use-package-local-or-remote
 denote-dired-weblinks
 "~/code/my-emacs-packages/denote-dired-weblinks/"
 "Duncan-Britt/denote-dired-weblinks"
 :hook (dired-mode . denote-dired-weblinks-mode)
 :bind (("s-b" . denote-dired-weblinks-create-bookmark)))

(provide '--use-denote-dired-weblinks__denote_dired_navigation_organization_ui_web@@20250503T074513)
;;; --use-denote-dired-weblinks__denote_dired_navigation_organization_ui_web@@20250503T074513.el ends here
