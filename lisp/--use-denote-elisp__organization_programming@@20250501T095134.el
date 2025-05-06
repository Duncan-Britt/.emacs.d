;;; --use-denote-elisp_organization_programming@@20250501T095134.el --- use-denote-elisp -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-denote-elisp
;; keywords: :organization:programming:
;; date: [2025-05-01 Thu 09:51]
;; identifier: 20250501T095134

;;; Code:
(require '--portable__utility@@20250505T200406)

(use-package-local-or-remote
 denote-elisp
 "~/code/my-emacs-packages/denote-elisp/"
 "Duncan-Britt/denote-elisp"
 :custom
 ((denote-elisp-directory
   (expand-file-name "lisp" user-emacs-directory)))) ;;=> "/Users/duncan/.emacs.d/lisp"

(provide '--use-denote-elisp__organization_programming@@20250501T095134)
;;; --use-denote-elisp__organization_programming@@20250501T095134.el ends here
