;;; --emacs-lisp__elisp_lisp_programming@@20250505T051916.el --- emacs-lisp -*- lexical-binding: t -*-

;;; Commentary:
;; title: emacs-lisp
;; keywords: :elisp:lisp:programming:
;; date: [2025-05-05 Mon 05:19]
;; identifier: 20250505T051916

;; ┌────────────┐
;; │ Emacs Lisp │
;; └────────────┘
;;; Code:
(use-package emacs
  :ensure nil
  :bind
  (("M-:" . pp-eval-expression)
   :map emacs-lisp-mode-map
   ("C-c C-p" . pp-eval-last-sexp))
  :hook
  ((emacs-lisp-mode . prettify-symbols-mode)))

(provide '--emacs-lisp__elisp_lisp_programming@@20250505T051916)
;;; --emacs-lisp__elisp_lisp_programming@@20250505T051916.el ends here
