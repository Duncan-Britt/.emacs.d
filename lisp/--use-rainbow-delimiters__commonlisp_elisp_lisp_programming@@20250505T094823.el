;;; --use-rainbow-delimiters__commonlisp_elisp_lisp_programming@@20250505T094823.el --- use-rainbow-delimiters -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-rainbow-delimiters
;; keywords: :commonlisp:elisp:lisp:programming:
;; date: [2025-05-05 Mon 09:48]
;; identifier: 20250505T094823

;;; Code:
(use-package rainbow-delimiters
  :ensure t
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (lisp-mode . rainbow-delimiters-mode)))

(provide '--use-rainbow-delimiters__commonlisp_elisp_lisp_programming@@20250505T094823)
;;; --use-rainbow-delimiters__commonlisp_elisp_lisp_programming@@20250505T094823.el ends here
