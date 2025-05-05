;;; --use-prolog__programming_prolog@@20250505T050425.el --- use-prolog -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-prolog
;; keywords: :programming:prolog:
;; date: [2025-05-05 Mon 05:04]
;; identifier: 20250505T050425

;;; Code:
(use-package prolog
  :ensure nil  ; prolog is built-in, so we don't need to ensure it
  :mode ("\\.pl\\'" . prolog-mode)
  :config
  (setq prolog-electric-if-then-else-flag t)
  (setq prolog-program-name "scryer-prolog"))

(provide '--use-prolog__programming_prolog@@20250505T050425)
;;; --use-prolog__programming_prolog@@20250505T050425.el ends here
