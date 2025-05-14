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
  (setq prolog-system-version '((swi
                                 (9 . 2))
                                (scryer
                                 (0 . 9))
                                (mmc
                                 (22 . 1))))
  ;; ┌───────────────┐
  ;; │ Scryer Prolog │
  ;; └───────────────┘
  ;; (setq prolog-program-name "scryer-prolog")
  ;; (setq prolog-system 'scryer)
  ;; (add-to-list 'prolog-consult-string '(scryer "consult(%f)."))
  ;; :hook (prolog-mode . (lambda () (setq prolog-system 'scryer)))
  )

(provide '--use-prolog__programming_prolog@@20250505T050425)
;;; --use-prolog__programming_prolog@@20250505T050425.el ends here
