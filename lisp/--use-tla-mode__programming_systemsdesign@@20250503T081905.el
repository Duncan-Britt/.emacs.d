;;; --use-tla-mode__programming_systemsdesign@@20250503T081905.el --- use-tla+-mode -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-tla-mode
;; keywords: :programming:systemsdesign:
;; date: [2025-05-03 Sat 08:19]
;; identifier: 20250503T081905

;; ┌──────┐
;; │ TLA+ │
;; └──────┘
;; See lisp/tla+-mode.el
;;; Code:
(require 'tla+-mode)

(use-package tla+-mode
  :ensure nil
  :mode ("\\.tla\\'" . tla+-mode)
  :hook (tla+-mode . (lambda ()
                       (tla+/load-symbols)
                       (prettify-symbols-mode 1))))

(provide '--use-tla-mode__programming_systemsdesign@@20250503T081905)
;;; --use-tla-mode__programming_systemsdesign@@20250503T081905.el ends here
