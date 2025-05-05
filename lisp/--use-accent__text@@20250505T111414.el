;;; --use-accent__text@@20250505T111414.el --- use-accent -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-accent
;; keywords: :text:
;; date: [2025-05-05 Mon 11:14]
;; identifier: 20250505T111414

;; M-x accent-corfu
;; Recommended binding: C-x C-a
;; Shows popup with accented characters based on the letter at point.
;;; Code:
(use-package accent
  :ensure t)

;; Append a list of custom accents to the default collection.
;; (setq accent-custom '((a (ă))
;;                       (o (ŏ))
;;                       (u (ŭ))))

(provide '--use-accent__text@@20250505T111414)
;;; --use-accent__text@@20250505T111414.el ends here
