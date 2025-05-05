;;; --use-stripspace__text@@20250505T104217.el --- use-stripspace -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-stripspace
;; keywords: :text:
;; date: [2025-05-05 Mon 10:42]
;; identifier: 20250505T104217

;;; Code:
(use-package stripspace
  :ensure t
  :commands stripsace-local-mode
  :hook ((prog-mode . stripspace-local-mode)))

(provide '--use-stripspace__text@@20250505T104217)
;;; --use-stripspace__text@@20250505T104217.el ends here
