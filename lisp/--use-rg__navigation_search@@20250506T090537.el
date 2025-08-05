;;; --use-rg__navigation_search@@20250506T090537.el --- use-rg -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-rg
;; keywords: :navigation:search:
;; date: [2025-05-06 Tue 09:05]
;; identifier: 20250506T090537

;; Replaces rgrep
;;; Code:
(use-package rg
  :ensure t
  :bind (("s-f" . rg-dwim)
         ("s-p s" . rg-project)))

(provide '--use-rg__navigation_search@@20250506T090537)
;;; --use-rg__navigation_search@@20250506T090537.el ends here
