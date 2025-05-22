;;; --use-doric-themes__appearance_color@@20250522T003446.el --- use-doric-themes -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-doric-themes
;; keywords: :appearance:color:
;; date: [2025-05-22 Thu 00:34]
;; identifier: 20250522T003446

;;; Code:
(use-package doric-themes
  :ensure (:host github :repo "protesilaos/doric-themes")
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)

  ;; (doric-themes-select 'doric-light)

  ;; ;; To load a random theme instead, use something like one of these:
  ;;
  ;; (doric-themes-load-random)
  ;; (doric-themes-load-random 'light)
  ;; (doric-themes-load-random 'dark)
  )

(provide '--use-doric-themes__appearance_color@@20250522T003446)
;;; --use-doric-themes__appearance_color@@20250522T003446.el ends here
