;;; --use-eat__shell_terminal@@20251214T223915.el --- use-eat -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-eat
;; keywords: :shell:terminal:
;; date: [2025-12-14 Sun 22:39]
;; identifier: 20251214T223915

;;; Code:
(use-package eat
  ;; [[file:~/notes/.images/eat-modes.png]]
  ;; NOTE Run M-x eat-compile-terminfo on first install--see
  ;; https://codeberg.org/akib/emacs-eat/issues/45
  :after eshell
  :ensure t
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

(provide '--use-eat__shell_terminal@@20251214T223915)
;;; --use-eat__shell_terminal@@20251214T223915.el ends here
