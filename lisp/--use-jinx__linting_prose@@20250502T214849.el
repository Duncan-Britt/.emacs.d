;;; --use-jinx__linting_prose@@20250502T214849.el --- use-jinx -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-jinx
;; keywords: :linting:prose:
;; date: [2025-05-02 Fri 21:48]
;; identifier: 20250502T214849

;; ┌───────────────┐
;; │ Spell checker │
;; └───────────────┘
;;; Code:
(use-package jinx ;; NOTE Custom variable `jinx-include-faces' can be used to add spell checking to comments and string in programming modes. Also see `jinx-exclude-faces'.
  :ensure t
  :hook
  ((org-mode . jinx-mode)
   (text-mode . jinx-mode))
  :bind (("M-$" . jinx-correct))) ;; M-x jinx-languages for other languages.

(provide '--use-jinx__linting_prose@@20250502T214849)
;;; --use-jinx__linting_prose@@20250502T214849.el ends here
