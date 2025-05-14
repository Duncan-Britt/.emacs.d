;;; --use-hyperbole__buttons_hypertext@@20250506T073343.el --- use-hyperbole -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-hyperbole
;; keywords: :buttons:hypertext:
;; date: [2025-05-06 Tue 07:33]
;; identifier: 20250506T073343

;;; Code:

(use-package hyperbole
  :ensure (:host github :repo "emacs-straight/hyperbole")
  :after org
  :custom
  (hkey-init nil) ;; I don't want Hyperbole's keybindings.
  :config
  (hyperbole-mode 1)
  :bind (("M-RET" . hkey-either)
         (:map org-mode-map
               ("M-RET" . hkey-either))))

(provide '--use-hyperbole__buttons_hypertext@@20250506T073343)
;;; --use-hyperbole__buttons_hypertext@@20250506T073343.el ends here
