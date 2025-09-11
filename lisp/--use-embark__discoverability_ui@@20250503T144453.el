;;; --use-embark__discoverability_ui@@20250503T144453.el --- use-embark -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-embark
;; keywords: :discoverability:ui:
;; date: [2025-05-03 Sat 14:44]
;; identifier: 20250503T144453

;;; Code:
(use-package embark
  :ensure t
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-<return>" . embark-act)
   ("C-h B" . embark-bindings))
  :hook (org-mode . (lambda () (local-set-key (kbd "C-<return>") #'embark-act))))

(provide '--use-embark__discoverability_ui@@20250503T144453)
;;; --use-embark__discoverability_ui@@20250503T144453.el ends here
