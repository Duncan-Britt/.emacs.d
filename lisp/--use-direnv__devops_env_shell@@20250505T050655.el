;;; --use-direnv__devops_env_shell@@20250505T050655.el --- use-direnv -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-direnv
;; keywords: :devops:env:shell:
;; date: [2025-05-05 Mon 05:06]
;; identifier: 20250505T050655

;; Global minor mode to automatically update the environment using direnv
;;; Code:
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(provide '--use-direnv__devops_env_shell@@20250505T050655)
;;; --use-direnv__devops_env_shell@@20250505T050655.el ends here
