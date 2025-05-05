;;; --use-exec-path-from-shell__env_python_shell@@20250505T045940.el --- use-exec-path-from-shell -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-exec-path-from-shell
;; keywords: :env:python:shell:
;; date: [2025-05-05 Mon 04:59]
;; identifier: 20250505T045940

;; Without this, eshell would use the homebrew version of python.
;;; Code:
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(provide '--use-exec-path-from-shell__env_python_shell@@20250505T045940)
;;; --use-exec-path-from-shell__env_python_shell@@20250505T045940.el ends here
