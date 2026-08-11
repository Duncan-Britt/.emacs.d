;;; --use-defconsole__devops_programming_tool@@20260718T094417.el --- use-defconsole -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-defconsole
;; keywords: :devops:programming:tool:
;; date: [2026-07-18 Sat 09:44]
;; identifier: 20260718T094417

;;; Code:
(use-package defconsole
  :ensure (:host github :repo "Duncan-Britt/defconsole" :branch "dev")
  :config
  (defun k-build () "Compile k." (interactive) (compile "echo compiling k ... && echo DONE"))
  (defun k-test () "Test k." (interactive) (compile "echo testing k ... && echo PASS"))
  (defun k-deploy () "Test k." (interactive) (compile "echo deploying k ... && echo DEPLOYED"))
  (defun k-db-migrate () "Migrating k db." (interactive) (compile "echo migrating k ... && echo MIGRATED"))

  (defconsole k-console (k-test k-build k-deploy k-db-migrate)))

(provide '--use-defconsole__devops_programming_tool@@20260718T094417)
;;; --use-defconsole__devops_programming_tool@@20260718T094417.el ends here
