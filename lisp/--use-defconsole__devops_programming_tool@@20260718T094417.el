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
  (defun k-build () (interactive) "Compile k." (compile "echo compiling k ... && echo DONE"))
  (defun k-test () (interactive) "Test k." (compile "echo testing k ... && echo PASS"))
  (defun k-deploy () (interactive) "Test k." (compile "echo deploying k ... && echo DEPLOYED"))
  (defun k-db-migrate () (interactive) "Migrating k db." (compile "echo migrating k ... && echo MIGRATED"))

  (defconsole k-console (k-test k-build k-deploy k-db-migrate)))

(provide '--use-defconsole__devops_programming_tool@@20260718T094417)
;;; --use-defconsole__devops_programming_tool@@20260718T094417.el ends here
