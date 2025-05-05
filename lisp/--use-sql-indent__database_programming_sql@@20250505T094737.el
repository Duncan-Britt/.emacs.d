;;; --use-sql-indent__database_programming_sql@@20250505T094737.el --- use-sql-indent -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-sql-indent
;; keywords: :database:programming:sql:
;; date: [2025-05-05 Mon 09:47]
;; identifier: 20250505T094737

;;; Code:
(use-package sql-indent
  :ensure t
  :hook ((sql-mode . sqlind-minor-mode)
         (cql-mode . sqlind-minor-mode)))

(provide '--use-sql-indent__database_programming_sql@@20250505T094737)
;;; --use-sql-indent__database_programming_sql@@20250505T094737.el ends here
