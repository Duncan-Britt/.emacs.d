;;; --use-breadcrumb__appearance_navigation@@20250503T140349.el --- use-breadcrumb -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-breadcrumb
;; keywords: :appearance:navigation:
;; date: [2025-05-03 Sat 14:03]
;; identifier: 20250503T140349

;;; Code:
(use-package breadcrumb
  :ensure t
  :config
  (defun dunc/breadcrumb-add-wrapper (orig-fun &rest args)
    "Add prefix and suffix to breadcrumb output."
    (let ((result (apply orig-fun args)))
      (if (not (string-empty-p result))
          (concat "⦗" result "⦘")
        result)))

  (advice-add 'breadcrumb-imenu-crumbs :around #'dunc/breadcrumb-add-wrapper)

  (breadcrumb-mode))

(provide '--use-breadcrumb__appearance_navigation@@20250503T140349)
;;; --use-breadcrumb__appearance_navigation@@20250503T140349.el ends here
