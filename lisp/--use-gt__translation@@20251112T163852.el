;;; --use-gt__translation@@20251112T163852.el --- use-gt -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-gt
;; keywords: :translation:
;; date: [2025-11-12 Wed 16:38]
;; identifier: 20251112T163852

;;; Code:
(use-package pdd
  :ensure (:host github :repo "lorniu/pdd.el"))

(use-package gt
  :after pdd
  :ensure (:host github :repo "lorniu/gt.el")
  :config
  (setq gt-langs '(en vi))
  (setq gt-default-translator (gt-translator :engines (gt-google-engine)
                                             :render (gt-buffer-render)))
  ;; This configuration means:
  ;; Initialize the default translator, let it translate between en
  ;; and fr via Google Translate, and the result will be displayed in
  ;; a new buffer.
  )

(provide '--use-gt__translation@@20251112T163852)
;;; --use-gt__translation@@20251112T163852.el ends here
