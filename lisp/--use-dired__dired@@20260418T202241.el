;;; --use-dired__dired@@20260418T202241.el --- use-dired -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-dired
;; keywords: :dired:
;; date: [2026-04-18 Sat 20:22]
;; identifier: 20260418T202241

;;; Code:
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh") ;; In dired, C-u s -alhS <- Sort by file size
  )


(provide '--use-dired__dired@@20260418T202241)
;;; --use-dired__dired@@20260418T202241.el ends here
