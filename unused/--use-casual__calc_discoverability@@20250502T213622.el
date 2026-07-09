;;; --use-casual__calc_discoverability@@20250502T213622.el --- use-casual -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-casual
;; keywords: :calc:discoverability:
;; date: [2025-05-02 Fri 21:36]
;; identifier: 20250502T213622

;;; Code:
(use-package casual
  :after (calc transient)
  :ensure t
  :bind
  (:map calc-mode-map
        ("C-o" . casual-calc-tmenu)))

(provide '--use-casual__calc_discoverability@@20250502T213622)
;;; --use-casual__calc_discoverability@@20250502T213622.el ends here
