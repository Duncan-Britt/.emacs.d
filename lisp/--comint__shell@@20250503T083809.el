;;; --comint__shell@@20250503T083809.el --- comint -*- lexical-binding: t -*-

;;; Commentary:
;; title: comint
;; keywords: :shell:
;; date: [2025-05-03 Sat 08:38]
;; identifier: 20250503T083809

;;; Code:
(use-package comint
  :ensure nil
  :bind (("s-r" . (lambda ()
                    (interactive)
                    (let ((current-prefix-arg '(4)))
                      (call-interactively 'comint-run))))))

(provide '--comint__shell@@20250503T083809)
;;; --comint__shell@@20250503T083809.el ends here
