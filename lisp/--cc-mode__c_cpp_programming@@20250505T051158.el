;;; --cc-mode__c_cpp_programming@@20250505T051158.el --- cc-mode -*- lexical-binding: t -*-

;;; Commentary:
;; title: cc-mode
;; keywords: :c:cpp:programming:
;; date: [2025-05-05 Mon 05:11]
;; identifier: 20250505T051158

;; ┌───────┐
;; │ C/C++ │
;; └───────┘
;;; Code:
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-map
              ("C-c c" . recompile)
              :map c++-mode-map
              ("C-c c" . recompile))
  :config
  (setq-default c-basic-offset 4)
  (add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "stroustrup")
            (setq c-basic-offset 4)))
  (with-eval-after-load 'c-ts-mode
    (setq c-ts-mode-indent-offset 4)
    (setq c-ts-mode-indent-style 'bsd)
    (define-key c-ts-mode-map (kbd "C-c c") #'recompile))

  (with-eval-after-load 'c++-ts-mode
    (define-key c++-ts-mode-map (kbd "C-c c") #'recompile)))

(provide '--cc-mode__c_cpp_programming@@20250505T051158)
;;; --cc-mode__c_cpp_programming@@20250505T051158.el ends here
