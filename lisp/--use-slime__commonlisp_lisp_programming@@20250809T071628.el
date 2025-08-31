;;; --use-slime__commonlisp_lisp_programming@@20250809T071628.el --- use-slime -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-slime
;; keywords: :commonlisp:lisp:programming:
;; date: [2025-08-09 Sat 07:16]
;; identifier: 20250809T071628

;;; Code:
(use-package slime
  :ensure t
  :config
  ;; (setq slime-lisp-implementations
  ;;       `((sbcl (,(executable-find "sbcl") "--dynamic-space-size" "4000"))))
  (setq slime-lisp-implementations
        '((sbcl ("sbcl") :coding-system utf-8-unix)
          (qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix))))



(provide '--use-slime__commonlisp_lisp_programming@@20250809T071628)
;;; --use-slime__commonlisp_lisp_programming@@20250809T071628.el ends here
