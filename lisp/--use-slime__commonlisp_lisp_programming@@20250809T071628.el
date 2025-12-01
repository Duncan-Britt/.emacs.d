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
          (qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix)))

  (defun my/slime-eval-print-last-expression (string)
    "Evaluate sexp before point; print value into the current buffer,
then comment region. TODO FIXME"
    (interactive (list (slime-last-expression)))
    (save-excursion
      (let ((start (point)))
        (slime-eval-async `(swank:eval-and-grab-output ,string)
          (lambda (result)
            (insert "\n")
            (cl-destructuring-bind (output value) result
              (push-mark)
              (insert output value)
              (comment-region start (point))))))))

  :bind ((:map lisp-mode-map
               ("C-j" . slime-eval-print-last-expression))))

(provide '--use-slime__commonlisp_lisp_programming@@20250809T071628)
;;; --use-slime__commonlisp_lisp_programming@@20250809T071628.el ends here
