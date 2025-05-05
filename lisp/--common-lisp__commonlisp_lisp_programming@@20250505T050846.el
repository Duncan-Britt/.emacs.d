;;; --common-lisp__commonlisp_lisp_programming@@20250505T050846.el --- common-lisp -*- lexical-binding: t -*-

;;; Commentary:
;; title: common-lisp
;; keywords: :commonlisp:lisp:programming:
;; date: [2025-05-05 Mon 05:08]
;; identifier: 20250505T050846

;;; Code:
(use-package lisp-mode
  :ensure nil  ; lisp-mode is built-in, so we don't need to ensure it
  :hook ((lisp-mode . prettify-symbols-mode))
  :config
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq slime-lisp-implementations
        `((sbcl (,(executable-find "sbcl") "--dynamic-space-size" "4000"))))
  ;; For hacking on Nyxt:
  ;; (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
  ;; (setq slime-lisp-implementations '((nyxt ("/opt/homebrew/bin/sbcl" "--dynamic-space-size 3072")
  ;;                                          :env ("CL_SOURCE_REGISTRY=/Users/duncan/quicklisp/dists/quicklisp/software//:~/code/nyxt//:~/code/nyxt/_build//"))))
  (when (file-directory-p "~/quicklisp")
    (load (expand-file-name "~/quicklisp/slime-helper.el"))))

(provide '--common-lisp__commonlisp_lisp_programming@@20250505T050846)
;;; --common-lisp__commonlisp_lisp_programming@@20250505T050846.el ends here
