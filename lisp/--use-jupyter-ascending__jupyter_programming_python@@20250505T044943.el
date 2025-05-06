;;; --use-jupyter-ascending__jupyter_programming_python@@20250505T044943.el --- use-jupyter-ascending -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-jupyter-ascending
;; keywords: :jupyter:programming:python:
;; date: [2025-05-05 Mon 04:49]
;; identifier: 20250505T044943

;;; Code:
(require '--portable__utility@@20250505T200406)

(use-package-local-or-remote
 jupyter-ascending
 "~/code/my-emacs-packages/jupyter-ascending/"
 "Duncan-Britt/jupyter-ascending"
 :hook (python-mode . (lambda ()
                        (when (and buffer-file-name
                                   (string-match-p "\\.sync\\.py\\'" buffer-file-name))
                          (jupyter-ascending-mode 1))))
 :bind (:map jupyter-ascending-mode-map
             ("C-c C-k" . jupyter-ascending-execute-line)
             ("C-c C-a" . jupyter-ascending-execute-all)
             ("C-c C-n" . jupyter-ascending-next-cell)
             ("C-c C-p" . jupyter-ascending-previous-cell)
             ("C-c t" . jupyter-ascending-cycle-cell-type)
             ("C-c '" . jupyter-ascending-edit-markdown-cell)))

(provide '--use-jupyter-ascending__jupyter_programming_python@@20250505T044943)
;;; --use-jupyter-ascending__jupyter_programming_python@@20250505T044943.el ends here
