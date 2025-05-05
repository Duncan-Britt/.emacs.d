;;; --use-treesit__programming_syntax@@20250505T095333.el --- use-treesit -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-treesit
;; keywords: :programming:syntax:
;; date: [2025-05-05 Mon 09:53]
;; identifier: 20250505T095333

;; Enable built-in tree-sitter support
;;; Code:
(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (c "https://github.com/tree-sitter/tree-sitter-c")
	  (common-lisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Enable tree-sitter for languages with available grammars
  (setq major-mode-remap-alist
        ;; I don't use c++-ts-mode because << on new lines don't indent correctly. ~ 2024
        ;; I don't use python-ts-mode, forgot why ~ 2024
        ;; I don't use ruby-ts-mode because C-M-f doesn't work in it - Oct. 2024
	'((bash-mode . bash-ts-mode)
	  (c-mode . c-ts-mode)
          (css-mode . css-ts-mode)
	  (json-mode . json-ts-mode)
	  (yaml-mode . yaml-ts-mode)))

  (dolist (mapping major-mode-remap-alist)
    (let* ((ts-mode (cdr mapping))
           (lang (intern (string-remove-suffix "-ts-mode" (symbol-name ts-mode)))))
      (when (and (not (treesit-ready-p lang t))
		 (not (eq lang 'c++))) ; Do c++ manually, interactively, if desired
        (treesit-install-language-grammar lang))))

  (defun check-treesit-grammar-installation ()
    "Check and report the installation status of tree-sitter grammars.
Note that it may show that C++ is not installed even when it is. Check with `M-x c++-ts-mode'"
    (let (installed not-installed)
      (dolist (mapping major-mode-remap-alist)
	(let* ((ts-mode (cdr mapping))
               (lang (intern (string-remove-suffix "-ts-mode" (symbol-name ts-mode)))))
          (if (treesit-ready-p lang t)
              (push lang installed)
            (push lang not-installed))))

      (with-current-buffer (get-buffer-create "*Tree-sitter Status*")
	(erase-buffer)
	(insert "Tree-sitter Grammar Installation Status:\n\n")
	(insert "Note that it may show that C++ is not installed even when it is. Check with `M-x c++-ts-mode`")
	(insert "Installed grammars:\n")
	(dolist (lang installed)
          (insert (format "  - %s\n" lang)))
	(insert "\nNot installed grammars:\n")
	(dolist (lang not-installed)
          (insert (format "  - %s\n" lang)))
	(insert "\nTo install missing grammars, you can use:\n")
	(insert "(treesit-install-language-grammar 'language-name)\n")
	(display-buffer (current-buffer)))))
  ;; (check-treesit-grammar-installation)
  )

(provide '--use-treesit__programming_syntax@@20250505T095333)
;;; --use-treesit__programming_syntax@@20250505T095333.el ends here
