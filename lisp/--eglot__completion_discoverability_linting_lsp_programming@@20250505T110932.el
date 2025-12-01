;;; --eglot__completion_discoverability_linting_lsp_programming@@20250505T110932.el --- eglot -*- lexical-binding: t -*-

;;; Commentary:
;; title: eglot
;; keywords: :completion:discoverability:linting:lsp:programming:
;; date: [2025-05-05 Mon 11:09]
;; identifier: 20250505T110932

;;; Code:
(use-package eglot
  :ensure nil
  :hook ((c-mode . eglot-ensure)
	 (c-ts-mode . eglot-ensure)
	 (c++-mode . eglot-ensure)
	 (c++-ts-mode . eglot-ensure)
	 (ruby-mode . eglot-ensure)
	 (ruby-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (asm-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-confirm-server-initiated-edits nil)
  (add-to-list 'eglot-server-programs
               '(asm-mode . ("armls")))
  (add-to-list 'eglot-server-programs
               '(web-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(js-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(elixir-mode . ("~/.elixir-ls/language_server.sh")))
  (add-to-list 'eglot-server-programs
               '(erlang-mode . ("elp" "server")))
  (add-to-list 'eglot-server-programs '(fennel-mode . ("fennel-ls")))

  ;; (with-eval-after-load 'eglot
  ;;   (add-to-list 'eglot-server-programs
  ;;                '(text-mode . ("harper-ls" "--stdio"))))
  )

(provide '--eglot__completion_discoverability_linting_lsp_programming@@20250505T110932)
;;; --eglot__completion_discoverability_linting_lsp_programming@@20250505T110932.el ends here
