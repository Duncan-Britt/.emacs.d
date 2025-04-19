;;; programming.el --- Code -*- lexical-binding: t; -*-

;;; Commentary:
;; ┌─────────────┐
;; │ Programming │
;; └─────────────┘
;;; Code:
(defun insert-box-comment (text)
  "Create a box comment with TEXT inside using the current mode's comment syntax."
  (interactive "sEnter text for box: ")
  (let* ((raw-comment (if comment-start comment-start "// "))
         (comment-marker
          (cond
           ((derived-mode-p 'emacs-lisp-mode) ";; ")
           ((derived-mode-p 'prolog-mode 'erlang-mode) "%% ")
           ;; Single-character comment markers that should be doubled
           ((and (= (length (string-trim raw-comment)) 1)
                 (not (string-match-p " $" raw-comment)))
            (concat raw-comment raw-comment " "))
           (t (if (string-match-p " $" raw-comment)
                  raw-comment
                (concat raw-comment " ")))))
         (text-width (length text))
         (box-width (+ text-width 2))
         (top-line (concat comment-marker "┌" (make-string box-width ?─) "┐"))
         (mid-line (concat comment-marker "│ " text " │"))
         (bot-line (concat comment-marker "└" (make-string box-width ?─) "┘")))
    (insert top-line "\n" mid-line "\n" bot-line "\n")))

(setq-default indent-tabs-mode nil)

;; Enable built-in tree-sitter support
(use-package treesit
  :ensure nil  ; built-in package
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

(use-package magit
  :after transient
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (lisp-mode . rainbow-delimiters-mode)))

(use-package sql-indent
  :ensure t
  :hook ((sql-mode . sqlind-minor-mode)
         (cql-mode . sqlind-minor-mode))
  ;; You can further customize indentation or align rules here if needed
  )

(use-package simple-httpd
  :ensure t)

(use-package inf-ruby
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)))

;; ┌────────┐
;; │ Elixir │
;; └────────┘

(use-package elixir-mode
  :ensure t)

(use-package inf-elixir
  :ensure t)

(use-package mix
  :ensure t)

(use-package ob-elixir
  :ensure t)

;; ┌────────┐
;; │ Erlang │
;; └────────┘

(use-package erlang ;; https://www.erlang.org/docs/24/man/erlang.el
  :ensure t)

;; LSP: https://whatsapp.github.io/erlang-language-platform/
;; edts           Erlang Development Tool Suite
;; erlstack-mode  Minor mode for analysing Erlang stacktraces
;; lfe-mode       Flavoured Erlang mode
;; earl           Erlang distribution protocol implementation

;; ┌───────┐
;; │ C/C++ │
;; └───────┘

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

;; (use-package realgud
;;   :ensure t)

;; (use-package realgud-lldb
;;   :after realgud
;;   :ensure t)

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (define-key c-mode-map (kbd "C-c c") 'recompile)))

;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (define-key c++-mode-map (kbd "C-c c") 'recompile)))

;; ┌────────────┐
;; │ Emacs Lisp │
;; └────────────┘

(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode) ;; lambda becomes λ.
(use-package pp
  :ensure nil  ; built-in package
  :bind
  ("C-c C-p" . pp-eval-last-sexp))

;; ┌─────────────┐
;; │ Common Lisp │
;; └─────────────┘

(use-package lisp-mode
  :ensure nil  ; lisp-mode is built-in, so we don't need to ensure it
  :hook ((lisp-mode . prettify-symbols-mode)))
(setq inferior-lisp-program (executable-find "sbcl"))
(setq slime-lisp-implementations
      `((sbcl (,(executable-find "sbcl") "--dynamic-space-size" "4000"))))
;; For hacking on Nyxt:
;; (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
;; (setq slime-lisp-implementations '((nyxt ("/opt/homebrew/bin/sbcl" "--dynamic-space-size 3072")
;;                                          :env ("CL_SOURCE_REGISTRY=/Users/duncan/quicklisp/dists/quicklisp/software//:~/code/nyxt//:~/code/nyxt/_build//"))))
(when (file-directory-p "~/quicklisp")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

;; ┌────────┐
;; │ Prolog │
;; └────────┘

(use-package prolog
  :ensure nil  ; prolog is built-in, so we don't need to ensure it
  :mode ("\\.pl\\'" . prolog-mode)
  :config
  (setq prolog-electric-if-then-else-flag t)
  (setq prolog-program-name "scryer-prolog"))

(use-package ciao
  ;; NOTE Installing Ciao Prolog will install ciao_emacs in .ciaoroot
  ;; and append elisp to load ciao mode to init.el.
  :ensure nil
  :hook ((ciao-mode . display-line-numbers-mode)
         (ciao-mode . my-ciao-remap-faces)
         (ciao-mode . hl-todo-mode))
  :config
  (defun my-ciao-remap-faces ()
    "Remap Ciao faces to standard font-lock faces using face-remapping-alist."
    (face-remap-add-relative 'ciao-face-variable 'font-lock-variable-name-face)
    (face-remap-add-relative 'ciao-face-string 'font-lock-string-face)
    (face-remap-add-relative 'ciao-face-comment 'font-lock-comment-face)
    (face-remap-add-relative 'ciao-face-clauseheadname 'font-lock-function-name-face)
    ;; (face-remap-add-relative 'ciao-face-quoted-atom 'font-lock-constant-face)
    ;; (face-remap-add-relative 'ciao-face-script-header 'font-lock-preprocessor-face)
    ;; (face-remap-add-relative 'ciao-face-concurrency-op 'font-lock-keyword-face)
    (face-remap-add-relative 'ciao-face-cut 'font-lock-negation-char-face)
    ;; (face-remap-add-relative 'ciao-face-funexp-atom 'font-lock-function-name-face)
    ;; (face-remap-add-relative 'ciao-face-builtin-directive 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-predicate-directive 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-module-directive 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-condcode-directive 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-library-directive 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-user-directive 'font-lock-builtin-face)

    ;; (face-remap-add-relative 'ciao-face-checked-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-true-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-false-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-trust-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-entry-assrt 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-check-assrt 'font-lock-preprocessor-face)
    ;; (face-remap-add-relative 'ciao-face-prop-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-test-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-texec-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-type-assrt 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-modedef-assrt 'font-lock-builtin-face)

    (face-remap-add-relative 'ciao-face-prompt 'font-lock-keyword-face)
    ;; (face-remap-add-relative 'ciao-face-answer-var 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-answer-val 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-yes-answer 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-no-answer 'font-lock-keyword-face)

    ;; (face-remap-add-relative 'ciaopp-option 'font-lock-builtin-face)

    ;; (face-remap-add-relative 'ciao-face-startup-message 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-error-mess 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-warning-mess 'font-lock-warning-face)
    ;; (face-remap-add-relative 'ciao-face-note-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-passed-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-failed-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-aborted-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-other-mess 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-highlight-code 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-selection-5-face 'font-lock-builtin-face)

    ;; (face-remap-add-relative 'ciao-face-lpdoc-command 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-lpdoc-verbatim 'font-lock-builtin-face)
    (face-remap-add-relative 'ciao-face-lpdoc-comment 'font-lock-doc-face)
    ;; (face-remap-add-relative 'ciao-face-lpdoc-bug-comment 'font-lock-builtin-face)

    ;; (face-remap-add-relative 'ciao-face-debug-call 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-exit 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-fail 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-redo 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-expansion 'font-lock-builtin-face)
    ;; (face-remap-add-relative 'ciao-face-debug-breakpoint 'font-lock-builtin-face)
    ))

(use-package sweeprolog
  :ensure t
  ;; :mode ("\\.pl\\'" . sweeprolog-mode)
  :hook ((sweeprolog-mode . sweeprolog-electric-layout-mode)
         ;; (sweeprolog-mode . (lambda ()
         ;;                     (push '(":-" . "←") prettify-symbols-alist)
         ;;                     (prettify-symbols-mode 1)))
         )
  :config
  (setq sweeprolog-enable-flymake nil))

(use-package ob-prolog
  :ensure t)

;; ┌────────┐
;; │ Python │
;; └────────┘

(use-package exec-path-from-shell ; Without this, eshell would use the homebrew version of python.
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package python
  :ensure nil
  :config
  (when (and (eq system-type 'gnu/linux)
             (string= (system-name) "duncans-macbookpro"))
    (setq python-shell-interpreter "python3.10")))

(use-package lark-mode
  :ensure t)

;; ┌──────────┐
;; │ Assembly │
;; └──────────┘

(add-hook 'asm-mode-hook
          (lambda ()
            (setq comment-start "#")
            (setq comment-end "")))

;; ┌──────┐
;; │ TLA+ │
;; └──────┘

;; See lisp/tla+-mode.el
(require 'tla+-mode)

(use-package tla+-mode
  :ensure nil
  :mode ("\\.tla\\'" . tla+-mode)
  :hook (tla+-mode . (lambda ()
                       (tla+/load-symbols)
                       (prettify-symbols-mode 1))))

;; ┌──────────┐
;; │ PlantUML │
;; └──────────┘

(use-package plantuml-mode
  :ensure t
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/Downloads/installers/plantuml-mit-1.2025.1.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; ┌─────────────┐
;; │ Environment │
;; └─────────────┘
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

;; ┌──────────┐
;; │ Networks │
;; └──────────┘
(use-package restclient
  ;; see examples:
  ;; https://github.com/pashky/restclient.el/blob/master/examples/httpbin
  :ensure t)

(use-package ob-restclient
  :ensure t)

;; ┌─────────┐
;; │ Jupyter │
;; └─────────┘
(use-package-local-or-remote
 jupyter-ascending
 "~/code/my-emacs-packages/jupyter-ascending/"
 "Duncan-Britt/jupyter-ascending"
  :ensure nil
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

(provide 'programming)
