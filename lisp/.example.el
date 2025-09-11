;;; example.el --- snippets I'm not using -*- lexical-binding: t; -*-

;;; Commentary:
;; Could be useful someday. But the code in this file is not loaded!
;;; Code:

;; NOTE: I ended up with a different solution to the problem described below, which involves
;; ammending org-preview-latex-process-alist. See `org-preview-latex-default-process'.
;; ====================================================================================
;; If I were to simply add
;;   #+latex_header: \usepackage{fontspec}
;;   #+latex_header: \setmainfont{Athelas}
;; to an org file, it breaks org latex previews because fontspec is only available with
;; the xelatex compiler. Therefore, to ensure the normal functioning of org latex
;; previews, I don't do the above, and instead, I set the latex_class to
;; `article-athelas', defined below, so that I can use fontspec on export
;; without it interfering with org latex previews.
;; For some reason, when I do this, (instead of of #+latex_header: \usepackage{fontspec}),
;; I also have to add the line #+latex_compiler: xelatex, even though I setq the
;; org-latex-pdf-process to xelatex above. I don't know why.

(use-package org
  :ensure nil
  :config
  (add-to-list 'org-latex-classes
               '("article-athelas"
                 "\\documentclass[11pt]{article}
\\usepackage{fontspec}
\\setmainfont{Athelas}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; ┌──────────────────────────────┐
;; │ Black and White (Tao) Themes │
;; └──────────────────────────────┘
;; (add-to-list 'load-path "~/.emacs.d/lisp/my-tao-themes/")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/my-tao-themes/")
;; (setq tao-theme-use-height t)
;; (require 'tao-theme)
;; (setq *theme-switcher-themes-dark* (cons "tao-yin" (mapcar #'symbol-name ef-themes-dark-themes)))
;; (setq *theme-switcher-themes-light* (cons "tao-yang" (mapcar #'symbol-name ef-themes-light-themes)))
;; I vendored the toa themes package and edited font-lock-comment face
;; on line 223, setting it to
;; `(font-lock-comment-face ((t (:foreground ,color-9 :weight normal :italic nil :variable-pitch nil :height ,(tao-theme-height 1.1) :inherit fixed-pitch ))))

;; ┌──────────┐
;; │ Assembly │
;; └──────────┘

;; (add-hook 'asm-mode-hook
;;           (lambda ()
;;             (setq comment-start "#")
;;             (setq comment-end "")))

;; ┌────────┐
;; │ Python │
;; └────────┘

(use-package python
  :ensure nil
  :config
  (when (and (eq system-type 'gnu/linux)
             (string= (system-name) "duncans-macbookpro"))
    (setq python-shell-interpreter "python3.10")))

;; ┌─────────────┐
;; │ Ciao Prolog │
;; └─────────────┘
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

;; ┌────────────┐
;; │ GDB & LLDB │ NOTE: I think LLDB is built in for Emacs 30
;; └────────────┘
;; (use-package realgud
;;   :ensure t)

;; (use-package realgud-lldb
;;   :after realgud
;;   :ensure t)

;; I'm no longer using ace-window because Avy solves moving between
;; frames for me.
;; (use-package ace-window
;;   :ensure t
;;   :bind
;;   (("C-x o" . ace-window))
;;   :config
;;   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;; The following 2 funs causes an issue where (text-scale-set 2),
;; which should only apply to the current buffer, applies to all the
;; buffers.
(defun my/org-remap-faces-hook (&rest args)
  "When using Athelas font, setq-local `face-remapping-alist'
s.t. variable and fixed pitch font sizes are proportional"
  (if (and (member (bound-and-true-p fontaine-current-preset)
                     '(Athelas-regular Athelas-share-screen))
             (eq major-mode 'org-mode))
    (setq-local face-remapping-alist `((default (:height 1.3) variable-pitch)
                                       (header-line (:height 1.0) fixed-pitch)
                                       (org-document-title (:height 1.0) org-document-title)
                                       (org-code (:height 0.9) org-code)
                                       (org-verbatim (:height 0.9) org-verbatim)
                                       (org-block (:height 0.9) org-block)
                                       (org-block-begin-line (:height 0.9) org-block-begin-line)
                                       (org-table (:height 0.9) org-table)
                                       (org-checkbox (:height 0.9) org-checkbox)
                                       (org-date (:height 0.9) org-date)
                                       (org-drawer (:height 0.9) org-drawer)
                                       (org-meta-line (:height 0.9) org-meta-line)))
    (setq-local face-remapping-alist '((default variable-pitch)))))

(defun my/apply-org-face-remapping-to-all-buffers (&rest _)
  "Apply org face remapping to all org-mode buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'org-mode)
        (my/org-remap-faces-hook)))))

;; ┌───────────┐
;; │ Hyperbole │
;; └───────────┘
;;; Commentary:
;; title: use-hyperbole
;; keywords: :buttons:hypertext:
;; date: [2025-05-06 Tue 07:33]
;; identifier: 20250506T073343
;; ┌────────────────────────────────────────────────────────────┐
;; │ I stopped using hyperbole because `embark-act' replaced    │
;; │ `hkey-either' for me. It's more light weight and flexible. │
;; └────────────────────────────────────────────────────────────┘
(use-package hyperbole
  :ensure (:host github :repo "emacs-straight/hyperbole")
  :after org
  :custom
  (hkey-init nil) ;; I don't want Hyperbole's keybindings.
  :config
  (hyperbole-mode 1)
  :bind (("M-RET" . hkey-either)
         (:map org-mode-map
               ("M-RET" . hkey-either))))
