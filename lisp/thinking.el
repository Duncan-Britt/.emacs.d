;;; thinking.el --- planning, writing, note taking, etc... -*- lexical-binding: t; -*-

;;; Commentary:
;; ┌──────────┐
;; │ Thinking │
;; └──────────┘
;;; Code:
(require 'portable)
(require 'morning-pages)
;; ┌──────────┐
;; │ Org Mode │
;; └──────────┘

(use-package org
  :ensure nil
  :after (ob-prolog ob-elixir ob-restclient)
  :config
  (defun my/org-syntax-table-modify ()
    "Modify `org-mode-syntax-table' for the current org buffer.
This fixes the issue where, in org source blocks, < matches )."
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

  (setq org-agenda-files (list (expand-file-name "~/Dropbox/agenda/agenda.org")))
  ;; (setq org-archive-location "~/Dropbox/agenda/agenda_archive.org::%s_archive") ;; <-- unused? Org Archiver has it's own location.
  ;; (setq org-plantuml-jar-path (expand-file-name "~/plantuml-1.2024.4.jar")) ;; <-- doesn't exist on my new mac
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages ;; Org source block execution
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (ruby . t)
     (js . t)
     (C . t)
     (octave . t)
     (lisp . t)
     (dot . t)
     (matlab . t)
     (sql . t)
     (plantuml . t)
     (shell . t)
     (prolog . t)
     (elixir . t)
     (restclient . t)))
  ;; Needed to run mysql in org babel
  (add-to-list 'exec-path "/usr/local/mysql-8.3.0-macos14-x86_64/bin") ;; <-- doesn't exist on new mac
  (setq org-babel-python-command "python3")
  (setq org-log-note-clock-out t)
  (setq org-list-allow-alphabetical t)
  (setq org-latex-listings 'minted ;; Export to LateX PDF using minted package
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-export-backends '(ascii html icalendar latex md))

  (require 'ox-gfm nil t) ;; <-- For github flavored markdown export
  (require 'blog-publishing)
  (require 'ut-table-manager)

  :hook ((org-mode . my/org-syntax-table-modify))
  :bind (("s-a" . org-agenda)))

(use-package org-contrib
  :ensure t
  :config
  ;; Export org subtree without heading using :ignore: tag
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(use-package ox-gfm
  :ensure t
  :after org)

(use-package ox-epub
  :ensure t
  :after org)

(use-package-local-or-remote
 archiver
 "~/code/my-emacs-packages/archiver/"
 "Duncan-Britt/emacs-archiver"
 :after org
 :init
 (setq *archiver-agenda-archive-location*
       (expand-file-name "~/Dropbox/agenda/agenda_archive.org"))
 :bind
 (:map org-mode-map
       ("C-c C-x C-a" . archiver-archive-heading)))

(use-package ob-async
  :ensure t
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("ipython")))

(use-package org-download
  :ensure t
  :custom
  (org-download-image-attr-list '("#+attr_org: :width 600"))
  (org-download-image-dir ".images/"))

(use-package-local-or-remote
 paste-img
 "~/code/my-emacs-packages/paste-img/"
 "Duncan-Britt/paste-img"
 :after (org org-download)
 :hook
 (org-mode . paste-img-mode))

;; ┌─────────┐
;; │ Writing │
;; └─────────┘

;; Spell checker
(use-package jinx ;; NOTE Custom variable `jinx-include-faces' can be used to add spell checking to comments and string in programming modes. Also see `jinx-exclude-faces'.
  :ensure t
  :hook
  ((org-mode . jinx-mode)
   (text-mode . jinx-mode))
  :bind (("M-$" . jinx-correct))) ;; M-x jinx-languages for other languages.

(use-package mw-thesaurus
  :ensure t
  :after org
  :bind
  (:map org-mode-map
        ("C-c t" . mw-thesaurus-lookup-dwim)))

(use-package reverso ;; Translation, Thesaurus, Grammar Checking (Online only)
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; ┌──────────────────────┐
;; │ Knowledge Management │
;; └──────────────────────┘

(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/Dropbox/notes/"))
  :hook (dired-mode . denote-dired-mode))

(use-package-local-or-remote
 denote-dired-bookmarks
 "~/code/my-emacs-packages/denote-dired-bookmarks/"
 "Duncan-Britt/denote-dired-bookmarks"
 :hook (dired-mode . denote-dired-bookmarks-mode)
 :bind (("s-b" . denote-dired-bookmarks-create-bookmark)))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package-local-or-remote
 org-pdftools
 "~/code/my-emacs-packages/org-pdftools/"
 "Duncan-Britt/org-pdftools"
 :hook (org-mode . org-pdftools-setup-link))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(use-package anki-editor
  :ensure t)

;; ┌───────────┐
;; │ Citations │
;; └───────────┘

(use-package org
  :ensure nil
  :config
  ;; for Bibtex Bibliography styles on latex export
  (setq org-cite-csl-styles-dir "~/code/citation-styles/"))

(use-package citeproc
  :ensure t
  :after org
  :config
  ;; Optional basic configuration
  ;; (setq org-cite-export-processors '((t citeproc)))
  )

;; ┌─────┐
;; │ LLM │
;; └─────┘

(use-package gptel
  :after transient
  :ensure t
  :bind (("C-c RET" . gptel-send))
  :config
  (require 'safe)

  (setq gptel--known-backends (assoc-delete-all "ChatGPT" gptel--known-backends))

  (setq gptel-model 'claude-3-7-sonnet-20250219
        gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t :key *a-gptel-token*))

  (gptel-make-openai "DeepSeek" ; Any name you want
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key *deep-token* ; can be a function that returns the key
    :models '(deepseek-chat deepseek-coder))

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(qwen2.5-coder:14b qwen2.5-coder:32b aya:latest))

  (setq gptel-default-mode 'org-mode))

(provide 'thinking)
