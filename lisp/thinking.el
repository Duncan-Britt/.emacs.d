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

  :hook ((org-mode . my/org-syntax-table-modify)))

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

;; ┌─────┐
;; │ RSS │
;; └─────┘

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds '(("https://sachachua.com/blog/feed/index.xml" blog emacs)
                       ("https://karthinks.com/index.xml" blog emacs)
                       ("http://yummymelon.com/devnull/feeds/all.atom.xml" blog emacs)
                       ("https://mcclim.common-lisp.dev/rss.xml" programming lisp gui)
                       ("https://bitbashing.io/feed.xml" blog programming)
                       ("https://world-playground-deceit.net/blog/new-posts.xml" blog programming lisp emacs)
                       ("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-only.xml" blog emacs)
                       ("https://learnyousomeerlang.com/rss" book erlang programming)
                       ("https://protagon.space/posts/index.xml" blog emacs)
                       ("https://borretti.me/feed.xml" blog programming)
                       ("https://malisper.me/category/postgres/feed/" blog programming)
                       ("https://ianthehenry.com/feed.xml" programming)
                       ("https://www.teamten.com/lawrence/writings/rss.xml" blog programming)
                       ("https://aartaka.me/rss.xml" blog programming lisp)
                       ("https://map.simonsarris.com/feed" blog)
                       ("https://spakhm.substack.com/feed" blog)
                       ("https://wrathofgnon.substack.com/feed" blog lowtech)
                       ("https://solar.lowtechmagazine.com/posts/index.xml" lowtech)
                       ("https://solar.lowtechmagazine.com/vn/posts/index.xml" lang-vn lowtech)
                       ("https://nntaleb.medium.com/feed" blog)
                       ("https://fukamachi.hashnode.dev/rss.xml" blog lisp)
                       ("https://protesilaos.com/master.xml" blog emacs)
                       ("http://www.aaronsw.com/2002/feeds/pgessays.rss" blog lisp business)
                       ("https://www.joelonsoftware.com/feed/" blog software business)
                       ("http://xahlee.info/comp/blog.xml" blog programming)
                       ("http://xahlee.info/js/blog.xml" blog webdev)
                       ("http://xahlee.info/emacs/emacs/blog.xml" blog emacs)
                       ("https://entropicthoughts.com/feed" blog)
                       ("https://steve-yegge.blogspot.com/atom.xml" blog emacs programming)
                       ("https://irreal.org/blog/?feed=rss2" blog emacs programming)
                       ("https://esrh.me/feed" blog emacs lisp programming)
                       ("https://worldspiritsockpuppet.com/feed.xml" blog ai philosophy)
                       ("https://eshelyaron.com/rss.xml" blog emacs prolog)
                       ("https://codelearn.me/feed.xml" blog emacs programming)
                       ("https://joarvarndt.se/rss.xml" blog emacs)
                       ("https://p.bauherren.ovh/rss" blog emacs)
                       ("https://funcall.blogspot.com/feeds/posts/default" blog lisp programming)
                       ("https://mcmansionhell.com/rss" blog architecture)
                       ("https://rvirding.blogspot.com/feeds/posts/default" blog programming erlang)
                       ;; Can look for more blogs here: https://actionlab.strongtowns.org/hc/en-us/articles/360059026332-Member-Blog-Roll
                       ("https://www.strongtowns.org/journal?format=rss" urban-planning)
                       ("https://nyxt.atlas.engineer/feed" lisp tools)
                       ("https://tomscii.sig7.se/feed.xml" blog programming)
                       ("https://thanosapollo.org/index.xml" blog emacs)
                       ("https://rakhim.exotext.com/rss.xml" blog webdev programming)
                       ("https://stevana.github.io/rss.xml" blog programming)
                       ("https://pvk.ca/atom.xml" blog programming algorithms lisp)
                       ("https://100r.co/links/rss.xml" blog sailing lowtech programming)
                       ("https://bernsteinbear.com/feed.xml" blog programming))))


;; `elfeed-score' https://github.com/sp1ff/elfeed-score
;; (use-package elfeed-score
;;   :ensure t
;;   :config
;;   (progn
;;     (elfeed-score-enable)
;;     (define-key elfeed-search-mode-map "=" elfeed-score-map)))

;; `elfeed-goodies'
;; https://github.com/jeetelongname/elfeed-goodies
;; (use-package elfeed-goodies
;;   :after elfeed
;;   :ensure t
;;   :config
;;   (elfeed-goodies/setup))

;; `elfeed-tube'
;; https://github.com/karthink/elfeed-tube
;; (use-package elfeed-tube
;;   :ensure t ;; or :straight t
;;   :after elfeed
;;   :demand t
;;   :config
;;   ;; (setq elfeed-tube-auto-save-p nil) ; default value
;;   ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
;;   (elfeed-tube-setup)

;;   :bind (:map elfeed-show-mode-map
;;          ("F" . elfeed-tube-fetch)
;;          ([remap save-buffer] . elfeed-tube-save)
;;          :map elfeed-search-mode-map
;;          ("F" . elfeed-tube-fetch)
;;          ([remap save-buffer] . elfeed-tube-save)))

(provide 'thinking)
