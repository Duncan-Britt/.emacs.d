;;; --org-mode__notebook_notetaking_org_organization_programming_prose_publishing_thinking@@20250505T110116.el --- org-mode -*- lexical-binding: t -*-

;;; Commentary:
;; title: org-mode
;; keywords: :notebook:notetaking:org:organization:programming:prose:publishing:thinking:
;; date: [2025-05-05 Mon 11:01]
;; identifier: 20250505T110116

;;; Code:
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

(use-package ob-async
  :ensure t
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("ipython")))

(provide '--org-mode__notebook_notetaking_org_organization_programming_prose_publishing_thinking@@20250505T110116)
;;; --org-mode__notebook_notetaking_org_organization_programming_prose_publishing_thinking@@20250505T110116.el ends here
