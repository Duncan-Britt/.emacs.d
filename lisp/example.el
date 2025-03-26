;;; example.el --- snippets I'm not using -*- lexical-binding: t; -*-

;;; Commentary:
;; Could be useful someday.
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

