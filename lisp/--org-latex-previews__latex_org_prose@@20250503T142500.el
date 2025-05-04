;;; --org-latex-previews__latex_org_prose@@20250503T142500.el --- org-latex-previews -*- lexical-binding: t -*-

;;; Commentary:
;; title: org-latex-previews
;; keywords: :latex:org:prose:
;; date: [2025-05-03 Sat 14:25]
;; identifier: 20250503T142500

;;; Code:
(use-package org
  :ensure nil
  :config
  ;; The following was added because using certain latex packages
  ;; caused problems for latex previews, even when export was fine.
  ;; As I recall, this was a problem:
  ;;     #+latex_header: \hypersetup{linktoc = all, colorlinks = true, linkcolor = blue, citecolor = blue}
  ;; Perhaps also this, I can't remember
  ;;    #+latex_header: \usepackage{fontspec}
  ;;    #+latex_header: \setmainfont{Athelas}
  (add-to-list 'org-preview-latex-process-alist
               '(my-header
                 :programs ("latex" "dvisvgm")
                 :description "Custom process ignoring document headers"
                 :message "you need to install the programs: latex and dvisvgm"
                 :image-input-type "dvi"
                 :image-output-type "svg"
                 :image-size-adjust (1.7 . 1.5)
                 :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
                 :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O")
                 :latex-header "\\documentclass{article}
  \\usepackage[usenames]{color}
  \\usepackage{amsmath,amssymb,amsthm}
  \\pagestyle{empty}
  \\begin{document}"))
  (setq org-preview-latex-default-process 'my-header))

(provide '--org-latex-previews__latex_org_prose@@20250503T142500)
;;; --org-latex-previews__latex_org_prose@@20250503T142500.el ends here
