;;; --org-style__appearance_org_prose@@20250503T141656.el --- org-style -*- lexical-binding: t -*-

;;; Commentary:
;; title: org-style
;; keywords: :appearance:org:prose:
;; date: [2025-05-03 Sat 14:16]
;; identifier: 20250503T141656

;;; Code:
(use-package org
  :ensure nil
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
  (setq org-image-actual-width nil)
  (setq org-preview-latex-default-process 'dvisvgm) ; Better latex rendering
  (defun my/resize-org-latex-overlays () ; auto resize latex when resizing text
    (cl-loop for o in (car (overlay-lists))
             if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
             do (plist-put (cdr (overlay-get o 'display))
		           :scale (expt text-scale-mode-step
				        text-scale-mode-amount))))
  (plist-put org-format-latex-options :foreground nil) ; latex previews match theme when switching themes.
  (plist-put org-format-latex-options :background nil)

  (add-to-list 'org-entities-user '("yhat" "$\\hat{y}$" nil "&#375;" "yhat" "yhat" "ŷ")) ; FIXME Not sure if I'm dealing with latex in a smart way.
  (add-to-list 'org-entities-user '("vdash" "$\\vdash$" nil "&vdash;" "vdash" "vdash" "⊢"))
  ;; Latin-1 Table: https://cs.stanford.edu/people/miles/iso8859.html
  ;; C-h v org-entities-user RET
  :custom
  (org-hide-emphasis-markers t) ; Hide /emphasis/ markers in org mode
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . variable-pitch-mode)
   (org-mode . visual-line-mode)
   (org-mode . pixel-scroll-precision-mode)
   (org-mode . (lambda () (display-line-numbers-mode 0)))
   (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'my/resize-org-latex-overlays nil t)))))

(use-package org-bullets
  :after org
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; ALTERNATIVES TO ORG BULLETS - org-ibullets and org-modern
;; (use-package org-ibullets ; <- last time I tried this (4/2025), seemed to chop off the left side of the bullets
;;   :after org
;;   :ensure (:host github :repo "jamescherti/org-ibullets.el")
;;   :commands org-ibullets-mode
;;   :hook (org-mode . org-ibullets-mode))
;; (use-package org-modern ; <- source blocks don't play nicely with org-indent-mode
;;   :after org
;;   :ensure t
;;   :hook ((org-mode . org-modern-mode)))

(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t) ; <-- This doesn't work when hyperbole package is loaded.
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t))

(use-package org-fragtog
  ;; automatically toggle latex previews in org mode.
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(provide '--org-style__appearance_org_prose@@20250503T141656)
;;; --org-style__appearance_org_prose@@20250503T141656.el ends here
