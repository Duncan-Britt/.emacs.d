;;; --org-style__appearance_org_prose@@20250503T141656.el --- org-style -*- lexical-binding: t -*-

;;; Commentary:
;; title: org-style
;; keywords: :appearance:org:prose:
;; date: [2025-05-03 Sat 14:16]
;; identifier: 20250503T141656

;;; Code:
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

(use-package org
  :ensure nil
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
  (setq org-image-actual-width nil)
  (setq org-preview-latex-default-process 'dvisvgm) ; Better latex rendering
  (defun my/resize-org-latex-overlays ()
    "Auto resize latex when resizing text"
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
   (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'my/resize-org-latex-overlays nil t)))
   (org-mode . my/org-remap-faces-hook)))

(use-package org-bullets
  :after org
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; ALTERNATIVE TO ORG BULLETS - org-ibullets
;; (use-package org-ibullets ; <- last time I tried this (4/2025), seemed to chop off the left side of the bullets
;;   :after org
;;   :ensure (:host github :repo "jamescherti/org-ibullets.el")
;;   :commands org-ibullets-mode
;;   :hook (org-mode . org-ibullets-mode))

(use-package org-modern
  :after org
  :ensure t
  :hook ((org-mode . org-modern-mode))
  :custom
  (org-modern-block-fringe nil)
  (org-modern-table nil)
  (org-modern-star nil))

(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t))

(use-package org-fragtog
  ;; automatically toggle latex previews in org mode.
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(use-package-local-or-remote
 fontaine-org
 "~/code/my-emacs-packages/fontaine-org/"
 "Duncan-Britt/fontaine-org"
 :after fontaine
 :config
 (defvar font-state-file (expand-file-name "last-font" user-emacs-directory)
   "File to save the last used theme.")

 (defun load-last-font ()
   "Load the last used theme from file."
   (if (file-exists-p font-state-file)
       (progn
         (let ((last-font (with-temp-buffer
                            (insert-file-contents font-state-file)
                            (intern (buffer-string)))))
           (fontaine-set-preset last-font)))
     (fontaine-set-preset 'Iosevka-regular)))

 (defun save-current-font (&rest _)
   "Save the current theme to a file."
   (when-let ((current-font-preset (car fontaine-preset-history)))
     (with-temp-file font-state-file
       (insert current-font-preset))))

 (fontaine-org-mode 1)
 (load-last-font)
 (advice-add 'fontaine-set-preset :after #'save-current-font)
 (advice-add 'fontaine-set-preset :after #'my/apply-org-face-remapping-to-all-buffers))

(provide '--org-style__appearance_org_prose@@20250503T141656)
;;; --org-style__appearance_org_prose@@20250503T141656.el ends here
