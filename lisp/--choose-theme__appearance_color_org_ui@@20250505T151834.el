;;; --choose-theme__appearance_color_org_ui@@20250505T151834.el --- choose-theme -*- lexical-binding: t -*-

;;; Commentary:
;; title: choose-theme
;; keywords: :appearance:color:org:ui:
;; date: [2025-05-05 Mon 15:18]
;; identifier: 20250505T151834

;;; Code:
(require '--util__helper_utility@@20250505T154748)

(with-eval-after-load-all (ef-themes consult calle24 hl-todo fontaine-org)
  (require 'ef-themes)
  (require 'consult)
  (require 'calle24)
  (require 'hl-todo)

  (defun insert-dark-before-extension (filepath)
    "Insert '-dark' before the file extension in the given FILEPATH."
    (let* ((file (file-name-nondirectory filepath)) ; Extract the file name
           (dir  (file-name-directory filepath))    ; Extract the directory path
           (name (file-name-base file))             ; Extract the base name without extension
           (ext  (file-name-extension file)))       ; Extract the extension
      (concat dir name "-dark." ext)))

  (defun my/toggle-inline-images (&optional include-linked beg end)
    "Toggle the display of inline images,
but use dark versions for dark mode."
    (interactive "P")
    (if (org--inline-image-overlays beg end)
        (progn
          (org-remove-inline-images beg end)
          (when (called-interactively-p 'interactive)
	    (message "Inline image display turned off")))
      (my/display-inline-images include-linked nil beg end)
      (when (called-interactively-p 'interactive)
        (let ((new (org--inline-image-overlays beg end)))
          (message (if new
		       (format "%d images displayed inline"
			       (length new))
		     "No images to display inline"))))))

  (use-package org
    :ensure nil
    :bind (:map org-mode-map
                ("C-c C-x C-v" . my/toggle-inline-images)))

  (defun my/display-inline-images (&optional include-linked refresh beg end)
    "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" or
     \"attachment\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
    (interactive "P")
    (when (display-graphic-p)
      (when refresh
        (org-remove-inline-images beg end)
        (when (fboundp 'clear-image-cache) (clear-image-cache)))
      (let ((end (or end (point-max))))
        (org-with-point-at (or beg (point-min))
	  (let* ((case-fold-search t)
	         (file-extension-re (image-file-name-regexp))
	         (link-abbrevs (mapcar #'car
				       (append org-link-abbrev-alist-local
					       org-link-abbrev-alist)))
	         ;; Check absolute, relative file names and explicit
	         ;; "file:" links.  Also check link abbreviations since
	         ;; some might expand to "file" links.
	         (file-types-re
		  (format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?\\(?:file\\|attachment\\):\\)"
			  (if (not link-abbrevs) ""
			    (concat "\\|" (regexp-opt link-abbrevs))))))
	    (while (re-search-forward file-types-re end t)
	      (let* ((link (org-element-lineage
			    (save-match-data (org-element-context))
			    '(link) t))
                     (linktype (org-element-property :type link))
		     (inner-start (match-beginning 1))
		     (path
		      (cond
		       ;; No link at point; no inline image.
		       ((not link) nil)
		       ;; File link without a description.  Also handle
		       ;; INCLUDE-LINKED here since it should have
		       ;; precedence over the next case.  I.e., if link
		       ;; contains filenames in both the path and the
		       ;; description, prioritize the path only when
		       ;; INCLUDE-LINKED is non-nil.
		       ((or (not (org-element-property :contents-begin link))
			    include-linked)
		        (and (or (equal "file" linktype)
                                 (equal "attachment" linktype))
			     (org-element-property :path link)))
		       ;; Link with a description.  Check if description
		       ;; is a filename.  Even if Org doesn't have syntax
		       ;; for those -- clickable image -- constructs, fake
		       ;; them, as in `org-export-insert-image-links'.
		       ((not inner-start) nil)
		       (t
		        (org-with-point-at inner-start
			  (and (looking-at
			        (if (char-equal ?< (char-after inner-start))
				    org-link-angle-re
				  org-link-plain-re))
			       ;; File name must fill the whole
			       ;; description.
			       (= (org-element-property :contents-end link)
				  (match-end 0))
			       (progn
                                 (setq linktype (match-string 1))
                                 (match-string 2))))))))
                (when (and path (member (car custom-enabled-themes) ef-themes-dark-themes)) ;; <-- Modifications to original: check if in dark mode and update path
                  (let ((new-path (insert-dark-before-extension path)))
                    (when (file-exists-p new-path)
                      (setq path new-path)))) ;; <-- end modifications
	        (when (and path (string-match-p file-extension-re path))
		  (let ((file (if (equal "attachment" linktype)
				  (progn
                                    (require 'org-attach)
				    (ignore-errors (org-attach-expand path)))
                                (expand-file-name path))))
		    (when (and file (file-exists-p file))
		      (let ((width (org-display-inline-image--width link))
			    (old (get-char-property-and-overlay
				  (org-element-property :begin link)
				  'org-image-overlay)))
		        (if (and (car-safe old) refresh)
                            (image-flush (overlay-get (cdr old) 'display))
			  (let ((image (org--create-inline-image file width)))
			    (when image
			      (let ((ov (make-overlay
				         (org-element-property :begin link)
				         (progn
					   (goto-char
					    (org-element-property :end link))
					   (skip-chars-backward " \t")
					   (point)))))
                                ;; FIXME: See bug#59902.  We cannot rely
                                ;; on Emacs to update image if the file
                                ;; has changed.
                                (image-flush image)
			        (overlay-put ov 'display image)
			        (overlay-put ov 'face 'default)
			        (overlay-put ov 'org-image-overlay t)
			        (overlay-put
			         ov 'modification-hooks
			         (list 'org-display-inline-remove-overlay))
			        (when (boundp 'image-map)
				  (overlay-put ov 'keymap image-map))
			        (push ov org-inline-image-overlays))))))))))))))))

  (defun my/refresh-inline-images ()
    "If displaying inline images, stop and restart display."
    (when (org--inline-image-overlays)
      (org-remove-inline-images)
      (message "hello")
      (ts-display-inline-images)))

  (defvar my/theme-state-file (expand-file-name "last-theme" user-emacs-directory)
    "File to save the last used theme.")

  (defun my/save-current-theme (&rest _)
    "Save the current theme to a file."
    (when-let ((current-theme (car custom-enabled-themes)))
      (with-temp-file my/theme-state-file
        (insert (symbol-name current-theme)))))

  (defun my/set-hl-todo-faces-according-to-ef-theme ()
    "Sets the faces of different TODO-esq keywords for the hl-todo package.
Done in accordance with the currently loaded ef-theme."
    (let ((theme (car custom-enabled-themes)))
      (when (and (boundp 'ef-themes-collection)
	         (member theme ef-themes-collection))
        (let* ((palette-var (intern (concat (symbol-name theme) "-palette")))
	       (palette (symbol-value palette-var))
	       (red (cadr (assoc 'red palette)))
	       (green (cadr (assoc 'green palette)))
	       (yellow (cadr (assoc 'yellow palette)))
	       (cyan (cadr (assoc 'cyan palette))))
	  (setq hl-todo-keyword-faces
	        `(("TODO" . ,red)
		  ("DONE" . ,green)
		  ("NOTE" . ,yellow)
		  ("FIXME" . ,red)
		  ("OKAY" . ,cyan)))))))

  (defun my/set-frame-light-or-dark (light-or-dark)
    "Update frame border to be light or dark.
Uses MacOS ns frame parameters and calle24 symbols"
    (when (eq 'ns window-system)
      (if (eq light-or-dark 'light)
          (calle24-dark-appearance)
        (calle24-light-appearance))
      (dolist (frame (frame-list))
        (set-frame-parameter frame 'ns-appearance light-or-dark))))

  (defun my/load-theme (theme)
    "Load 'doric' or 'ef' THEME.
THEME should be a symbol."
    (cond ((string-prefix-p "ef" (symbol-name theme))
           (ef-themes-select theme))
          ((string-prefix-p "doric" (symbol-name theme))
           (doric-themes-select theme)
           ;; NOTE: Doric themes doesn't (yet) have a post load hook
           ;; like ef-themes, so that is why these are here but not in
           ;; the ef-themes case.
           (my/save-current-theme)
           (my/refresh-inline-images)
           (my/set-hl-todo-faces-according-to-ef-theme)
           (global-hl-todo-mode))
          (t
           (user-error "Expected ef or doric theme, found %s" theme))))

  (defun my/apply-theme (theme)
    "Apply THEME with proper handling of default theme and refreshing."
    (message "theme chosen: %s" theme)
    (let ((theme-sym (if (stringp theme) (intern theme) theme)))
      (when (eq theme-sym 'default)
        (setq theme-sym nil))
      (unless (eq theme-sym (car custom-enabled-themes))
        (mapc #'disable-theme custom-enabled-themes)
        (when theme-sym
          (my/load-theme theme-sym)))))

  (setq ef-themes-post-load-hook (lambda ()
                                   (my/save-current-theme)
                                   (my/refresh-inline-images)
                                   (my/set-hl-todo-faces-according-to-ef-theme)
                                   (global-hl-todo-mode)))

  (defun my/choose-theme ()
    "Switch color theme.
Based on choice of light or dark mode, also set frame property 'ns-appearance"
    (interactive)
    (let ((light-or-dark (intern
                          (downcase
                           (completing-read "Choose category: "
                                            '("Light" "Dark")))))
          (saved-theme (car custom-enabled-themes))
          (saved-ns-appearance (frame-parameter nil 'ns-appearance)))
      (my/set-frame-light-or-dark light-or-dark)
      (condition-case nil
          (consult--read
           (mapcar #'symbol-name (if (eq light-or-dark 'light)
                                     (append ef-themes-light-themes
                                             doric-themes-light-themes)
                                   (append ef-themes-dark-themes
                                           doric-themes-dark-themes)))
           :prompt "Choose theme: "
           :require-match t
           :category 'theme
           :history 'consult--theme-history
           :lookup (lambda (selected &rest _)
                     (setq selected (and selected (intern-soft selected)))
                     (or (and selected (car (memq selected (append ef-themes-collection doric-themes-collection))))
                         saved-theme))
           :state (lambda (action theme)
                    (pcase action
                      ('return (my/apply-theme theme))
                      ((and 'preview (guard theme))
                       (my/apply-theme theme))))
           :default (symbol-name (or saved-theme 'default)))
        (quit
         (my/apply-theme saved-theme)
         (my/set-frame-light-or-dark saved-ns-appearance)))))

  (defun my/load-last-theme ()
    "Load the last used theme from file."
    (if (file-exists-p my/theme-state-file)
        (progn
          (mapc #'disable-theme custom-enabled-themes) ; Disable all other themes to avoid awkward blending:
          (let ((last-theme (with-temp-buffer
                              (insert-file-contents my/theme-state-file)
                              (intern (buffer-string)))))
            (my/load-theme last-theme)))
      (ef-themes-select 'ef-dream)))

  (run-at-time 0.2 nil #'my/load-last-theme)
  (global-set-key (kbd "C-t") #'my/choose-theme))

(provide '--choose-theme__appearance_color_org_ui@@20250505T151834)
;;; --choose-theme__appearance_color_org_ui@@20250505T151834.el ends here
