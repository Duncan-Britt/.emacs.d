;;; morning-pages.el --- Based on The Artist's Way -*- lexical-binding: t; -*-

;;; Commentary:
;; ┌───────────────┐
;; │ Morning Pages │
;; └───────────────┘

;;; Code:

(defvar morning-pages--saved-font-preset nil
  "Saved font preset for restoring after journaling.")

(defun morning-pages ()
  "Create a scratch buffer for journaling with word count notification and variable-width font."
  (interactive)
  (let ((buffer (get-buffer-create "*morning-pages*")))
    (pop-to-buffer buffer)    
    (setq-local word-count-threshold 1350)
    (setq-local morning-pages--saved-font-preset fontaine-current-preset)
    (fontaine-set-preset 'Athelas)
    (variable-pitch-mode 1)
    (olivetti-mode 1)
    (delete-other-windows)
    (add-hook 'after-change-functions #'morning-pages--check-word-count nil t)
    (add-hook 'kill-buffer-hook #'morning-pages--restore-font-preset nil t)))

(defun morning-pages--check-word-count (&optional _beg _end _len)
  "Check word count and change background color if threshold is reached."
  (let ((word-count (count-words (point-min) (point-max))))
    (when (>= word-count word-count-threshold)
      (let ((theme (car custom-enabled-themes)))
        (when (and (boundp 'ef-themes-collection)
                   (member theme ef-themes-collection))
          (let* ((palette-var (intern (concat (symbol-name theme) "-palette")))
                 (palette (symbol-value palette-var))
                 (bg (cadr (assoc 'bg-inactive palette))))
            (face-remap-add-relative 'default :background bg)))))))

(defun morning-pages--restore-font-preset ()
  "Restore the saved font preset when the buffer is killed."
  (when morning-pages--saved-font-preset
    (fontaine-set-preset morning-pages--saved-font-preset)))

(provide 'morning-pages)
