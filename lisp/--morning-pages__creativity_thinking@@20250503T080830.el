;;; --morning-pages__creativity_thinking@@20250503T080830.el --- morning-pages -*- lexical-binding: t -*-

;;; Commentary:
;; title: morning-pages
;; keywords: :creativity:thinking:
;; date: [2025-05-03 Sat 08:08]
;; identifier: 20250503T080830

;; ┌--──────────────────-───────────────────────┐
;; │ Morning Pages — based on The Artist's Way │
;; └──--─────────────────-──────────────────────┘
;;; Code:
(defvar morning-pages--saved-font-preset nil
  "Saved font preset for restoring after journaling.")

(defun morning-pages ()
  "Create a scratch buffer for journaling with word count notification and variable-width font."
  (interactive)
  (let ((buffer (get-buffer-create "*morning-pages*")))
    (pop-to-buffer buffer)
    (setq-local word-count-threshold 1350)
    (variable-pitch-mode 1)
    (text-scale-set 2)
    (when (boundp 'olivetti-mode)
      (olivetti-mode 1)
      (olivetti-set-width 80))
    (delete-other-windows)
    (add-hook 'after-change-functions #'morning-pages--check-word-count nil t)))

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

(defun my/insert-15-list ()
  "Insert up to 15."
  (interactive)
  (insert "1.
2.
3.
4.
5.
6.
7.
8.
9.
10.
11.
12.
13.
14.
15."))

(provide '--morning-pages__creativity_thinking@@20250503T080830)
;;; --morning-pages__creativity_thinking@@20250503T080830.el ends here
