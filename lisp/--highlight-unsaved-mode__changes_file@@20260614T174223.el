;;; --highlight-unsaved-mode__changes_file@@20260614T174223.el --- highlight-unsaved-mode -*- lexical-binding: t -*-

;;; Commentary:
;; title: highlight-unsaved-mode
;; keywords: :changes:file:
;; date: [2026-06-14 Sun 17:42]
;; identifier: 20260614T174223

;; https://karthinks.com/software/even-more-batteries-included-with-emacs/#highlight-buffer-changes--m-x-highlight-changes-mode
;;; Code:
(require 'hilit-chg)
(defun highlight-changes-mode-turn-off ()
  (and highlight-changes-mode (highlight-changes-mode -1)))

(define-minor-mode highlight-unsaved-mode
  "Highlight all changes until the buffer is saved."
  :lighter "H"
  (cond
   ((not (buffer-file-name))
    (user-error "Highlight-until-save-mode is only meant for use in file-visiting buffers"))
   (highlight-unsaved-mode
    (highlight-changes-mode 1)
    (add-hook 'after-save-hook #'highlight-changes-mode-turn-on nil t)
    (add-hook 'before-save-hook #'highlight-changes-mode-turn-off nil t))
   (t (highlight-changes-mode -1)
      (remove-hook 'after-save-hook #'highlight-changes-mode-turn-on t)
      (remove-hook 'before-save-hook #'highlight-changes-mode-turn-off t))))

(add-hook 'text-mode-hook #'highlight-unsaved-mode)

(provide '--highlight-unsaved-mode__changes_file@@20260614T174223)
;;; --highlight-unsaved-mode__changes_file@@20260614T174223.el ends here
