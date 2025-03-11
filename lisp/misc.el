;;; misc.el --- Don't know where else to put these. -*- lexical-binding: t; -*-

;;; Commentary:
;; ┌───────────────┐
;; │ Miscellaneous │
;; └───────────────┘

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq large-file-warning-threshold 30000000)
  (save-place-mode 1))

(use-package transient
  :ensure t)

(use-package time
  :ensure nil
  :config
  (setq world-clock-list
        '(("America/Denver" "Denver")
          ("Asia/Saigon" "Saigon")
          ("America/New_York" "Princeton")))
  (setq world-clock-time-format "%t%A %B %_d%n%l:%M %p%n===============================")
  ;; C-h f format-time-string
  ;; %d <- day of month, e.g. `03'
  ;; %A <- week day, e.g. `Monday'
  ;; %B <- month, e.g. `March'
  ;; %R <- military time, e.g. `22:35'
  ;; %Z <- time zone, e.g. `+07' or `EST'
  ;; %I <- 12 hour clock hour, %l is blank padded version
  )

(use-package calendar
  :ensure nil
  :config
  (defun calendar-insert-date ()
    "Capture the date at point, exit the Calendar, insert the date."
    (interactive)
    (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
      (calendar-exit)
      (insert (format "<%d-%02d-%02d>" year month day))))
  (define-key calendar-mode-map (kbd "RET") 'calendar-insert-date))

(use-package-when-local
 directory-slideshow
 "~/code/my-emacs-packages/directory-slideshow/")

(use-package djvu
  :ensure t)

(provide 'misc)
