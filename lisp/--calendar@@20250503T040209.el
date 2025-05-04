;;; --calendar@@20250503T040209.el --- calendar -*- lexical-binding: t -*-

;;; Commentary:
;; title: calendar
;; keywords:
;; date: [2025-05-03 Sat 04:02]
;; identifier: 20250503T040209

;;; Code:
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

(provide '--calendar@@20250503T040209)
;;; --calendar@@20250503T040209.el ends here
