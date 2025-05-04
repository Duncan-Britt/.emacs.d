;;; --world-clock__time@@20250503T040349.el --- world-clock -*- lexical-binding: t -*-

;;; Commentary:
;; title: world-clock
;; keywords: :time:
;; date: [2025-05-03 Sat 04:03]
;; identifier: 20250503T040349

;;; Code:
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

(provide '--world-clock__time@@20250503T040349)
;;; --world-clock__time@@20250503T040349.el ends here
