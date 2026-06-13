;;; --use-tmr__alarm_reminder_time_timer@@20260323T093119.el --- use-tmr -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-tmr
;; keywords: :alarm:reminder:time:timer:
;; date: [2026-03-23 Mon 09:31]
;; identifier: 20260323T093119

;;; Code:
(use-package tmr
  :ensure t
  :config
  ;; (setq tmr-sound-file "/System/Library/Sounds/Glass.aiff")
  (setq tmr-sound-file "/System/Library/Sounds/Ping.aiff")
  ;; (setq tmr-sound-file "/System/Library/Sounds/Purr.aiff")
  ;; (setq tmr-sound-file "/System/Library/Sounds/Submarine.aiff")
  ;; (setq tmr-sound-file "/System/Library/Sounds/Hero.aiff")

  ;; (defun my/tmr-notify-macos (timer)
  ;;   "Send a macOS notification for TIMER."
  ;;   (let ((title "TMR Timer Finished")
  ;;         (body (if (tmr--timer-description timer)
  ;;                   (tmr--timer-description timer)
  ;;                 "Timer has elapsed")))
  ;;     (call-process "osascript" nil 0 nil
  ;;                   "-e" (format "display dialog %S with title %S sound name \"Glass\""
  ;;                                body title))))
  ;; (setq tmr-timer-finished-functions
  ;;       (list #'my/tmr-notify-macos
  ;;             #'tmr-sound-play
  ;;             #'tmr-print-message-for-finished-timer))
  )

(call-process "osascript" nil 0 nil
                    "-e" (format "display dialog %S with title %S sound name \"Glass\""
                                 "Body" "Title"))

(provide '--use-tmr__alarm_reminder_time_timer@@20260323T093119)
;;; --use-tmr__alarm_reminder_time_timer@@20260323T093119.el ends here
