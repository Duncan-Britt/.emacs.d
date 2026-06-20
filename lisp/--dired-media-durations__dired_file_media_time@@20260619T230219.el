;;; --dired-media-durations__dired_file_media_time@@20260619T230219.el --- dired-media-durations -*- lexical-binding: t -*-

;;; Commentary:
;; title: dired-media-durations
;; keywords: :dired:file:media:time:
;; date: [2026-06-19 Fri 23:02]
;; identifier: 20260619T230219

;; Requires: ffprobe (part of ffmpeg) to be on PATH.
;; Entry points:
;;   my/dired-sum-durations-dwim    — marked files if any, else all visible
;;   my/dired-sum-durations-buffer  — all visible files (respects dired-narrow)
;;   my/dired-sum-durations-marked  — marked files (falls back to file at point)

;;; Code:
(require 'cl-lib)
(require 'dired)

;;;; ── Helpers ────────────────────────────────────────────────────────────────

(defun my/dired-get-file-duration (file)
  "Return the duration of FILE in seconds (a float) via ffprobe.
Returns nil when FILE is not a regular file, has no detectable duration,
or ffprobe returns a non-zero exit code."
  (when (file-regular-p file)
    (with-temp-buffer
      ;; Stdout → current buffer; stderr → /dev/null (the nil slot).
      (when (zerop (call-process "ffprobe" nil '(t nil) nil
                                 "-v" "error"
                                 "-show_entries" "format=duration"
                                 "-of" "default=noprint_wrappers=1:nokey=1"
                                 file))
        (let* ((raw (string-trim (buffer-string)))
               (n   (and (not (string-empty-p raw))
                         (string-to-number raw))))
          (and n (> n 0) n))))))

(defun my/dired-format-duration (seconds)
  "Format SECONDS as a compact, human-readable string.
Examples:
  45          → \"45s\"
  185         → \"3m 05s\"
  5025        → \"1h 23m 45s\"
  90061       → \"1d 1h 01m 01s\""
  (let* ((total (round seconds))
         (s     (mod total 60))
         (tm    (/ total 60))
         (m     (mod tm 60))
         (th    (/ tm 60))
         (h     (mod th 24))
         (d     (/ th 24)))
    (cond
     ((> d 0) (format "%dd %dh %02dm %02ds" d h m s))
     ((> h 0) (format "%dh %02dm %02ds" h m s))
     ((> m 0) (format "%dm %02ds" m s))
     (t       (format "%ds" s)))))

(defun my/dired-get-visible-files ()
  "Return absolute paths of every file on a visible line in the dired buffer.
Lines hidden by `dired-narrow' (or any other means) are skipped by checking
`invisible-p' at the start of each line."
  (let (files)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (unless (invisible-p (point))
          (when-let ((f (dired-get-filename nil t)))
            (push f files)))
        (forward-line 1)))
    (nreverse files)))

(defun my/dired--any-marked-p ()
  "Return non-nil if at least one file is marked in the current dired buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (dired-marker-regexp) nil t)))

;;;; ── Core worker ────────────────────────────────────────────────────────────

(defun my/dired--sum-durations (files label)
  "Sum durations of FILES and display the result prefixed by LABEL.
FILES is a list of absolute paths.  Files that ffprobe cannot read (e.g.
plain text, images) are silently counted as skipped and reported at the end.
A progress reporter is shown while ffprobe runs."
  (unless (executable-find "ffprobe")
    (user-error "ffprobe not found — please install ffmpeg"))
  (let* ((n        (length files))
         (reporter (make-progress-reporter
                    (format "Reading durations (%d file%s)…"
                            n (if (= n 1) "" "s"))
                    0 n))
         (total   0.0)
         (counted 0)
         (skipped 0))
    (cl-loop for file in files
             for i from 1 do
             (progress-reporter-update reporter i)
             (if-let ((dur (my/dired-get-file-duration file)))
                 (progn (cl-incf total dur)
                        (cl-incf counted))
               (cl-incf skipped)))
    (progress-reporter-done reporter)
    (message "%s — %s  (%d media file%s%s)"
             label
             (my/dired-format-duration total)
             counted
             (if (= counted 1) "" "s")
             (if (> skipped 0)
                 (format ", %d skipped" skipped)
               ""))))

;;;; ── Interactive commands ───────────────────────────────────────────────────

;;;###autoload
(defun my/dired-sum-durations-buffer ()
  "Sum and display total duration of all *visible* files in the dired buffer.
Respects `dired-narrow': only files whose lines are currently visible are
included, so narrowing before calling this command filters the result."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  (let ((files (my/dired-get-visible-files)))
    (if files
        (my/dired--sum-durations files "Visible files")
      (message "No visible files in buffer"))))

;;;###autoload
(defun my/dired-sum-durations-marked ()
  "Sum and display total duration of marked files in the dired buffer.
Falls back to the file at point when no files are marked, following the
standard dired convention."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  (let* ((marked-p (my/dired--any-marked-p))
         (files    (dired-get-marked-files))
         (label    (if marked-p
                       (format "Marked files (%d)" (length files))
                     "File at point")))
    (if files
        (my/dired--sum-durations files label)
      (message "No files selected"))))

;;;###autoload
(defun my/dired-sum-durations-dwim ()
  "Sum and display total duration of media files — DWIM style.
If any files are marked, operates on those (calls
`my/dired-sum-durations-marked').  Otherwise operates on every visible
file in the buffer, respecting `dired-narrow' (calls
`my/dired-sum-durations-buffer')."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  (if (my/dired--any-marked-p)
      (my/dired-sum-durations-marked)
    (my/dired-sum-durations-buffer)))

(provide '--dired-media-durations__dired_file_media_time@@20260619T230219)
;;; --dired-media-durations__dired_file_media_time@@20260619T230219.el ends here
