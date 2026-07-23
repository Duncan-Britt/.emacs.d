;;; --filter-buffer__buffer_filter_navigation_regexp_search@@20260722T223140.el --- filter-buffer -*- lexical-binding: t -*-

;;; Commentary:
;; title: filter-buffer
;; keywords: :buffer:filter:navigation:regexp:search:
;; date: [2026-07-22 Wed 22:31]
;; identifier: 20260722T223140

;; ┌─────────────────────────────────────────────────────────────┐
;; │ This lets me filter the results of find-name-dired or occur │
;; │ buffers.                                                    │
;; └─────────────────────────────────────────────────────────────┘
;;; Code:
(defun dunc/filter-buffer (exclude-regexp)
  "Run `occur' with OCCUR-REGEXP, then flush lines matching EXCLUDE-REGEXP."
  (interactive "sExclude regexp: ")
  (let ((inhibit-read-only t))
    (flush-lines exclude-regexp)))

(provide '--filter-buffer__buffer_filter_navigation_regexp_search@@20260722T223140)
;;; --filter-buffer__buffer_filter_navigation_regexp_search@@20260722T223140.el ends here
