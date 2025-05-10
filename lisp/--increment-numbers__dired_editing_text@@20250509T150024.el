;;; --increment-numbers__dired_editing_text@@20250509T150024.el --- increment-numbers -*- lexical-binding: t -*-

;;; Commentary:
;; title: increment-numbers
;; keywords: :dired:editing:text:
;; date: [2025-05-09 Fri 15:00]
;; identifier: 20250509T150024

;;; Code:
(defun my/increment-numbers-in-region (start end &optional n)
  "Increment all numbers in the selected region by N (default 1)."
  (interactive "r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]+\\)" nil t)
        (replace-match
         (let* ((num-str (match-string 1))
                (padding (length num-str))
                (num (string-to-number num-str))
                (new-num (+ num n)))
           (format (format "%%0%dd" padding) new-num)))))))

(defun my/increment-numbers-in-rectangle (start end &optional n)
  "Increment all numbers in the selected rectangle by N (default 1).

Useful in Dired to rename many files, e.g
002.jpg => 003.jpg
003.jpg => 004.jpg"
  (interactive "r\np")
  (let ((n (or n 1)))
    (apply-on-rectangle
     (lambda (startcol endcol)
       (let ((line-start (progn (move-to-column startcol t) (point)))
             (line-end (progn (move-to-column endcol t) (point))))
         (save-excursion
           (let ((text (buffer-substring line-start line-end)))
             (delete-region line-start line-end)
             (goto-char line-start)
             (insert (replace-regexp-in-string
                      "\\([0-9]+\\)"
                      (lambda (match)
                        (let* ((num-str (match-string 1 match))
                               (padding (length num-str))
                               (num (string-to-number num-str))
                               (new-num (+ num n)))
                          (format (format "%%0%dd" padding) new-num)))
                      text))))))
     start end)))

(provide '--increment-numbers__dired_editing_text@@20250509T150024)
;;; --increment-numbers__dired_editing_text@@20250509T150024.el ends here
