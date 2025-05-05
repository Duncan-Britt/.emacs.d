;;; --box-comment__programming_text@@20250505T095430.el --- box-comment -*- lexical-binding: t -*- ;;=> 1

;;; Commentary:
;; title: box-comment
;; keywords: :programming:text:
;; date: [2025-05-05 Mon 09:54]
;; identifier: 20250505T095430

;; ┌─────────┐
;; │ Like so │
;; └─────────┘
;;; Code:
(defun insert-box-comment (text)
  "Create a box comment with TEXT inside using the current mode's comment syntax."
  (interactive "sEnter text for box: ")
  (let* ((raw-comment (if comment-start comment-start "// "))
         (comment-marker
          (cond
           ((derived-mode-p 'emacs-lisp-mode) ";; ")
           ((derived-mode-p 'prolog-mode 'erlang-mode) "%% ")
           ;; Single-character comment markers that should be doubled
           ((and (= (length (string-trim raw-comment)) 1)
                 (not (string-match-p " $" raw-comment)))
            (concat raw-comment raw-comment " "))
           (t (if (string-match-p " $" raw-comment)
                  raw-comment
                (concat raw-comment " ")))))
         (text-width (length text))
         (box-width (+ text-width 2))
         (top-line (concat comment-marker "┌" (make-string box-width ?─) "┐"))
         (mid-line (concat comment-marker "│ " text " │"))
         (bot-line (concat comment-marker "└" (make-string box-width ?─) "┘")))
    (let* ((bol (save-excursion
                  (beginning-of-line)
                  (point)))
           (indent-str (buffer-substring-no-properties bol (point))))
      (if (string-match-p "\\`[ ]*\\'" indent-str)
          (insert top-line "\n" indent-str mid-line "\n" indent-str bot-line "\n")
        (insert-rectangle (list top-line mid-line bot-line))))))

(provide '--box-comment__programming_text@@20250505T095430)

;;; --box-comment__programming_text@@20250505T095430.el ends here
