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
(defun my/comment-marker (raw-comment)
  "Get the appropriate comment marker for the current buffer."
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

(defun my/insert-box-comment (text)
  "Create a box comment with TEXT inside using the current mode's comment syntax."
  (interactive "sEnter text for box: ")
  (let* ((raw-comment (if comment-start comment-start "// "))
         (comment-marker (my/comment-marker raw-comment))
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

(defun my/box-rect ()
  "Put the region in a box."
  (interactive)
  (when (region-active-p)
    (let* ((lines (delete-extract-rectangle (region-beginning) (region-end)))
           (max-line-width (cl-reduce (lambda (acc str)
                                        (max acc
                                             (length str)))
                                      lines
                                      :initial-value 0))
           (box-width (+ max-line-width 2))
           (comment-marker (my/comment-marker))
           (top-line (concat comment-marker "┌" (make-string box-width ?─) "┐"))
           (mid-lines (mapcar (lambda (text)
                                (concat comment-marker "│ " text " │"))
                              lines))
           (bot-line (concat comment-marker "└" (make-string box-width ?─) "┘")))
      (insert-rectangle `(,top-line ,@mid-lines ,bot-line)))))

(provide '--box-comment__programming_text@@20250505T095430)

;;; --box-comment__programming_text@@20250505T095430.el ends here
