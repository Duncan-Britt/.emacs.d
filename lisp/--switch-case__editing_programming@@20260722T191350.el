;;; --switch-case__editing_programming@@20260722T191350.el --- toggle-case -*- lexical-binding: t -*-

;;; Commentary:
;; title: switch-case
;; keywords: :editing:programming:
;; date: [2026-07-22 Wed 19:13]
;; identifier: 20260722T191350

;;; Code:

(defun dunc/switch-case--split-words (str)
  "Split STR into a list of downcased words."
  (let ((case-fold-search nil))
    (mapcar #'downcase
            (split-string
             (replace-regexp-in-string
              "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1 \\2"
              (replace-regexp-in-string
               "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1 \\2" str))
             "[_ -]+" t))))

;; (dunc/switch-case--split-words "hello_world")
;;=> ("hello" "world")
;; (dunc/switch-case--split-words "helloWorld")
;;=> ("hello" "world")
;; (dunc/switch-case--split-words "HelloWorld")
;;=> ("hello" "world")
;; (dunc/switch-case--split-words "hello-world")
;;=> ("hello" "world")
;; (dunc/switch-case--split-words "HELLO_CRUEL0_WORLD")
;;=> ("hello" "cruel0" "world")

(defun dunc/switch-case (style)
  "Convert the symbol at point to STYLE.
STYLE is one of snake_case, camelCase, PascalCase, kebab-case,
or SCREAMING_SNAKE_CASE."
  (interactive
   (list (completing-read
          "Case: "
          '("snake_case" "camelCase" "PascalCase"
            "kebab-case" "SCREAMING_SNAKE_CASE")
          nil t)))
  (if-let* ((bounds (bounds-of-thing-at-point 'symbol)))
      (let* ((words (dunc/switch-case--split-words
                     (buffer-substring-no-properties (car bounds) (cdr bounds))))
             (new (pcase style
                    ("snake_case" (string-join words "_"))
                    ("camelCase" (concat (car words)
                                         (mapconcat #'capitalize (cdr words) "")))
                    ("PascalCase" (mapconcat #'capitalize words ""))
                    ("kebab-case" (string-join words "-"))
                    ("SCREAMING_SNAKE_CASE" (string-join (mapcar #'upcase words) "_")))))
        (delete-region (car bounds) (cdr bounds))
        (goto-char (car bounds))
        (insert new))
    (user-error "No symbol at point")))

(provide '--switch-case__editing_programming@@20260722T191350)
;;; --switch-case__editing_programming@@20260722T191350.el ends here
