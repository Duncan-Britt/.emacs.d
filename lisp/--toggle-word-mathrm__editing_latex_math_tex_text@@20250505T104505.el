;;; --toggle-word-mathrm__editing_latex_math_tex_text@@20250505T104505.el --- toggle-word-mathrm -*- lexical-binding: t -*-

;;; Commentary:
;; title: toggle-word-mathrm
;; keywords: :editing:latex:math:tex:text:
;; date: [2025-05-05 Mon 10:45]
;; identifier: 20250505T104505

;;; Code:
(use-package tex-mode
  :ensure nil
  :config
  (defun toggle-word-mathrm ()
    "Toggle wrapping the word at point in \\mathrm{...}"
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
        (let* ((start (car bounds))
               (end (cdr bounds))
               (word (buffer-substring-no-properties start end))
               (mathrm-prefix "\\mathrm{")
               (mathrm-suffix "}")
               (prefix-len (length mathrm-prefix))
               (suffix-len (length mathrm-suffix)))
          ;; Check if previous chars are \mathrm{ and next chars are }
          (if (and (>= start prefix-len)
                   (>= (point-max) (+ end suffix-len))
                   (string= (buffer-substring-no-properties
                             (- start prefix-len) start)
                            mathrm-prefix)
                   (string= (buffer-substring-no-properties
                             end (+ end suffix-len))
                            mathrm-suffix))
              ;; Remove \mathrm{...}
              (progn
                (delete-region (- start prefix-len) (+ end suffix-len))
                (insert word))
            ;; Add \mathrm{...}
            (delete-region start end)
            (insert mathrm-prefix word mathrm-suffix))))))
  :bind (:map tex-mode-map
              ("C-c m" . toggle-word-mathrm)))

(provide '--toggle-word-mathrm__editing_latex_math_tex_text@@20250505T104505)
;;; --toggle-word-mathrm__editing_latex_math_tex_text@@20250505T104505.el ends here
