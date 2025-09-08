;;; --util__helper_utility@@20250505T154748.el --- util -*- lexical-binding: t -*-

;;; Commentary:
;; title: util
;; keywords: :helper:utility:
;; date: [2025-05-05 Mon 15:47]
;; identifier: 20250505T154748

;;; Code:
(defmacro with-eval-after-load-all (features &rest forms)
  "Eval FORMS after all FEATURES have been loaded."
  (declare (indent 1))
  (if (null features)
      `(progn ,@forms)
    `(with-eval-after-load ',(car features)
       (require ',(car features))
       (with-eval-after-load-all ,(cdr features) ,@forms))))

;; (macroexpand '(with-eval-after-load-all (ef-themes consult calle24 hl-todo)
;;                 (require 'ef-themes)
;;                 (require 'consult)
;;                 (require 'calle24)
;;                 (require 'hl-todo))) ;;=> (eval-after-load 'ef-themes (lambda nil (require 'ef-themes) (with-eval-after-load-all (consult calle24 hl-todo) (require ...) (require ...) (require ...) (require ...))))

(defun lookup (key assoc-list)
  "Utility function to loook up value by key in assoc list."
  (cdr (assoc key assoc-list)))

;; (lookup 'foo '((foo . bar))) ;;=> bar

(defun shuffle-vector! (v)
  (let ((n (length v)))
    (dotimes (i (1- n) v)
      (cl-rotatef (aref v i) (aref v (+ i (random (- n i))))))))

;; (shuffle '[1 2 3 4 5]) ;;=> [4 1 3 2 5]

(provide '--util__helper_utility@@20250505T154748)
;;; --util__helper_utility@@20250505T154748.el ends here
