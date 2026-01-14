;;; --use-popper__navigation_ui_window@@20250505T105143.el --- use-popper -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-popper
;; keywords: :navigation:ui:window:
;; date: [2025-05-05 Mon 10:51]
;; identifier: 20250505T105143

;;; Code:
(use-package popper
  :ensure t
  :bind (("C-c C-;"   . popper-toggle)
         ("s-;"   . popper-cycle))
  :init
  (setq popper-reference-buffers
        '("Output\\*$"
          "\\*Async Shell Command\\*"
          ;; "\\*Org Agenda\\*"
          "\\*slime-repl sbcl\\*"
          "\\*slime-repl qlot\\*"
          "\\*slime-inspector\\*"
          "\\*Ciao\\*"
          "\\*sweeprolog-top-level\\*"
          "\\*prolog\\*"
          "\\*erlang\\*"
          "\\*Python\\*"
          sldb-mode
          "\\*Claude\\*"
          "\\*DeepSeek\\*"
          "\\*Gemini\\*"
          "\\*Ollama\\*"
	  "\\*ChatGPT\\*"
	  "\\*Warnings\\*"
	  "\\*compilation\\*"
	  "\\Backtrace\\*"
	  "\\*ruby\\*"
          "\\* Merriam-Webster Thesaurus \\*"
          ;; help-mode
          ;; compilation-mode
          ))
  (setq popper-group-function #'popper-group-by-project)

  (defun my/popper-group-function ()
    ""
    )
  (setq popper-group-function #'my/popper-group-function)

  (defun my/popper-display-popup (buffer &optional alist)
    "Display popup-buffer BUFFER based on the number of windows in the frame."
    (display-buffer-in-side-window
     buffer
     (append alist
             `((side . right)
               (window-width . ,(cond ((>= (frame-width) 80)
                                       80)
                                      (t
                                       (frame-width))))))))

  (setq popper-display-function #'my/popper-display-popup)

  (popper-mode +1)
  (popper-echo-mode +1))

(provide '--use-popper__navigation_ui_window@@20250505T105143)
;;; --use-popper__navigation_ui_window@@20250505T105143.el ends here
