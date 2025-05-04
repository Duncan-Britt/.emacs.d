;;; --dired-toggle-marked__dired_mouse@@20250503T085927.el --- dired-toggle-marked -*- lexical-binding: t -*-

;;; Commentary:
;; title: dired-toggle-marked
;; keywords: :dired:mouse:
;; date: [2025-05-03 Sat 08:59]
;; identifier: 20250503T085927

;; ┌─────────────────────────────────────────┐
;; │ Command click to toggle marked in Dired │
;; └─────────────────────────────────────────┘
;;; Code:
(use-package dired
  :ensure nil
  :config
  (defun my/dired-toggle-marked (event)
    "Toggle marked in dired on click EVENT."
    (interactive "e")
    (let ((marked-files (dired-get-marked-files))
          (file-name (get-filename-at-mouse-click event))
          (window (posn-window (event-end event)))
          (pos (posn-point (event-end event))))
      ;; (print marked-files)
      ;; (print file-name)
      (with-current-buffer (window-buffer window)
        (when (eq major-mode 'dired-mode)
          (save-excursion
            (goto-char pos)
            (if (member file-name marked-files)
                (dired-unmark nil)
              (dired-mark nil)))))))
  :bind ((:map dired-mode-map
               ("s-<mouse-1>" . my/dired-toggle-marked))))

(provide '--dired-toggle-marked__dired_mouse@@20250503T085927)
;;; --dired-toggle-marked__dired_mouse@@20250503T085927.el ends here
