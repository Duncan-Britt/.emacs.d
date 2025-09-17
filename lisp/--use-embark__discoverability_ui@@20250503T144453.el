;;; --use-embark__discoverability_ui@@20250503T144453.el --- use-embark -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-embark
;; keywords: :discoverability:ui:
;; date: [2025-05-03 Sat 14:44]
;; identifier: 20250503T144453

;;; Code:
(defun my/go-back-dwim ()
  "Try xref-go-back, fall back to pop-global-mark."
  (interactive)
  (condition-case nil
      (xref-go-back)
    (error (pop-global-mark))))

(use-package embark
  :ensure t
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-indicators '(embark-verbose-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))
  ;; (setq embark-prompter #'embark-completing-read-prompter)
  ;; ┌─────────────────────────────────────────────────────────────────┐
  ;; │ It would be really cool if I could make right clicks pull up an │
  ;; │ embark actions buffer where I can click on the actions. The     │
  ;; │ implementation below doesn't totally work correctly. It does    │
  ;; │ manage to pull up a buffer and the actions do take place, but   │
  ;; │ there seems to be some follow up that messes things up, and the │
  ;; │ actions buffer doesn't go away. Might try to make it work       │
  ;; │ someday.                                                        │
  ;; └─────────────────────────────────────────────────────────────────┘
  ;; (defun my/embark-mouse-prompter (keymap update)
  ;;   "Display actions in a buffer with mouse-clickable entries."
  ;;   (let* ((buffer (get-buffer-create "*Embark Actions*"))
  ;;          (actions (embark--formatted-bindings keymap))
  ;;          (candidates (car actions))
  ;;          (default (cdr actions))
  ;;          choice)

  ;;     (with-current-buffer buffer
  ;;       (erase-buffer)
  ;;       (setq-local cursor-type nil)
  ;;       (insert "Embark Actions (click or press key):\n\n")

  ;;       (dolist (candidate candidates)
  ;;         (let* ((formatted (car candidate))
  ;;                (cmd (caddr candidate))
  ;;                (key (key-description (cadddr candidate))))
  ;;           (insert (propertize key 'face 'embark-keybinding))
  ;;           (insert " ")
  ;;           (insert-text-button
  ;;            formatted
  ;;            'face 'default
  ;;            'mouse-face 'highlight
  ;;            'embark-command cmd
  ;;            'action (lambda (button)
  ;;                      (setq choice (button-get button 'embark-command))
  ;;                      (throw 'embark-done choice))
  ;;            'help-echo (format "Run: %s" cmd)))
  ;;         (insert "\n"))

  ;;       (display-buffer buffer)

  ;;       (unwind-protect
  ;;           (catch 'embark-done
  ;;             (let ((overriding-terminal-local-map keymap))
  ;;               (while (null choice)
  ;;                 (let ((event (read-event "Select action: ")))
  ;;                   (cond
  ;;                    ((mouse-event-p event)
  ;;                     (mouse-set-point event)
  ;;                     (let ((button (button-at (point))))
  ;;                       (when button
  ;;                         (button-activate button))))
  ;;                    (t
  ;;                     (setq unread-command-events (list event))
  ;;                     (setq choice (key-binding (read-key-sequence-vector nil)))))))
  ;;               (when (buffer-live-p buffer)
  ;;                 (kill-buffer buffer))
  ;;               choice)
  ;;             (when (buffer-live-p buffer)
  ;;               (kill-buffer buffer)))))))

  ;; (setq embark-prompter #'my/embark-mouse-prompter)
  ;; (global-set-key (kbd "<mouse-3>") 'my/embark-act-on-mouse-target)

  (defun my/embark-dwim-mouse-target (event)
    "Perform default embark action on thing at mouse."
    (interactive "e")
    (let* ((posn (event-start event))
           (window (posn-window posn))
           (pos (posn-point posn)))
      (when (and window (windowp window) pos)
        (select-window window)
        (goto-char pos)
        (call-interactively 'embark-dwim))))

  (defun my/embark-act-on-mouse-target (event)
    "Call embark-act but with the clicked thing as the target."
    (interactive "e")
    (let* ((posn (event-start event))
           (window (posn-window posn))
           (pos (posn-point posn)))
      (when (and window (windowp window) pos)
        (select-window window)
        (goto-char pos)
        (call-interactively 'embark-act))))

  :bind
  (("C-<return>"    . embark-act)
   ("M-RET"         . embark-dwim)
   ("C-h B"         . embark-bindings)
   ("s-<mouse-1>"   . my/embark-dwim-mouse-target)
   ("S-s-<mouse-1>" . my/go-back-dwim))
  :hook (org-mode . (lambda () (local-set-key (kbd "C-<return>") #'embark-act))))



(provide '--use-embark__discoverability_ui@@20250503T144453)
;;; --use-embark__discoverability_ui@@20250503T144453.el ends here
