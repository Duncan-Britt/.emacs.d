;;; --mode-line__appearance@@20250502T210916.el --- my-mode-line -*- lexical-binding: t -*-

;;; Commentary:
;; title: mode-line
;; keywords: :appearance:
;; date: [2025-05-02 Fri 21:09]
;; identifier: 20250502T210916

;;; Code:
(use-package emacs
  :ensure nil
  :config
  (defvar-local my/modeline-major-mode
      '(:eval
        (when (mode-line-window-selected-p)
          (propertize (format " %s " (capitalize (symbol-name major-mode)))
                      'face 'mode-line-active
                      'mouse-face 'mode-line-highlight
                      'help-echo "Describe mode"
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1] 'describe-mode)
                                   map))))
    "Mode line construct to display the major mode.")

  (defvar prot-modeline-vc-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1] 'vc-diff)
      (define-key map [mode-line down-mouse-3] 'vc-root-diff)
      map)
    "Keymap to display on VC indicator.")

  (defun prot-modeline--vc-text (file branch &optional face)
    "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
    (concat
     (propertize (char-to-string #xE0A0) 'face 'shadow)
     " "
     (propertize branch
                 'face face
                 'mouse-face 'mode-line-highlight
                 'help-echo (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
                                    (vc-working-revision file))
                 'local-map prot-modeline-vc-map)
     "  "))

  (defun prot-modeline--vc-details (file branch &optional face)
    "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
    (prot-modeline--vc-text file branch face))

  (defvar prot-modeline--vc-faces
    '((added . vc-locally-added-state)
      (edited . vc-edited-state)
      (removed . vc-removed-state)
      (missing . vc-missing-state)
      (conflict . vc-conflict-state)
      (locked . vc-locked-state)
      (up-to-date . vc-up-to-date-state))
    "VC state faces.")

  (defun prot-modeline--vc-get-face (key)
    "Get face from KEY in `prot-modeline--vc-faces'."
    (alist-get key prot-modeline--vc-faces 'up-to-date))

  (defun prot-modeline--vc-face (file backend)
    "Return VC state face for FILE with BACKEND."
    (prot-modeline--vc-get-face (vc-state file backend)))

  (defun prot-modeline--vc-branch-name (file backend)
    "Return capitalized VC branch name for FILE with BACKEND."
    (when-let ((rev (vc-working-revision file backend))
               (branch (or (vc-git--symbolic-ref file)
                           (substring rev 0 7))))
      (capitalize branch)))

  (defvar-local prot-modeline-vc-branch
      '(:eval
        (when-let* (((mode-line-window-selected-p))
                    (file (buffer-file-name))
                    (backend (vc-backend file))
                    (branch (prot-modeline--vc-branch-name file backend))
                    (face (prot-modeline--vc-face file backend)))
          (prot-modeline--vc-details file branch face)))
    "Mode line construct to return propertized VC branch.")

  (defvar-local my/modeline-kbd-macro
      '(:eval
        (when (and (mode-line-window-selected-p) defining-kbd-macro)
          (list
           (propertize " KMacro " 'face 'ansi-color-inverse)
           " ")))
    "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

  (defvar-local my/modeline-meow-indicator
      '(:eval
        (when (and (bound-and-true-p meow-mode) (mode-line-window-selected-p))
          (let* ((indicator (meow-indicator))
                 (mode-name (substring-no-properties indicator))
                 (padded-name (format " %-7s" (string-trim mode-name))))
            (list
             (cond
              ((string-equal (string-trim mode-name) "INSERT")
               (propertize padded-name 'face 'ansi-color-inverse))
              ((string-equal (string-trim mode-name) "NORMAL")
               (propertize padded-name 'face 'ansi-color-inverse))
              (t
               (propertize padded-name 'face 'ansi-color-inverse)))
             " "))))
    "Mode line construct to display meow mode, e.g. INSERT, NORMAL, etc.")

  (defvar-local my/modeline-input-method
      '(:eval
        (when current-input-method-title
          (propertize (format " %s " current-input-method-title)
                      'face 'ansi-color-inverse
                      'help-echo (format "Current input method: %s" current-input-method))))
    "Mode line construct to report the multilingual environment.")

  (defvar-local my/modeline-time
      '(:eval
        (when (mode-line-window-selected-p)
          (propertize (format-time-string "%I:%M %p ")
                      'face 'mode-line-active
                      'mouse-face 'mode-line-highlight
                      'help-echo "Show world clock"
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1] 'world-clock)
                                   (define-key map [header-line mouse-1] 'world-clock)
                                   map))))
    "Mode line construct to display the current time with a world-clock on click.")

  (defvar-local my/modeline-date
      '(:eval
        (when (mode-line-window-selected-p)
          (propertize (format-time-string "%a, %b %-d ")
                      'face 'mode-line-active
                      'mouse-face 'mode-line-highlight
                      'help-echo (format-time-string "%A, %B %-d. Show calendar")
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1] 'calendar)
                                   (define-key map [header-line mouse-1] 'calendar)
                                   map))))
    "Mode line construct to display the current date with calendar on click.")


  (display-battery-mode 1)
  (defun my-battery-update-advice (orig-fun &rest args)
    "Add custom face to battery indicator when battery is normal."
    (let ((result (apply orig-fun args)))
      (when battery-mode-line-string
        (let ((len (length battery-mode-line-string))
              (percentage (and battery-status-function
                               (car (read-from-string
                                     (cdr (assq ?p (funcall battery-status-function))))))))
          (when (and (numberp percentage)
                     (>= percentage battery-load-low))
            (add-face-text-property 0 len 'mode-line-active t battery-mode-line-string))))
      result))
  (advice-add 'battery-update :around #'my-battery-update-advice)
  (setq battery-mode-line-format "【%b%p%%】")
  (defvar-local my/modeline-battery
      '(:eval
        (when (mode-line-window-selected-p)
          'battery-mode-line-string)))

  (defvar-local prot-modeline-eglot
      `(:eval
        (when (and (featurep 'eglot) (mode-line-window-selected-p))
          (list '(eglot--managed-mode eglot--mode-line-format)
                " ")))
    "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

  (defvar-local my/modeline-modified
      '(:eval
        (when (and (buffer-file-name) (buffer-modified-p))
          (propertize "●  " 'face 'warning)))
    "mode line construct to indicate unsaved changes.")

  (defvar-local my/modeline-remote-indicator
      '(:eval
        (when (and (mode-line-window-selected-p)
                   (or (and buffer-file-name
                            (file-remote-p buffer-file-name))
                       (and default-directory
                            (file-remote-p default-directory))))
          (let* ((remote-file (or buffer-file-name default-directory))
                 (method (file-remote-p remote-file 'method))
                 (host (file-remote-p remote-file 'host)))
            (list
             (propertize (format " %s " (upcase method))
                         'face 'ansi-color-inverse
                         'help-echo (format "Remote file via %s to %s" method host))
             " "))))
    "Mode line construct to indicate remote files with the method used.")

  (defvar my/modeline-max-filename-length 40
    "Maximum length of filename to display in mode line before truncation.")

  (defvar-local my/modeline-buffer-identification
      '(:eval
        (let* ((name (if (buffer-file-name)
                         (file-name-nondirectory (buffer-file-name))
                       (buffer-name)))
               (truncated (if (and (buffer-file-name)
                                   (> (length name) my/modeline-max-filename-length))
                              (concat (substring name 0 (- my/modeline-max-filename-length 3)) "...")
                            name)))
          (propertize (format " %s " truncated)
                      'face 'mode-line-buffer-id
                      'mouse-face 'mode-line-highlight
                      'help-echo (buffer-file-name)
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1] 'dired-jump)
                                   (define-key map [header-line mouse-1] 'dired-jump)
                                   map))))
    "Mode line construct for buffer identification with truncated filenames.")

  (defun toggle-display-line-numbers ()
    (interactive)
    (if display-line-numbers-mode
        (display-line-numbers-mode -1)
      (display-line-numbers-mode 1)))

  (defvar-local my/modeline-current-line-number
      '(:eval
        (when (mode-line-window-selected-p)
          (propertize (format "%s" (line-number-at-pos))
                      'face 'mode-line-active
                      'mouse-face 'mode-line-highlight
                      'help-echo "Toggle line numbers"
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1] 'toggle-display-line-numbers)
                                   (define-key map [header-line mouse-1] 'toggle-display-line-numbers)
                                   map))))
    "Mode line construct to display the line number at point.")
  ;; NOTE: The following is necessary for my/modelin-current-line-number
  ;; because, with a single window only (not split window), the
  ;; current line number was not being updated when I moved the cursor
  ;; up and down lines.
  (add-hook 'post-command-hook #'force-mode-line-update)

  (dolist (construct '(my/modeline-buffer-identification
                       my/modeline-major-mode
                       prot-modeline-vc-branch
                       my/modeline-kbd-macro
                       my/modeline-meow-indicator
                       my/modeline-input-method
                       my/modeline-time
                       my/modeline-date
                       my/modeline-battery
                       my/modeline-modified
                       my/modeline-remote-indicator
                       prot-modeline-eglot
                       my/modeline-current-line-number))
    (put construct 'risky-local-variable t))

  (setq which-func-format
        '("〘"
          (:propertize which-func-current local-map
                       (keymap
                        (mode-line keymap
                                   (mouse-3 . end-of-defun)
                                   (mouse-2 . (lambda () (interactive) (which-func-mode -1) (which-func-mode 1)))
                                   (mouse-1 . beginning-of-defun)))
                       face which-func mouse-face mode-line-highlight help-echo "Current function
mouse-1: go to beginning
mouse-2: toggle rest visibility
mouse-3: go to end")
          "〙"))
  (which-function-mode -1)

  (setq-default mode-line-format
                '("%e"
                  "  "
                  my/modeline-meow-indicator
                  my/modeline-kbd-macro
                  my/modeline-remote-indicator
                  my/modeline-buffer-identification
                  my/modeline-current-line-number
                  " "
                  prot-modeline-vc-branch
                  my/modeline-input-method
                  prot-modeline-eglot
                  my/modeline-major-mode
                  (which-function-mode which-func-format)
                                        ; mode-line-format-right-align TODO uncomment when I move to Emacs 30:
                  my/modeline-battery
                  my/modeline-time
                  my/modeline-date
                  my/modeline-modified
                  )))

(provide '--mode-line__appearance@@20250502T210916)
;;; --mode-line__appearance@@20250502T210916.el ends here
