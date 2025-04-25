;;; misc.el --- Don't know where else to put these. -*- lexical-binding: t; -*-

;;; Commentary:
;; ┌───────────────┐
;; │ Miscellaneous │
;; └───────────────┘

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq large-file-warning-threshold 30000000)
  (save-place-mode 1))

(use-package transient
  :ensure t)

(use-package time
  :ensure nil
  :config
  (setq world-clock-list
        '(("America/Denver" "Denver")
          ("Asia/Saigon" "Saigon")
          ("America/New_York" "Princeton")))
  (setq world-clock-time-format "%t%A %B %_d%n%l:%M %p%n===============================")
  ;; C-h f format-time-string
  ;; %d <- day of month, e.g. `03'
  ;; %A <- week day, e.g. `Monday'
  ;; %B <- month, e.g. `March'
  ;; %R <- military time, e.g. `22:35'
  ;; %Z <- time zone, e.g. `+07' or `EST'
  ;; %I <- 12 hour clock hour, %l is blank padded version
  )

(use-package calendar
  :ensure nil
  :config
  (defun calendar-insert-date ()
    "Capture the date at point, exit the Calendar, insert the date."
    (interactive)
    (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
      (calendar-exit)
      (insert (format "<%d-%02d-%02d>" year month day))))
  (define-key calendar-mode-map (kbd "RET") 'calendar-insert-date))

(use-package-when-local
 directory-slideshow
 "~/code/my-emacs-packages/directory-slideshow/")

(use-package djvu
  :ensure t)

(use-package comint
  :ensure nil
  :bind (("s-r" . (lambda ()
                    (interactive)
                    (let ((current-prefix-arg '(4)))
                      (call-interactively 'comint-run))))))

(use-package dired
  :ensure nil
  :config
  (global-unset-key (kbd "C-x C-d")) ;; default is `list-directory'.

  (defun xah-open-in-external-app (&optional Fname)
    "Open the current file or dired marked files in external app.
When called in emacs lisp, if Fname is given, open that.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Created: 2019-11-04
Version: 2025-04-18"
    (interactive)
    (let (xfileList xdoIt)
      (setq xfileList
            (if Fname
                (list Fname)
              (if (eq major-mode 'dired-mode)
                  (dired-get-marked-files)
                (list buffer-file-name))))
      (setq xdoIt (if (<= (length xfileList) 10) t (y-or-n-p "Open more than 10 files? ")))
      (when xdoIt
        (cond
         ((eq system-type 'windows-nt)
          (let ((xoutBuf (get-buffer-create "*xah open in external app*"))
                (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
            (mapc
             (lambda (x)
               (apply 'start-process (append (list "xah open in external app" xoutBuf) xcmdlist (list (format "'%s'" (string-replace "'" "`'" x))) nil)))
             xfileList)))
         ((eq system-type 'darwin)
          (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
         ((eq system-type 'gnu/linux)
          (mapc (lambda (xfpath)
                  (call-process shell-file-name nil 0 nil
                                shell-command-switch
                                (format "%s %s"
                                        "xdg-open"
                                        (shell-quote-argument xfpath))))
                xfileList))
         ((eq system-type 'berkeley-unix)
          (mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList))))))

  (defun get-filename-at-mouse-click (event)
    "Get filename at mouse click position in dired."
    (let* ((window (posn-window (event-end event)))
           (pos (posn-point (event-end event)))
           (xy (posn-x-y (event-end event)))
           (x (car xy))
           (y (cdr xy)))
      (with-current-buffer (window-buffer window)
        (when (eq major-mode 'dired-mode)
          (save-excursion
            (goto-char pos)
            (let* ((filename (dired-get-filename nil t))
                   (bol (line-beginning-position))
                   (eol (line-end-position))
                   (line-start-x (car (posn-x-y (posn-at-point bol))))
                   (filename-pos (and filename
                                      (string-match (regexp-quote (file-name-nondirectory filename))
                                                    (buffer-substring bol eol))
                                      (+ bol (match-beginning 0))))
                   (filename-end (and filename-pos (+ filename-pos (length (file-name-nondirectory filename)))))
                   (filename-x-start (and filename-pos (car (posn-x-y (posn-at-point filename-pos)))))
                   (filename-x-end (and filename-end (car (posn-x-y (posn-at-point filename-end))))))
              (when (and filename-x-start filename-x-end
                         (<= filename-x-start x)
                         (<= x filename-x-end))
                filename)))))))

  (defun my-dired-context-menu-item (menu click)
    "Add a top-level 'Open in external app' item to Dired's context MENU at CLICK position.
The Open in external app context menue button comes up when
either some file is clicked on, some file(s) are marked in dired,
or there is a file at point. It opens the clicked file if a file
was clicked on, or the marked file(s) otherwise."
    (if (and (eq major-mode 'dired-mode)
             (or (mouse-posn-property (event-start click) 'dired-filename)
                 (dired-get-marked-files)))
        (if-let (clicked-file-name (get-filename-at-mouse-click click))
            (progn
              (define-key-after menu [my-dired-action]
                `(menu-item "Open in external app" (lambda () (interactive) (xah-open-in-external-app ,clicked-file-name))
                            :help "Open marked or clicked file(s) in external app"))
              menu)
          (define-key-after menu [my-dired-action]
            '(menu-item "Open in external app" xah-open-in-external-app
                        :help "Open marked or clicked file(s) in external app"))
          menu)
      menu))

  (add-hook 'context-menu-functions #'my-dired-context-menu-item)

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

(provide 'misc)
