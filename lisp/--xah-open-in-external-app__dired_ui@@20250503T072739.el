;;; --xah-open-in-external-app__dired_ui@@20250503T072739.el --- dired -*- lexical-binding: t -*-

;;; Commentary:
;; title: xah-open-in-external-app
;; keywords: :dired:ui:
;; date: [2025-05-03 Sat 07:27]
;; identifier: 20250503T072739

;;; Code:
(use-package dired
  :ensure nil
  :config
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

  (defun my-context-menu-item-for-xah-open-in-external-app (menu click)
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

  (add-hook 'context-menu-functions #'my-context-menu-item-for-xah-open-in-external-app))

(provide '--xah-open-in-external-app__dired_ui@@20250503T072739)
;;; --xah-open-in-external-app__dired_ui@@20250503T072739.el ends here
