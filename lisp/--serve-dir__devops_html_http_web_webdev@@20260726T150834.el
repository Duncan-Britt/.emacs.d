;;; --serve-dir__devops_html_http_web_webdev@@20260726T150834.el --- serve-dir -*- lexical-binding: t -*-

;;; Commentary:
;; title: serve-dir
;; keywords: :devops:html:http:web:webdev:
;; date: [2026-07-26 Sun 15:08]
;; identifier: 20260726T150834

;;; Code:
(defun dunc/serve-dir (dir &optional port)
  "Serve contents of DIR over http using Python's built-in server.
With prefix argument, prompt for PORT (default 8000)."
  (interactive
   (list (read-directory-name "Serve directory: ")
         (if current-prefix-arg
             (read-number "Port: " 8000)
           8000)))
  (let* ((port (or port 8000))
         (default-directory (expand-file-name dir))
         (buffer-name (format "*serve-dir:%d*" port)))
    (if (get-buffer-process buffer-name)
        (message "Server already running in %s" buffer-name)
      (let ((proc (start-process "serve-dir" buffer-name
                                 "python3" "-m" "http.server"
                                 (number-to-string port))))
        (set-process-query-on-exit-flag proc nil)
        (message "Serving %s at http://localhost:%d/ (buffer %s)"
                 default-directory port buffer-name)))))

(defun dunc/serve-dir-stop (&optional port)
  "Stop the serve-dir server on PORT (default 8000)."
  (interactive (list (if current-prefix-arg (read-number "Port: " 8000) 8000)))
  (let ((buf (format "*serve-dir:%d*" (or port 8000))))
    (when-let ((proc (get-buffer-process buf)))
      (kill-process proc)
      (message "Stopped server on port %d" port))))

(provide '--serve-dir__devops_html_http_web_webdev@@20260726T150834)
;;; --serve-dir__devops_html_http_web_webdev@@20260726T150834.el ends here
