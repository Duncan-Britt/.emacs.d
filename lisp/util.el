;;; util.el --- Some utilities

;;; Commentary:

;;; Code:



;; ┌────────┐
;; │ Unused │
;; └────────┘

;; (defun my/is-text-file-using-file-cmd (filename)
;;     "Use external 'file' command to detect if FILENAME is a text file."
;;     (when (and filename
;;                (file-exists-p filename)
;;                (file-readable-p filename))
;;       (if-let ((file-cmd (executable-find "file")))
;;           (with-temp-buffer
;;             (when (zerop (call-process file-cmd nil t nil "--brief" "--mime-type"
;;                                        (expand-file-name filename)))
;;               (goto-char (point-min))
;;               (looking-at "text/")))
;;         (progn
;;           (lwarn 'file-detection :warning
;;                  "'file' command not found in %s at line %d"
;;                  (or load-file-name buffer-file-name)
;;                  (line-number-at-pos))
;;           nil))))

;; (defun my/delete-trailing-whitespace-in-repo (&rest _args)
;;   "Delete trailing whitespace in the current git repository."
;;   (when (and (buffer-file-name)
;;              (vc-git-root default-directory)
;;              (executable-find "file"))
;;     (save-excursion
;;       (let* ((initially-open-buffers (mapcar #'buffer-file-name (buffer-list)))
;;              (default-directory (vc-git-root (buffer-file-name)))
;;              (modified-buffer-files (mapcar #'buffer-file-name
;;                                             (seq-filter (lambda (buf)
;;                                                           (and (buffer-modified-p buf)
;;                                                                (buffer-file-name buf)))  ; Check buffer has a file
;;                                                         (buffer-list))))
;;              (changed-files-in-repo (mapcar (lambda (x) (expand-file-name x))
;;                                             (magit-unstaged-files)))
;;              (changed-text-files-in-repo (seq-filter (lambda (file-path)
;;                                                        (and (my/is-text-file-using-file-cmd file-path)
;;                                                             (not (seq-some
;;                                                                   (lambda (buf-file)
;;                                                                     (file-equal-p (expand-file-name file-path)
;;                                                                                   buf-file))
;;                                                                   modified-buffer-files))))
;;                                                      changed-files-in-repo)))
;;         (dolist (file changed-text-files-in-repo)
;;           (with-current-buffer (find-file-noselect file)
;;             (delete-trailing-whitespace)
;;             (save-buffer)
;;             (unless (seq-some
;;                      (lambda (initially-open-buffer-file)
;;                        (file-equal-p (buffer-file-name)
;;                                      initially-open-buffer-file))
;;                      initially-open-buffers)
;;               (kill-buffer))))))))

(provide 'util)
