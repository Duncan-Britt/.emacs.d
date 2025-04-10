;;; jupyter-ascending.el --- Edit Jupyter Notebooks from Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Duncan Britt
;;
;; Author: Duncan Britt
;; Version: 0.1
;; Keywords: jupyter, notebook, python
;; Package-Requires: ((emacs "29.4") (python "0.28"))
;;
;;; Commentary:
;;
;; This package provides a minor mode for editing Jupyter notebooks in Emacs
;; using the jupyter_ascending Python package: https://github.com/imbue-ai/jupyter_ascending
;;
;; ┌──────────────┐
;; │ Installation │
;; └──────────────┘
;;   The following dependencies must be installed, in addition to Jupyter:
;;   pip install jupyter_ascending &&
;;   python3 -m jupyter nbextension    install jupyter_ascending --sys-prefix --py && \
;;   python3 -m jupyter nbextension     enable jupyter_ascending --sys-prefix --py && \
;;   python3 -m jupyter serverextension enable jupyter_ascending --sys-prefix --py
;;
;;   Example package installation (using elpaca) with recommended keybindings
;;   (use-package jupyter-ascending
;;     :ensure t
;;     :hook (python-mode . (lambda ()
;;                            (when (and buffer-file-name
;;                                       (string-match-p "\\.sync\\.py\\'" buffer-file-name))
;;                              (jupyter-ascending-mode 1))))
;;     :bind (:map jupyter-ascending-mode-map
;;                 ("C-c C-k" . ja-execute-line)
;;                 ("C-c C-a" . ja-execute-all)
;;                 ("C-c C-n" . ja-next-cell)
;;                 ("C-c C-p" . ja-previous-cell)
;;                 ("C-c t" . ja-cycle-cell-type)
;;                 ("C-c '" . ja-edit-markdown-cell)))
;;
;; ┌───────┐
;; │ Usage │
;; └───────┘
;;   Create a notebook pair with:
;;       M-x ja-create-notebook-pair RET example RET
;;   Or, equivalently
;;       python3 -m jupyter_ascending.scripts.make_pair --base example
;;   This creates synced files: example.sync.py and example.sync.ipynb
;;
;;   Start jupyter and open the notebook:
;;       With example.sync.py open,
;;       M-x ja-start-notebook
;;   Or, equivalently,
;;       python3 -m jupyter notebook example.sync.ipynb
;;
;;   If you have an existing jupyter notebook, create a python file from it,
;;       M-x ja-convert-notebook RET example.ipynb RET
;;   Or, equivalently,
;;       jupytext --to py:percent <file_name>
;;   and then add the .sync suffix to both files
;;
;;; Code:
;; ┌─────────────┐
;; │ Keybindings │
;; └─────────────┘
(defgroup jupyter-ascending nil
  "Edit Jupyter notebooks in Emacs using jupyter_ascending."
  :group 'tools
  :prefix "ja-")

(defcustom ja-python-command "python3"
  "Python command used by jupyter ascending.")

(defvar jupyter-ascending-mode-map (make-sparse-keymap)
  "Keymap for `jupyter-ascending-mode'.")

;; ┌──────────┐
;; │ Commands │
;; └──────────┘
(defun ja-sync-file ()
  "Sync the current buffer with its associated Jupyter notebook."
  (interactive)
  (when (called-interactively-p 'any)
    (save-buffer))
  (ja--run-jupyter-ascending-command
   "sync"
   (concat "--filename \"" (ja--get-filename) "\"")))

(defun ja-execute-line ()
  "Execute the cell at current line in the associated Jupyter notebook."
  (interactive)
  (save-buffer)
  (ja--run-jupyter-ascending-command
   "execute"
   (concat "--filename \"" (ja--get-filename) "\"")
   (concat "--linenumber \"" (number-to-string (ja--get-current-line-number)) "\"")))

(defun ja-execute-all ()
  "Execute all cells in the associated Jupyter notebook."
  (interactive)
  (save-buffer)
  (ja--run-jupyter-ascending-command
   "execute_all"
   (concat "--filename \"" (ja--get-filename) "\"")))

(defun ja-start-notebook ()
  "Start a Jupyter notebook for the current file.
Assumes the notebook has the same name as the current file but with .ipynb extension."
  (interactive)
  (let* ((current-file (ja--get-filename))
         (notebook-file (concat (file-name-sans-extension current-file) ".ipynb"))
         (default-directory (file-name-directory (expand-file-name current-file))))

    ;; Make sure the notebook exists
    (unless (file-exists-p notebook-file)
      (error "Notebook file %s does not exist. Run ja-init-file first" notebook-file))

    ;; Start notebook server in the appropriate directory
    (async-shell-command
     (format "%s -m jupyter notebook %s"
             ja-python-command
             (file-name-nondirectory notebook-file))
     "*jupyter-notebook*")))

(defun ja-restart-notebook ()
  "Restart associated Jupyter notebook."
  (interactive)
  (save-buffer)
  (ja--run-jupyter-ascending-command
   "restart"
   (concat "--filename \"" (ja--get-filename) "\"")))

(defun ja-after-save-hook ()
  "Run after saving to sync with Jupyter notebook."
  (when jupyter-ascending-mode
    (ja-sync-file)))

(defun ja-next-cell ()
  "Move point to the next cell marked by '# %%' at the beginning of a line."
  (interactive)
  (let ((orig-point (point))
        (found nil))
    (end-of-line)  ; Move past current line before searching
    (if (re-search-forward "^# %%" nil t)
        (beginning-of-line)
      ;; No next cell found, create one
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n# %%\n")
      (message "Created new code cell"))))

(defun ja-previous-cell ()
  "Move point to the previous cell marked by '# %%' at the beginning of a line."
  (interactive)
  (let ((orig-point (point))
        (found nil))
    (beginning-of-line)  ; Move to beginning of current line before searching
    (if (re-search-backward "^# %%" nil t)
        (beginning-of-line)
      (goto-char orig-point)
      (message "No previous cell found"))))

(defun ja-cycle-cell-type ()
  "Toggle the current cell between code and markdown type."
  (interactive)
  (save-excursion
    ;; Find the current cell header
    (let ((cell-start (save-excursion
                        (if (looking-at "^# %%")
                            (point)
                          (and (re-search-backward "^# %%" nil t)
                               (point))))))
      (if (not cell-start)
          (message "Not in a cell")
        (goto-char cell-start)
        (if (looking-at "^# %% \\[markdown\\]")
            ;; If it's a markdown cell, convert to code cell
            (progn
              (delete-region (point) (line-end-position))
              (insert "# %%")
              (message "Converted to code cell"))
          ;; If it's a code cell, convert to markdown cell
          (progn
            (delete-region (point) (line-end-position))
            (insert "# %% [markdown]")
            (message "Converted to markdown cell")))))))

(defun ja-markdown-RET ()
  "Handle RET key in markdown cells.
If in a markdown cell, insert a '#' comment marker with proper spacing.
Otherwise, use the default RET behavior."
  (interactive)
  (if (ja--in-markdown-cell-p)
      (progn
        (newline)
        (insert "# ")
        (set-marker (mark-marker) (point) (current-buffer)))
    (newline-and-indent)))

(defun ja-create-notebook-pair (base-name)
  "Create a synced pair of Jupyter notebook files using jupyter_ascending.
With BASE-NAME as the file prefix (without extension), creates .sync.py and .sync.ipynb files."
  (interactive
   (list
    (read-string "Base name for notebook: ")))

  (let* ((default-directory (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory))
         (command (format "%s -m jupyter_ascending.scripts.make_pair --base %s"
                          ja-python-command
                          (shell-quote-argument base-name)))
         (buffer-name "*jupyter-ascending-create*"))

    (message "Creating notebook pair with base name: %s" base-name)
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (let ((proc (start-process "ja-create-notebook" buffer-name
                                 shell-file-name shell-command-switch command)))
        (set-process-sentinel
         proc
         (lambda (process event)
           (if (string-match "finished" event)
               (message "Created Jupyter notebook pair: %s.sync.{py,ipynb}" base-name)
             (message "Error creating notebook pair: %s"
                      (with-current-buffer buffer-name (buffer-string))))
           ;; Optionally visit the new files
           (when (string-match "finished" event)
             (when (y-or-n-p "Open the Python file? ")
               (find-file (format "%s.sync.py" base-name))
               (jupyter-ascending-mode 1)))))))))

(defun ja-convert-notebook ()
  "Convert an existing Jupyter notebook to a synced pair with jupytext.
Renames both files with .sync infix."
  (interactive)
  (let* ((ipynb-file (cond
                      ;; If in dired and at an ipynb file
                      ((and (derived-mode-p 'dired-mode)
                            (dired-get-filename nil t)
                            (string-match "\\.ipynb$" (dired-get-filename nil t)))
                       (dired-get-filename nil t))
                      ;; If current buffer is an ipynb file
                      ((and (buffer-file-name)
                            (string-match "\\.ipynb$" (buffer-file-name)))
                       (buffer-file-name))
                      ;; Otherwise prompt with a file selector
                      (t (read-file-name "Select Jupyter notebook (.ipynb): " nil nil t nil
                                         (lambda (filename)
                                           (string-match "\\.ipynb$" filename))))))
         (ipynb-file (expand-file-name ipynb-file))
         (ipynb-dir (file-name-directory ipynb-file))
         (ipynb-base (file-name-sans-extension (file-name-nondirectory ipynb-file)))
         (py-file (expand-file-name (concat ipynb-base ".py") ipynb-dir))
         (sync-ipynb-file (expand-file-name (concat ipynb-base ".sync.ipynb") ipynb-dir))
         (sync-py-file (expand-file-name (concat ipynb-base ".sync.py") ipynb-dir))
         (buffer-name "*jupytext-convert*"))

    ;; Run jupytext to convert ipynb to py
    (message "Converting %s to Python..." ipynb-file)
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (let ((proc (start-process "jupytext" buffer-name
                                 "jupytext" "--to" "py:percent" ipynb-file)))
        (set-process-sentinel
         proc
         (lambda (process event)
           (if (string-match "finished" event)
               (progn
                 (message "Converted %s to %s" ipynb-file py-file)
                 ;; Now rename files with .sync infix
                 (rename-file ipynb-file sync-ipynb-file t)
                 (rename-file py-file sync-py-file t)
                 (message "Created synced pair: %s and %s"
                          sync-ipynb-file sync-py-file)

                 ;; Optionally open the Python file
                 (when (y-or-n-p "Open the Python file? ")
                   (find-file sync-py-file)
                   (jupyter-ascending-mode 1)))
             (message "Error converting notebook: %s"
                      (with-current-buffer buffer-name (buffer-string))))))))))

;; ┌──────────────────┐
;; │ Markdown editing │
;; └──────────────────┘
(defvar-local ja--edit-marker nil
  "Marker for the original location in the source buffer.")

(defvar-local ja--edit-src-buffer nil
  "Buffer that contains the original markdown cell.")

(defvar-local ja--edit-cell-overlay nil
  "Overlay highlighting the cell being edited.")

(defun ja-save-src-buffer ()
  "Save the source buffer associated with this edit buffer."
  (interactive)
  (when (and ja--edit-src-buffer (buffer-live-p ja--edit-src-buffer))
    (let* ((cell-content (buffer-string))
           (src-buffer ja--edit-src-buffer)
           (overlay (buffer-local-value 'ja--edit-cell-overlay src-buffer))
           (saved-edit-buffer (current-buffer)))
      (with-current-buffer src-buffer
        (when (buffer-local-value 'ja--edit-cell-overlay src-buffer)
          (let ((start (overlay-start overlay))
                (end (overlay-end overlay)))
            (undo-boundary)
            (goto-char start)
            (delete-region start end)

            ;; Add the comment prefix to each line
            ;; Split content by lines, add comment prefix, then join
            (let ((commented-lines
                   (mapconcat (lambda (line) (concat "# " line))
                              (split-string cell-content "\n")
                              "\n")))
              (insert commented-lines))

            ;; Recreate the overlay for the new content
            (let ((new-end (point)))
              (delete-overlay overlay)
              (setq overlay (make-overlay start new-end))
              (overlay-put overlay 'face 'secondary-selection)
              (setq-local ja--edit-cell-overlay overlay)
              ;; Update overlay in edit buffer too
              (with-current-buffer saved-edit-buffer
                (setq-local ja--edit-cell-overlay overlay)))))

        (save-buffer)
        (ja-sync-file)))))

(defun ja-edit-markdown-cell ()
  "Edit the current markdown cell in a dedicated buffer."
  (interactive)
  (barf-if-buffer-read-only)
  (unless (ja--in-markdown-cell-p)
    (user-error "Not in a markdown cell"))

  ;; Find the cell boundaries
  (let* ((cell-start (save-excursion
                       (re-search-backward "^# %%" nil t)
                       (forward-line 1)
                       (point)))
         (cell-end (save-excursion
            (let ((next-cell (save-excursion
                               (and (re-search-forward "^# %%" nil t)
                                    (line-beginning-position)))))
              ;; Start from cell-start
              (goto-char cell-start)
              ;; Find the last line that starts with # before the next cell
              (let ((last-comment-line nil))
                (while (and (< (point) (or next-cell (point-max)))
                            (not (eobp)))
                  (when (looking-at "^#")
                    (setq last-comment-line (line-end-position)))
                  (forward-line 1))
                ;; If we found at least one comment line, use it as end
                ;; Otherwise fall back to next cell or point-max
                (or last-comment-line next-cell (point-max))))))
         (cell-content (ja--markdown-cell-content cell-start cell-end))
         (edit-buffer (generate-new-buffer (concat "*ja-markdown-edit*")))
         (src-buffer (current-buffer))
         (src-window (selected-window))
         (overlay (make-overlay cell-start cell-end)))

    ;; Set properties for the overlay
    (overlay-put overlay 'face 'secondary-selection)
      (setq ja--edit-cell-overlay overlay)

    ;; Create marker for the original position
    (setq ja--edit-marker (set-marker (make-marker) (point)))

    ;; Set up the edit buffer
    (with-current-buffer edit-buffer
      (markdown-mode)
      (insert cell-content)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq-local ja--edit-src-buffer src-buffer)
      (setq-local ja--edit-cell-overlay overlay)

      ;; Set up local keybindings for this edit buffer
      (use-local-map (copy-keymap markdown-mode-map))
      (local-set-key (kbd "C-c C-c") #'ja-edit-markdown-finish)
      (local-set-key (kbd "C-c C-k") #'ja-edit-markdown-abort)

      ;; Remap save commands to save the source buffer
      (local-set-key (kbd "C-x C-s") #'ja-save-src-buffer)

  ;; Set header line to indicate what we're editing
  (setq header-line-format
        (substitute-command-keys
         "Edit markdown cell. \\[ja-edit-markdown-finish] to finish, \\[ja-edit-markdown-abort] to abort. \\[ja-save-src-buffer] to save source.")))

    ;; Pop to the edit buffer
    (pop-to-buffer edit-buffer)))

(defun ja-edit-markdown-finish ()
  "Finish editing the markdown cell and update the original buffer."
  (interactive)
  (let ((edit-buffer (current-buffer))
        (cell-content (buffer-string))
        (src-buffer ja--edit-src-buffer)
        (overlay ja--edit-cell-overlay)
        (edit-window (selected-window)))

    (unless (and src-buffer (buffer-live-p src-buffer))
      (user-error "Source buffer gone, cannot finish editing"))

    ;; Insert the edited content back into the source buffer
    (with-current-buffer src-buffer
      (when (buffer-local-value 'ja--edit-cell-overlay src-buffer)
        (let ((overlay (buffer-local-value 'ja--edit-cell-overlay src-buffer))
              (start (overlay-start overlay))
              (end (overlay-end overlay)))
          (undo-boundary)
          (goto-char start)
          (delete-region start end)

          ;; Add the comment prefix to each line
          ;; Split content by lines, add comment prefix, then join
          (let ((commented-lines
                 (mapconcat (lambda (line) (concat "# " line))
                            (split-string cell-content "\n")
                            "\n")))
            (insert commented-lines))
          ;; Move cursor to the original position if possible
          (when (and ja--edit-marker (marker-buffer ja--edit-marker))
            (goto-char ja--edit-marker)
            (set-marker ja--edit-marker nil))

          ;; Clean up overlay
          (delete-overlay overlay)
          (setq ja--edit-cell-overlay nil))))

    ;; Find window displaying source buffer
    (let ((src-window (get-buffer-window src-buffer)))
      (if src-window
          ;; If source buffer is visible in a window, select it
          (select-window src-window)
        ;; Otherwise, switch to source buffer in the edit window
        (set-window-buffer edit-window src-buffer)))

    ;; Kill the edit buffer
    (let ((kill-buffer-query-functions nil))
      (kill-buffer edit-buffer)

      (delete-window edit-window))))

(defun ja-edit-markdown-abort ()
  "Abort editing the markdown cell."
  (interactive)
  (let ((edit-buffer (current-buffer))
        (src-buffer ja--edit-src-buffer)
        (overlay ja--edit-cell-overlay)
        (edit-window (selected-window)))

    ;; Go back to source buffer
    (when (and src-buffer (buffer-live-p src-buffer))
      (with-current-buffer src-buffer
        ;; Clean up overlay
        (when (and overlay (overlay-buffer overlay))
          (delete-overlay overlay)
          (setq ja--edit-cell-overlay nil))
        ;; Move cursor to the original position if possible
        (when (and ja--edit-marker (marker-buffer ja--edit-marker))
          (goto-char ja--edit-marker)
          (set-marker ja--edit-marker nil)))

      ;; Find window displaying source buffer
      (let ((src-window (get-buffer-window src-buffer)))
        (if src-window
            ;; If source buffer is visible in a window, select it
            (select-window src-window)
          ;; Otherwise, switch to source buffer in the edit window
          (set-window-buffer edit-window src-buffer))))

    ;; Kill the edit buffer
    (let ((kill-buffer-query-functions nil))
      (kill-buffer edit-buffer))

    (delete-window edit-window)))

;; ┌────────────┐
;; │ Minor mode │
;; └────────────┘
;;;###autoload
(define-minor-mode jupyter-ascending-mode
  "Minor mode for editing Jupyter notebooks with jupyter_ascending.

\\{jupyter-ascending-mode-map}"
  :lighter " JA"
  :keymap jupyter-ascending-mode-map
  :group 'jupyter-ascending
  (if jupyter-ascending-mode
      (progn
        (add-hook 'after-save-hook #'ja-after-save-hook nil t)
        (local-set-key (kbd "RET") 'ja-markdown-RET)
        (message "Jupyter-Ascending mode enabled"))
    (remove-hook 'after-save-hook #'ja-after-save-hook t)
    (local-unset-key (kbd "RET"))
    (message "Jupyter-Ascending mode disabled")))

;; ┌────────────────────┐
;; │ Internal Functions │
;; └────────────────────┘

(defun ja--markdown-cell-content (cell-start cell-end)
  (let ((cell-content ""))
    (save-excursion
      (goto-char cell-start)
      (while (< (point) cell-end)
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (if (looking-at "^#\\s-?")
              ;; If line starts with # and optional space
              (let ((comment-end (match-end 0))
                    (line-content (buffer-substring-no-properties line-start line-end)))
                ;; Only add the part after the comment marker
                (setq cell-content
                      (concat cell-content
                              (substring line-content (- comment-end line-start))
                              "\n")))
            ;; If no comment marker (shouldn't happen in well-formed cells),
            ;; just add the line as-is
            (setq cell-content
                  (concat cell-content
                          (buffer-substring-no-properties line-start line-end)
                          "\n"))))
        (forward-line 1)))

    (when (string-match-p "\n\\'" cell-content)
      (setq cell-content (substring cell-content 0 -1)))
    cell-content))

(defun ja--get-current-line-number ()
  "Get the current line number."
  (line-number-at-pos))

(defun ja--get-filename ()
  "Get the current buffer's filename."
  (buffer-file-name))

(defun ja--run-jupyter-ascending-command (command &rest args)
  "Run a jupyter_ascending COMMAND with ARGS asynchronously."
  (let* ((proc-name "jupyter-ascending")
         (module-path (concat "jupyter_ascending.requests." command))
         ;; Convert args from a list of strings with quoted parameters to a proper argument list
         (processed-args (mapcar (lambda (arg)
                                  (if (string-match-p "--\\([a-z]+\\)\\s-+\"\\(.+\\)\"" arg)
                                      (let ((parts (split-string arg "\"" t)))
                                        (list (car parts) (nth 1 parts)))
                                    arg))
                                args))
         ;; Flatten the list of arguments
         (flat-args (apply #'append (list "-m" module-path)
                           (mapcar (lambda (arg)
                                    (if (listp arg)
                                        (list (string-trim (car arg)) (cadr arg))
                                      (list arg)))
                                  processed-args))))
    (message "Running: %s %s" ja-python-command (mapconcat #'identity flat-args " "))
    (let ((proc (apply #'start-process
                       proc-name
                       "*jupyter-ascending*"  ; buffer to help with debugging
                       ja-python-command
                       flat-args)))
      (set-process-sentinel
       proc
       (lambda (process event)
         (if (string-match "finished" event)
             (message "Jupyter ascending `%s' completed successfully" command)
           (message "Jupyter command event: %s" event)))))))

(defun ja--in-markdown-cell-p ()
  "Return non-nil if point is within a markdown cell.
A markdown cell is defined by a line starting with '# %% [markdown]'."
  (save-excursion
    (let ((cell-start (save-excursion
                        (if (looking-at "^# %%")
                            (point)
                          (and (re-search-backward "^# %%" nil t)
                               (point)))))
          (next-cell-start (save-excursion
                             (forward-line)
                             (and (re-search-forward "^# %%" nil t)
                                  (line-beginning-position)))))
      (when cell-start
        (goto-char cell-start)
        (and (looking-at "^# %% \\[markdown\\]")
             (or (not next-cell-start)
                 (< (point) next-cell-start)))))))

(provide 'jupyter-ascending)
;;; jupyter-ascending.el ends here
