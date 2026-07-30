;;; --grid-dired-narrow__dired_media_navigation_search@@20260729T224121.el --- dired-narrow integration for grid-dired -*- lexical-binding: t -*-

;; Author: claude-fable-5
;; Version: 0.1
;; Package-Requires: ((emacs "30.2"))
;; title: grid-dired-narrow
;; keywords: :dired:media:navigation:search:
;; date: [2026-07-29 Wed 22:41]
;; identifier: 20260729T224121

;;; Commentary:

;; Glue between grid-dired (or plain image-dired) and dired-narrow:
;; while you live-narrow a Dired buffer, the associated thumbnail grid
;; narrows along with it, keystroke by keystroke.
;;
;; - Typing in the dired-narrow minibuffer filters the grid live.
;; - RET keeps both the Dired buffer and the grid narrowed.
;; - C-g restores both.
;; - Reverting the Dired buffer (g) restores the full grid.
;;
;; Loading this file installs the integration; grid-dired.el does so
;; automatically once dired-narrow is loaded.  It also binds "/" in
;; the thumbnail buffer to `grid-dired-narrow-thumbnails', which lets
;; you start narrowing without leaving the grid.
;;
;; How it works: dired-narrow hides non-matching Dired lines by
;; putting an `:dired-narrow' text property on them.  After each
;; update we collect the still-visible file names, intersect them with
;; a snapshot of the grid taken when narrowing started, and rebuild
;; the thumbnail buffer from cached thumbnails.  Rebuilding is cheap:
;; thumbnails are files on disk (or cached placeholder specs) by the
;; time you narrow.

;;; Code:

(require 'cl-lib)
(require 'image-dired)
(with-eval-after-load 'dired-narrow
  (require 'dired-narrow))
(require 'grid-dired nil t)

(defgroup grid-dired-narrow nil
  "Narrow the image-dired thumbnail grid together with dired-narrow."
  :prefix "grid-dired-narrow-"
  :group 'grid-dired)

(defcustom grid-dired-narrow-delay 0.05
  "Seconds to wait after a keystroke before updating the grid.
Coalesces updates while you type quickly.  Set to 0 for immediate
updates."
  :type 'number)


;;; State

(defvar grid-dired-narrow--all-thumbs nil
  "Snapshot of the full grid taken when narrowing started.
A list of (ORIGINAL-FILE . ASSOCIATED-DIRED-BUFFER) in display order,
or nil when the grid is not narrowed.")

(defvar grid-dired-narrow--timer nil
  "Pending debounce timer for the live grid update.")


;;; Reading grid and Dired state

(defun grid-dired-narrow--snapshot ()
  "Return the thumbnails currently in the grid, in order.
A list of (ORIGINAL-FILE . ASSOCIATED-DIRED-BUFFER).  Must be called
with the thumbnail buffer current."
  (save-excursion
    (goto-char (point-min))
    (let (acc)
      (while (not (eobp))
        (when (get-text-property (point) 'image-dired-thumbnail)
          (push (cons (get-text-property (point) 'original-file-name)
                      (get-text-property (point) 'associated-dired-buffer))
                acc))
        (forward-char))
      (nreverse acc))))

(defun grid-dired-narrow--grid-uses-dired-p (dired-buf)
  "Return non-nil if the grid's thumbnails come from DIRED-BUF.
Must be called with the thumbnail buffer current."
  (let ((pairs (or grid-dired-narrow--all-thumbs
                   (grid-dired-narrow--snapshot))))
    (cl-some (lambda (pair) (eq (cdr pair) dired-buf)) pairs)))

(defun grid-dired-narrow--visible-files (dired-buf)
  "Return the absolute names of files not narrowed away in DIRED-BUF.
Visibility is defined by dired-narrow's `:dired-narrow' text
property, so this needs no dired-narrow internals at runtime."
  (with-current-buffer dired-buf
    (save-excursion
      (goto-char (point-min))
      (let (acc)
        (while (not (eobp))
          (unless (get-text-property (point) :dired-narrow)
            (when-let ((file (dired-get-filename nil t)))
              (push file acc)))
          (forward-line 1))
        (nreverse acc)))))


;;; Rebuilding the grid

(defun grid-dired-narrow--thumb-file (file)
  "Return the thumbnail file for FILE, creating it when possible."
  (condition-case nil
      (image-dired--get-create-thumbnail-file file)
    ;; E.g. `grid-dired-mode' was turned off and FILE no longer
    ;; matches image-dired's regexp.  A cached thumbnail may still
    ;; exist; point at it either way.
    (error (image-dired-thumb-name file))))

(defun grid-dired-narrow--rebuild (pairs)
  "Replace the grid's contents with the thumbnails in PAIRS.
PAIRS is a list of (ORIGINAL-FILE . ASSOCIATED-DIRED-BUFFER).  Keeps
point on the same file when it survives the filter.  Must be called
with the thumbnail buffer current."
  (let ((inhibit-read-only t)
        (current-file (get-text-property (point) 'original-file-name)))
    (erase-buffer)
    (setq image-dired--number-of-thumbnails 0)
    (pcase-dolist (`(,file . ,dired-buf) pairs)
      (image-dired-insert-thumbnail
       (grid-dired-narrow--thumb-file file) file dired-buf)
      (cl-incf image-dired--number-of-thumbnails))
    (image-dired--line-up-with-method)
    (goto-char (point-min))
    (when current-file
      (let ((pos (point-min)))
        (while (and pos
                    (not (equal (get-text-property pos 'original-file-name)
                                current-file)))
          (setq pos (next-single-property-change pos 'original-file-name)))
        (goto-char (or pos (point-min)))))
    (when-let ((win (get-buffer-window (current-buffer))))
      (set-window-point win (point)))
    (image-dired--thumb-update-marks)
    (image-dired--update-header-line)))

(defun grid-dired-narrow--apply (visible-files)
  "Narrow the grid to VISIBLE-FILES, or restore it when nothing is hidden.
Must be called with the thumbnail buffer current."
  (let* ((current (grid-dired-narrow--snapshot))
         (master (or grid-dired-narrow--all-thumbs current))
         (vset (make-hash-table :test 'equal)))
    (dolist (file visible-files) (puthash file t vset))
    (let ((keep (cl-remove-if-not (lambda (pair) (gethash (car pair) vset))
                                  master)))
      (unless (equal keep current)
        (setq grid-dired-narrow--all-thumbs master)
        (grid-dired-narrow--rebuild keep))
      ;; Fully restored (or never actually filtered): drop the snapshot
      ;; so future grids start fresh.
      (when (equal keep master)
        (setq grid-dired-narrow--all-thumbs nil)))))


;;; Syncing

(defun grid-dired-narrow--sync (dired-buf)
  "Make the grid match what is visible in DIRED-BUF, if they are related."
  (when-let ((thumb-buf (get-buffer image-dired-thumbnail-buffer)))
    (when (buffer-live-p dired-buf)
      (with-current-buffer thumb-buf
        (when (grid-dired-narrow--grid-uses-dired-p dired-buf)
          (grid-dired-narrow--apply
           (grid-dired-narrow--visible-files dired-buf)))))))

(defun grid-dired-narrow--sync-soon (dired-buf)
  "Debounced version of `grid-dired-narrow--sync'."
  (when (timerp grid-dired-narrow--timer)
    (cancel-timer grid-dired-narrow--timer))
  (setq grid-dired-narrow--timer
        (run-with-timer grid-dired-narrow-delay nil
                        #'grid-dired-narrow--sync dired-buf)))

(defun grid-dired-narrow--update-advice (&rest _)
  "Mirror a live dired-narrow update into the thumbnail grid.
Installed as :after advice on `dired-narrow--update', which runs with
the Dired buffer current."
  (grid-dired-narrow--sync-soon (current-buffer)))

(defun grid-dired-narrow--internal-advice (orig-fun &rest args)
  "Sync the grid once narrowing finishes, however it finishes.
Runs ORIG-FUN (`dired-narrow--internal') with ARGS; the unwind also
fires when the user cancels with C-g, at which point dired-narrow has
already restored the Dired buffer."
  (let ((dired-buf (current-buffer)))
    (unwind-protect
        (apply orig-fun args)
      (when (timerp grid-dired-narrow--timer)
        (cancel-timer grid-dired-narrow--timer))
      (grid-dired-narrow--sync dired-buf))))

(defun grid-dired-narrow--after-readin ()
  "Restore the grid after its Dired buffer is reverted.
Reverting (`g') is dired-narrow's documented way to undo a committed
narrow, so the grid must follow suit."
  (when grid-dired-narrow--all-thumbs
    (grid-dired-narrow--sync-soon (current-buffer))))


;;; Narrowing straight from the grid

(defun grid-dired-narrow-thumbnails ()
  "Narrow the grid by running `dired-narrow' on its Dired buffer.
Type to filter, RET to keep the narrowed view, C-g to cancel."
  (interactive nil image-dired-thumbnail-mode)
  (let ((dired-buf (or (image-dired-associated-dired-buffer)
                       (cdr (or (car grid-dired-narrow--all-thumbs)
                                (car (grid-dired-narrow--snapshot)))))))
    (unless (buffer-live-p dired-buf)
      (user-error "No associated Dired buffer to narrow"))
    (if-let ((win (get-buffer-window dired-buf)))
        (with-selected-window win (dired-narrow))
      (with-current-buffer dired-buf (dired-narrow)))))


;;; Wiring

(advice-add 'dired-narrow--update :after #'grid-dired-narrow--update-advice)
(advice-add 'dired-narrow--internal :around #'grid-dired-narrow--internal-advice)
(add-hook 'dired-after-readin-hook #'grid-dired-narrow--after-readin)
(keymap-set image-dired-thumbnail-mode-map "/" #'grid-dired-narrow-thumbnails)

(defun grid-dired-narrow-unload-function ()
  "Remove grid-dired-narrow's advice and hooks."
  (advice-remove 'dired-narrow--update #'grid-dired-narrow--update-advice)
  (advice-remove 'dired-narrow--internal #'grid-dired-narrow--internal-advice)
  (remove-hook 'dired-after-readin-hook #'grid-dired-narrow--after-readin)
  (keymap-unset image-dired-thumbnail-mode-map "/" t)
  nil)

(provide '--grid-dired-narrow__dired_media_navigation_search@@20260729T224121)

;;; --grid-dired-narrow__dired_media_navigation_search@@20260729T224121.el ends here
