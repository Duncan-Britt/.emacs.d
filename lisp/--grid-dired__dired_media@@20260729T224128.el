;;; --grid-dired__dired_media@@20260729T224128.el --- Browse any files as a grid of thumbnails -*- lexical-binding: t -*-

;; Author: claude-fable-5
;; Version: 0.1
;; Package-Requires: ((emacs "30.2"))
;; title: grid-dired
;; keywords: :dired:media:
;; date: [2026-07-29 Wed 22:41]
;; identifier: 20260729T224128

;;; Commentary:

;; Image-Dired is great for browsing pictures as a grid of thumbnails,
;; but it only knows about image files.  This package extends it - via
;; a handful of `advice-add' hooks, no forking - so the same grid works
;; for videos, audio files, and (optionally) every other file type.
;;
;; Quick start:
;;
;;   M-x grid-dired RET ~/Movies RET
;;
;; All the usual image-dired keys work in the resulting buffer:
;; arrow keys to move, RET to open, m/u/d to mark/unmark/flag in the
;; associated Dired buffer, TAB to jump to Dired, g g to re-line-up.
;;
;; How thumbnails are produced, by file type:
;;
;; - Images and PDFs: the stock image-dired pipeline (GraphicsMagick /
;;   ImageMagick).  If neither is installed, we fall back to `sips'
;;   and Quick Look on macOS.
;; - Video and audio: ffmpeg.  When the bundled media-thumbnail
;;   package is on `load-path' we reuse its command builders, which
;;   try embedded cover art first and then walk a chain of seek
;;   positions so short or odd files still produce a frame.
;; - Everything else: on macOS, Quick Look (`qlmanage') renders real
;;   previews for PDFs, text, HTML, Office documents, and more.
;; - Universal fallback: a generated SVG "file card" showing the
;;   extension and file name, so every file gets *some* tile in the
;;   grid.  Directories get a folder card; RET on one opens Dired.
;;
;; Opening files from the grid (RET):
;;
;; - Images/PDFs: stock image-dired display buffer.
;; - Video/audio: visited with `find-file', so if you use
;;   ready-player-mode you get its player buffer.  Without
;;   ready-player, the file is handed to `open'/`xdg-open'.
;; - Directories: opened in Dired.
;; - Anything else: visited normally (C-RET always opens externally).
;;
;; Thumbnails are cached where image-dired caches them (see
;; `image-dired-thumbnail-storage'), so all of this composes with the
;; Thumbnail Managing Standard, tags, marks, tracking, etc.

;;; Code:

(require 'cl-lib)
(require 'image-dired)
(require 'svg)

;; Optional: bundled alongside this package.  When present we reuse its
;; ffmpeg command builders and seek-time chain; when absent we fall
;; back to a simpler built-in ffmpeg invocation.
(require 'media-thumbnail nil t)

(declare-function media-thumbnail-ffmpeg-frame-cmd "media-thumbnail")
(declare-function media-thumbnail--resolve-seek-times "media-thumbnail")
(declare-function image-dired--probe-thumbnail-cmd "image-dired-external")
(declare-function w32-shell-execute "w32fns.c")
(defvar media-thumbnail-video-exts)
(defvar media-thumbnail-ffmpeg-executable)
(defvar media-thumbnail-ffmpeg-seek-times)


;;; Customizable variables

(defgroup grid-dired nil
  "Browse any files as a grid of thumbnails, building on Image-Dired."
  :prefix "grid-dired-"
  :group 'image-dired)

(defcustom grid-dired-show-all-files t
  "If non-nil, include every file in the thumbnail grid.
Files without a real preview get a generated placeholder card.
If nil, only images, videos, audio files and PDFs are included."
  :type 'boolean)

(defcustom grid-dired-video-extensions
  (if (boundp 'media-thumbnail-video-exts)
      media-thumbnail-video-exts
    '("mp4" "m4v" "mkv" "webm" "mov" "qt" "avi" "wmv" "flv" "mpg"
      "mpeg" "m2v" "ogv" "3gp" "3g2" "ts" "vob" "rmvb" "f4v" "mts"))
  "File name extensions treated as video files."
  :type '(repeat string))

(defcustom grid-dired-audio-extensions
  '("mp3" "m4a" "m4b" "flac" "ogg" "oga" "opus" "wav" "aiff" "aif"
    "aac" "wma" "mka" "ape" "wv" "dsf" "alac")
  "File name extensions treated as audio files."
  :type '(repeat string))

(defcustom grid-dired-ffmpeg-program
  (if (boundp 'media-thumbnail-ffmpeg-executable)
      media-thumbnail-ffmpeg-executable
    "ffmpeg")
  "Name of the ffmpeg executable used for video and audio thumbnails."
  :type 'string)

(defcustom grid-dired-qlmanage-program "qlmanage"
  "Name of the macOS Quick Look thumbnailing executable."
  :type 'string)

(defcustom grid-dired-sips-program "sips"
  "Name of the macOS sips executable, used to scale and convert images."
  :type 'string)

(defcustom grid-dired-external-open-command
  (cond ((eq system-type 'darwin) "open")
        ((executable-find "xdg-open") "xdg-open"))
  "Program used to open a file with the system's default application."
  :type '(choice string (const :tag "Not set" nil)))

(defcustom grid-dired-use-ready-player t
  "If non-nil and ready-player is installed, open media files with it.
When this is nil, or ready-player is not installed, video and audio
files selected in the thumbnail buffer are opened with
`grid-dired-external-open-command' instead."
  :type 'boolean)


;;; File-type predicates

(defun grid-dired--file-extension (file)
  "Return FILE's extension downcased, or nil."
  (when-let ((ext (file-name-extension file)))
    (downcase ext)))

(defun grid-dired--video-file-p (file)
  "Return non-nil if FILE looks like a video file."
  (member (grid-dired--file-extension file) grid-dired-video-extensions))

(defun grid-dired--audio-file-p (file)
  "Return non-nil if FILE looks like an audio file."
  (member (grid-dired--file-extension file) grid-dired-audio-extensions))

(defun grid-dired--media-file-p (file)
  "Return non-nil if FILE looks like a video or audio file."
  (or (grid-dired--video-file-p file)
      (grid-dired--audio-file-p file)))

(defun grid-dired--image-file-p (file)
  "Return non-nil if FILE is something stock image-dired can thumbnail."
  (let ((case-fold-search t))
    (or (string-match-p (image-file-name-regexp) file)
        (string-match-p "\\.pdf\\'" file))))

(defun grid-dired--imagemagick-usable-p ()
  "Return non-nil if image-dired's own thumbnail command is usable."
  (and (stringp image-dired-cmd-create-thumbnail-program)
       (executable-find image-dired-cmd-create-thumbnail-program)
       (or (not (fboundp 'image-dired--probe-thumbnail-cmd))
           (image-dired--probe-thumbnail-cmd
            image-dired-cmd-create-thumbnail-program))))

(defun grid-dired--file-non-empty-p (path)
  "Return non-nil if PATH exists on disk with non-zero size."
  (when-let ((attrs (file-attributes path)))
    (> (file-attribute-size attrs) 0)))


;;; Extending which files image-dired accepts

(defun grid-dired--file-name-regexp-advice (orig-fun)
  "Extend the regexp from ORIG-FUN with media (or all) file names."
  (if grid-dired-show-all-files
      ".*"
    (concat (funcall orig-fun)
            "\\|\\."
            (regexp-opt (append grid-dired-video-extensions
                                grid-dired-audio-extensions)
                        t)
            "\\'")))

(defun grid-dired--check-executable-advice (orig-fun executable &optional func)
  "Don't error about a missing thumbnail program; grid-dired has fallbacks.
ORIG-FUN, EXECUTABLE and FUNC as in `image-dired--check-executable-exists'."
  (if (eq executable 'image-dired-cmd-create-thumbnail-program)
      (condition-case nil
          (funcall orig-fun executable func)
        (error t))
    (funcall orig-fun executable func)))


;;; ffmpeg thumbnails for video and audio

(defun grid-dired--poster-cmd (file thumb size)
  "Shell command extracting FILE's embedded cover art into THUMB.
The result is scaled to fit in a SIZE x SIZE box.  Exits non-zero
when FILE has no attached-picture stream."
  (mapconcat
   #'identity
   (list grid-dired-ffmpeg-program
         "-nostdin" "-y" "-loglevel" "error"
         "-i" (shell-quote-argument file)
         "-map" "0:v:m:disposition:attached_pic"
         "-frames:v" "1" "-q:v" "2"
         "-vf" (shell-quote-argument
                (format "scale=%d:%d:force_original_aspect_ratio=decrease"
                        size size))
         (shell-quote-argument thumb))
   " "))

(defun grid-dired--frame-cmd (file thumb size seek)
  "Shell command decoding one frame of FILE at SEEK seconds into THUMB.
The frame is scaled to fit in a SIZE x SIZE box."
  (mapconcat
   #'identity
   (list grid-dired-ffmpeg-program
         "-nostdin" "-y" "-loglevel" "error"
         "-i" (shell-quote-argument file)
         "-ss" seek
         "-frames:v" "1" "-q:v" "2"
         "-vf" (shell-quote-argument
                (format "scale=%d:%d:force_original_aspect_ratio=decrease"
                        size size))
         (shell-quote-argument thumb))
   " "))

(defun grid-dired--ffmpeg-cmds (file thumb)
  "Return the list of shell commands to try, in order, to thumbnail FILE.
Cover art is attempted first; then a frame is decoded at each of a
series of seek positions.  When media-thumbnail is available its
command builder and seek chain are used (including \"10%\"-style
entries resolved via ffprobe); otherwise a fixed fallback chain."
  (let ((size (image-dired--thumb-size)))
    (cons
     (grid-dired--poster-cmd file thumb size)
     (if (featurep 'media-thumbnail)
         (mapcar (lambda (seek)
                   (media-thumbnail-ffmpeg-frame-cmd
                    file thumb :size size :seek-time seek))
                 (media-thumbnail--resolve-seek-times
                  file media-thumbnail-ffmpeg-seek-times))
       (mapcar (lambda (seek)
                 (grid-dired--frame-cmd file thumb size seek))
               '("10" "1" "0.1"))))))

(defun grid-dired--try-shell-cmds (cmds thumb callback)
  "Run shell CMDS one after another until one produces THUMB.
Call CALLBACK with non-nil on the first success, or nil when the
whole list is exhausted."
  (if (null cmds)
      (funcall callback nil)
    (let ((proc (start-process-shell-command
                 "grid-dired-thumb" nil (car cmds))))
      (set-process-sentinel
       proc
       (lambda (p _event)
         (when (memq (process-status p) '(exit signal))
           (if (and (eq (process-status p) 'exit)
                    (zerop (process-exit-status p))
                    (grid-dired--file-non-empty-p thumb))
               (funcall callback t)
             (grid-dired--try-shell-cmds (cdr cmds) thumb callback))))))))

(defun grid-dired--handler-ffmpeg (file thumb callback)
  "Create THUMB for media FILE with ffmpeg; report success via CALLBACK."
  (if (not (executable-find grid-dired-ffmpeg-program))
      (funcall callback nil)
    (grid-dired--try-shell-cmds
     (grid-dired--ffmpeg-cmds file thumb) thumb callback)))


;;; Quick Look and sips thumbnails (macOS)

(defun grid-dired--handler-quicklook (file thumb callback)
  "Create THUMB for FILE using macOS Quick Look; report via CALLBACK.
Quick Look writes BASENAME.png into a directory of our choosing, so
the result is converted/renamed to THUMB afterwards."
  (if (not (and (eq system-type 'darwin)
                (executable-find grid-dired-qlmanage-program)))
      (funcall callback nil)
    (let* ((tmpdir (make-temp-file "grid-dired-ql" t))
           (png (expand-file-name
                 (concat (file-name-nondirectory file) ".png") tmpdir))
           (proc (start-process
                  "grid-dired-qlmanage" nil grid-dired-qlmanage-program
                  "-t" "-s" (number-to-string (image-dired--thumb-size))
                  "-o" tmpdir (expand-file-name file))))
      (set-process-sentinel
       proc
       (lambda (p _event)
         (when (memq (process-status p) '(exit signal))
           (unwind-protect
               (funcall
                callback
                (and (grid-dired--file-non-empty-p png)
                     (if (equal (file-name-extension thumb) "png")
                         (progn (rename-file png thumb t) t)
                       ;; The inserted image spec assumes the thumbnail
                       ;; format matches its extension, so transcode.
                       (zerop (call-process
                               grid-dired-sips-program nil nil nil
                               "-s" "format" "jpeg" png "--out" thumb)))))
             (delete-directory tmpdir t))))))))

(defun grid-dired--handler-sips (file thumb callback)
  "Create THUMB for image FILE using macOS sips; report via CALLBACK.
Fallback for Macs without ImageMagick/GraphicsMagick."
  (if (not (and (eq system-type 'darwin)
                (executable-find grid-dired-sips-program)))
      (funcall callback nil)
    (let ((proc (start-process
                 "grid-dired-sips" nil grid-dired-sips-program
                 "-s" "format" (if (equal (file-name-extension thumb) "png")
                                   "png" "jpeg")
                 "-Z" (number-to-string (image-dired--thumb-size))
                 (expand-file-name file) "--out" thumb)))
      (set-process-sentinel
       proc
       (lambda (p _event)
         (when (memq (process-status p) '(exit signal))
           (funcall callback (and (eq (process-status p) 'exit)
                                  (zerop (process-exit-status p))
                                  (grid-dired--file-non-empty-p thumb)))))))))


;;; Placeholder cards (pure SVG, no external programs)

(defconst grid-dired--placeholder-palette
  '("#a86fc9" "#5b8dbf" "#58a878" "#c99a54" "#c96f6f"
    "#5fa8b8" "#8b8bc0" "#b87fa0" "#96a85a" "#c98354")
  "Accent colors for placeholder cards, picked by extension hash.")

(defvar grid-dired--placeholder-specs (make-hash-table :test 'equal)
  "Map of original file name to placeholder image spec.
Lets redisplays show the placeholder immediately instead of a broken
image while the (futile) thumbnail job re-runs.")

(defun grid-dired--placeholder-image (file)
  "Return an SVG image spec acting as a thumbnail card for FILE."
  (let* ((size (image-dired--thumb-size))
         (dir-p (file-directory-p file))
         (ext (or (grid-dired--file-extension file) ""))
         (label (cond (dir-p "")
                      ((string-empty-p ext) "?")
                      (t (upcase (truncate-string-to-width ext 6)))))
         (name (truncate-string-to-width
                (file-name-nondirectory (directory-file-name file))
                (max 8 (/ size 9)) nil nil "…"))
         (color (nth (mod (sxhash-equal (if dir-p "dir" ext))
                          (length grid-dired--placeholder-palette))
                     grid-dired--placeholder-palette))
         (svg (svg-create size size)))
    (svg-rectangle svg 0 0 size size :fill "#2b2e36" :rx (/ size 16))
    (if dir-p
        (progn                          ; folder: tab + body
          (svg-rectangle svg (* size 0.22) (* size 0.24)
                         (* size 0.30) (* size 0.14)
                         :fill color :rx (/ size 40))
          (svg-rectangle svg (* size 0.22) (* size 0.32)
                         (* size 0.56) (* size 0.36)
                         :fill color :rx (/ size 40)))
      ;; document: sheet + extension band
      (svg-rectangle svg (* size 0.30) (* size 0.16)
                     (* size 0.40) (* size 0.50)
                     :fill "#c9cdd6" :rx (/ size 40))
      (svg-rectangle svg (* size 0.20) (* size 0.42)
                     (* size 0.60) (* size 0.22)
                     :fill color :rx (/ size 40))
      (svg-text svg label
                :x (/ size 2.0) :y (* size 0.585)
                :text-anchor "middle"
                :fill "#ffffff" :font-weight "bold"
                :font-family "sans-serif" :font-size (/ size 6.5)))
    (svg-text svg name
              :x (/ size 2.0) :y (* size 0.86)
              :text-anchor "middle"
              :fill "#9aa0ad" :font-family "sans-serif"
              :font-size (/ size 10.0))
    (svg-image svg
               :relief image-dired-thumb-relief
               :margin image-dired-thumb-margin)))

(defun grid-dired--set-thumb-display (original-file image)
  "Show IMAGE for every thumbnail of ORIGINAL-FILE in the thumb buffer."
  (when-let ((buf (get-buffer image-dired-thumbnail-buffer)))
    (with-current-buffer buf
      (with-silent-modifications
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (and (get-text-property (point) 'image-dired-thumbnail)
                       (equal (get-text-property (point) 'original-file-name)
                              original-file))
              (put-text-property (point) (1+ (point)) 'display image))
            (forward-char)))))))

(defun grid-dired--handler-placeholder (file _thumb callback)
  "Show a generated placeholder card for FILE; report via CALLBACK.
No thumbnail file is written; the card is rendered in-buffer.  Returns
the symbol `no-file' through CALLBACK so the caller skips file-based
bookkeeping."
  (if (not (image-type-available-p 'svg))
      (funcall callback nil)
    (let ((image (grid-dired--placeholder-image file)))
      (puthash (expand-file-name file) image grid-dired--placeholder-specs)
      (grid-dired--set-thumb-display file image)
      (funcall callback 'no-file))))

(defun grid-dired--insert-thumbnail-advice (_file original-file-name _buf)
  "After a thumbnail insert, immediately reapply a known placeholder.
ORIGINAL-FILE-NAME is the file the thumbnail stands for."
  (when-let ((image (gethash (expand-file-name original-file-name)
                             grid-dired--placeholder-specs)))
    (put-text-property (1- (point)) (point) 'display image)))


;;; Thumbnail creation dispatch

(defun grid-dired--handlers-for (file)
  "Return thumbnail handlers to try for FILE, in order.
The symbol `image-dired' means: use the stock pipeline."
  (cond
   ;; Already known to need a placeholder: don't retry the heavier
   ;; handlers every time the grid is redisplayed or rebuilt.
   ((gethash (expand-file-name file) grid-dired--placeholder-specs)
    (list #'grid-dired--handler-placeholder))
   ((file-directory-p file)
    (list #'grid-dired--handler-placeholder))
   ((grid-dired--image-file-p file)
    (if (grid-dired--imagemagick-usable-p)
        'image-dired
      (list #'grid-dired--handler-sips
            #'grid-dired--handler-quicklook
            #'grid-dired--handler-placeholder)))
   ((grid-dired--media-file-p file)
    (list #'grid-dired--handler-ffmpeg
          #'grid-dired--handler-quicklook
          #'grid-dired--handler-placeholder))
   (t
    (list #'grid-dired--handler-quicklook
          #'grid-dired--handler-placeholder))))

(defun grid-dired--finish-job (file thumb ok)
  "Do image-dired queue bookkeeping after a thumbnail job for FILE.
THUMB is the thumbnail file; OK is nil (failure), t (THUMB written)
or `no-file' (success with no file, e.g. a placeholder)."
  (cl-decf image-dired-queue-active-jobs)
  (run-at-time 0 nil #'image-dired-thumb-queue-run)
  (cond
   ((null ok)
    (message "grid-dired: no thumbnail for %s" (abbreviate-file-name file)))
   ((and (eq ok t) (file-exists-p thumb))
    (set-file-modes thumb #o600)
    (clear-image-cache thumb))))

(defun grid-dired--run-handlers (handlers file thumb)
  "Try HANDLERS in order to create THUMB for FILE.
Each handler is called with (FILE THUMB CALLBACK) and must call
CALLBACK exactly once with nil, t or `no-file'."
  (if (null handlers)
      (grid-dired--finish-job file thumb nil)
    (condition-case err
        (funcall (car handlers) file thumb
                 (lambda (ok)
                   (if ok
                       (grid-dired--finish-job file thumb ok)
                     (grid-dired--run-handlers (cdr handlers) file thumb))))
      (error
       (message "grid-dired: handler %s failed for %s: %s"
                (car handlers) file (error-message-string err))
       (grid-dired--run-handlers (cdr handlers) file thumb)))))

(defun grid-dired--create-thumb-1-advice (orig-fun original-file thumbnail-file)
  "Create THUMBNAIL-FILE for ORIGINAL-FILE, dispatching on file type.
Image files keep the stock pipeline (ORIG-FUN); everything else goes
through grid-dired's handler chain."
  (let ((handlers (grid-dired--handlers-for original-file)))
    (if (eq handlers 'image-dired)
        (funcall orig-fun original-file thumbnail-file)
      (let ((dir (file-name-directory thumbnail-file)))
        (unless (file-exists-p dir)
          (with-file-modes #o700 (make-directory dir t))))
      (grid-dired--run-handlers handlers original-file thumbnail-file)
      nil)))


;;; Opening files from the thumbnail buffer

(defvar grid-dired--kill-buffer-on-display t
  "If non-nil, displaying a new file kills the previous display buffer.")

(defun grid-dired-open-externally (file)
  "Open FILE with the system's default application."
  (interactive (list (or (image-dired-original-file-name)
                         (user-error "No file at point"))))
  (cond
   (grid-dired-external-open-command
    (start-process "grid-dired-open" nil
                   grid-dired-external-open-command
                   (expand-file-name file)))
   ((fboundp 'w32-shell-execute)
    (w32-shell-execute "open" (expand-file-name file)))
   (t (user-error "`grid-dired-external-open-command' is not set"))))

(defun grid-dired--display-file-buffer (file)
  "Visit FILE normally and show it in the image-dired display window.
Unlike stock image-dired this does not visit the file raw/literally,
so major modes like ready-player get to handle it."
  (when-let ((buf (get-buffer image-dired-display-image-buffer)))
    (when grid-dired--kill-buffer-on-display
      (kill-buffer buf)))
  (let ((cur-win (selected-window))
        (buf (find-file-noselect file)))
    (pop-to-buffer buf)
    (unless (get-buffer image-dired-display-image-buffer)
      (rename-buffer image-dired-display-image-buffer))
    (select-window cur-win)))

(defun grid-dired--display-image-advice (orig-fun file &optional ignored)
  "Display FILE appropriately for its type.
Images and PDFs go through ORIG-FUN (stock image-dired); IGNORED is
its historical second argument.  Directories open in Dired, media
files open in ready-player or externally, everything else is simply
visited."
  (let ((file (expand-file-name file)))
    (cond
     ((file-directory-p file)
      (dired-other-window file))
     ((grid-dired--image-file-p file)
      (funcall orig-fun file ignored))
     ((grid-dired--media-file-p file)
      (if (and grid-dired-use-ready-player
               (fboundp 'ready-player-mode))
          (grid-dired--display-file-buffer file)
        (grid-dired-open-externally file)))
     ;; Don't dump huge unknown binaries into a buffer.
     ((let ((size (file-attribute-size (file-attributes file))))
        (and size large-file-warning-threshold
             (> size large-file-warning-threshold)))
      (grid-dired-open-externally file))
     (t
      (grid-dired--display-file-buffer file)))))


;;; Minor mode and entry points

(defun grid-dired-clear-placeholders ()
  "Forget all cached placeholder cards.
Useful after installing a new thumbnailer (ffmpeg etc.), so files
that previously fell back to a placeholder get retried."
  (interactive)
  (clrhash grid-dired--placeholder-specs))

;;;###autoload
(define-minor-mode grid-dired-mode
  "Global minor mode teaching Image-Dired about non-image files.
While enabled, image-dired commands (`image-dired',
`image-dired-display-thumbs', ...) accept video, audio and - when
`grid-dired-show-all-files' is non-nil - arbitrary files, generate
appropriate thumbnails for them, and open them sensibly from the
thumbnail buffer."
  :global t
  :group 'grid-dired
  (if grid-dired-mode
      (progn
        (advice-add 'image-dired--file-name-regexp :around
                    #'grid-dired--file-name-regexp-advice)
        (advice-add 'image-dired-create-thumb-1 :around
                    #'grid-dired--create-thumb-1-advice)
        (advice-add 'image-dired--check-executable-exists :around
                    #'grid-dired--check-executable-advice)
        (advice-add 'image-dired-display-image :around
                    #'grid-dired--display-image-advice)
        (advice-add 'image-dired-insert-thumbnail :after
                    #'grid-dired--insert-thumbnail-advice)
        (unless image-dired-external-viewer
          (setq image-dired-external-viewer
                grid-dired-external-open-command)))
    (advice-remove 'image-dired--file-name-regexp
                   #'grid-dired--file-name-regexp-advice)
    (advice-remove 'image-dired-create-thumb-1
                   #'grid-dired--create-thumb-1-advice)
    (advice-remove 'image-dired--check-executable-exists
                   #'grid-dired--check-executable-advice)
    (advice-remove 'image-dired-display-image
                   #'grid-dired--display-image-advice)
    (advice-remove 'image-dired-insert-thumbnail
                   #'grid-dired--insert-thumbnail-advice)))

;;;###autoload
(defun grid-dired (dir)
  "Browse DIR as a grid of thumbnails, including videos and other files.
Enables `grid-dired-mode' and runs `image-dired' on DIR.  See the
Commentary in grid-dired.el for the keys available in the resulting
buffer (they are the standard image-dired keys)."
  (interactive "DShow thumbnail grid for directory: ")
  (grid-dired-mode 1)
  (image-dired dir))

;;;###autoload
(defun grid-dired-display-marked ()
  "Display thumbnails for the marked files in the current Dired buffer.
Like `image-dired-display-thumbs', but with grid-dired's support for
videos and other file types enabled."
  (interactive nil dired-mode)
  (grid-dired-mode 1)
  (image-dired-display-thumbs))

(provide '--grid-dired__dired_media@@20260729T224128)

;; Optional integrations.  After `provide' so the glue file's
;; (require '--grid-dired__dired_media@@20260729T224128) is a no-op even when dired-narrow was loaded
;; before us and the form runs immediately.
(with-eval-after-load 'dired-narrow
  (require '--grid-dired-narrow__dired_media_navigation_search@@20260729T224121))

;;; --grid-dired__dired_media@@20260729T224128.el ends here
