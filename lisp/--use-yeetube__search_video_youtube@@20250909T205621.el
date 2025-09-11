;;; --use-yeetube__search_video_youtube@@20250909T205621.el --- use-yeetube -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-yeetube
;; keywords: :search:video:youtube:
;; date: [2025-09-09 Tue 20:56]
;; identifier: 20250909T205621

;;; Code:
;; ┌──────────────────────────────────────────────────────────────────────────┐
;; │ NOTE: I'm not using this for now because of concerns about getting my IP │
;; │ address blocked for scraping, because, technically, yeetube scrapes      │
;; │ the results from YouTube, and I'm pretty sure this would be a            │
;; │ violation of their terms of service.                                     │
;; └──────────────────────────────────────────────────────────────────────────┘
;; (use-package yeetube
;;   :ensure (:url "https://codeberg.org/ThanosApollo/emacs-yeetube")
;;   :init (define-prefix-command 'my/yeetube-map)
;;   :config
;;   (setf yeetube-mpv-disable-video nil) ;; Disable video output
;;   (setf yeetube-mpv-video-quality "480")
;;   :bind (("C-c y" . 'my/yeetube-map)
;;           :map my/yeetube-map
;; 		  ("s" . 'yeetube-search)
;; 		  ("b" . 'yeetube-play-saved-video)
;; 		  ("d" . 'yeetube-download-videos)
;; 		  ("p" . 'yeetube-mpv-toggle-pause)
;; 		  ("v" . 'yeetube-mpv-toggle-video)
;; 		  ("V" . 'yeetube-mpv-toggle-no-video-flag)
;; 		  ("k" . 'yeetube-remove-saved-video)))

(provide '--use-yeetube__search_video_youtube@@20250909T205621)
;;; --use-yeetube__search_video_youtube@@20250909T205621.el ends here
