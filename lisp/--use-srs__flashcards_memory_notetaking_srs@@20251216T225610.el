;;; --use-srs__flashcards_memory_notetaking_srs@@20251216T225610.el --- use-flashcard -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-srs
;; keywords: :flashcards:memory:notetaking:srs:
;; date: [2025-12-16 Tue 22:56]
;; identifier: 20251216T225610

;;; Code:
(use-package srs
  :ensure (:host github :repo "Duncan-Britt/srs.el")
  :config
  (add-to-list 'srs-path-list (expand-file-name "~/notes/*.org"))
  (add-to-list 'srs-path-list (expand-file-name "~/code/erlang/*")))

(provide '--use-srs__flashcards_memory_notetaking_srs@@20251216T225610)
;;; --use-srs__flashcards_memory_notetaking_srs@@20251216T225610.el ends here
