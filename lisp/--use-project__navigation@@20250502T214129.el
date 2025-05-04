;;; --use-project__navigation@@20250502T214129.el --- use-project -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-project
;; keywords: :navigation:
;; date: [2025-05-02 Fri 21:41]
;; identifier: 20250502T214129

;;; Code:
(use-package project
  :ensure nil
  :config
  ;; (global-unset-key (kbd "C-x p"))
  (global-set-key (kbd "s-p") project-prefix-map)
  ;; TODO currently there is s-p ! for project-shell-command but this
  ;; is not powerful like C-u M-x `comint-run'. For instance I cannot
  ;; run a repl this way, or start the rebar3 shell this way.  So I
  ;; want to create a command `my/project-comint-run' and bind it to
  ;; something like s-p r (which currently is project-replace regex ->
  ;; need to move that first).
  ;; NOTE: this todo would be obviated by my upcomoing `shellx' package.
  )

(provide '--use-project__navigation@@20250502T214129)
;;; --use-project__navigation@@20250502T214129.el ends here
