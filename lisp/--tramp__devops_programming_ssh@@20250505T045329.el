;;; --tramp__devops_programming_ssh@@20250505T045329.el --- tramp -*- lexical-binding: t -*-

;;; Commentary:
;; title: tramp
;; keywords: :devops:programming:ssh:
;; date: [2025-05-05 Mon 04:53]
;; identifier: 20250505T045329

;;; Code:
(use-package tramp
  :ensure nil
  :config
  ;; ┌───────────────────────────────────────────────────────────────────┐
  ;; │ [2025-09-18] I'm using tramp for my VPS. Since I'm the only one   │
  ;; │ who changes anything, I can be safe using the cached version of   │
  ;; │ the files since the files won't change unless I change them. I    │
  ;; │ won't have other emacs sessions editing the same files, so I can  │
  ;; │ disable locks. And I'm not using version control for anything     │
  ;; │ remotely,or if I do, I don't need the vc system in emacs to help. │
  ;; └───────────────────────────────────────────────────────────────────┘
  (setq remote-file-name-inhibit-cache nil
        remote-file-name-inhibit-locks t
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp)
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t
        tramp-copy-size-limit (* 1024 1024) ;; 1 MB
        tramp-verbose 2)

  ;; (add-to-list
  ;;  'tramp-methods
  ;;  '("gcssh"
  ;;    (tramp-login-program "gcloud compute ssh --zone us-central1-c")
  ;;    (tramp-login-args (("%h")))
  ;;    (tramp-async-args (("-q")))
  ;;    (tramp-remote-shell "/bin/sh")
  ;;    (tramp-remote-shell-args ("-c"))
  ;;    (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null")
  ;;                    ("-o" "UserKnownHostsFile=/dev/null")
  ;;                    ("-o" "StrictHostKeyChecking=no")))
  ;;    (tramp-default-port 22)))
  )

(provide '--tramp__devops_programming_ssh@@20250505T045329)
;;; --tramp__devops_programming_ssh@@20250505T045329.el ends here
