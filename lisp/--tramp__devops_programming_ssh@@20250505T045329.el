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
  (add-to-list
   'tramp-methods
   '("gcssh"
     (tramp-login-program "gcloud compute ssh --zone us-central1-c")
     (tramp-login-args (("%h")))
     (tramp-async-args (("-q")))
     (tramp-remote-shell "/bin/sh")
     (tramp-remote-shell-args ("-c"))
     (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null")
                     ("-o" "UserKnownHostsFile=/dev/null")
                     ("-o" "StrictHostKeyChecking=no")))
     (tramp-default-port 22))))

(provide '--tramp__devops_programming_ssh@@20250505T045329)
;;; --tramp__devops_programming_ssh@@20250505T045329.el ends here
