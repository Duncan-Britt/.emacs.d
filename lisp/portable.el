;;; portable.el --- Make config portable between my machines. -*- lexical-binding: t; -*-

;;; Commentary:
;; ┌───────────────────────────────┐
;; │ Portability & Synchronization │
;; └───────────────────────────────┘
;;; Code:

(defmacro use-package-local-or-remote (package-name local-path remote-repo &rest use-package-args)
  `(if (file-directory-p ,local-path)
       (use-package ,package-name
         :ensure (:repo ,local-path)
         ,@use-package-args)
     (use-package ,package-name
         :ensure (:host github :repo ,remote-repo)
         ,@use-package-args)))

(defmacro use-package-when-local (package-name local-path &rest use-package-args)
  `(when (file-directory-p ,local-path)
     (use-package ,package-name
       :ensure (:repo ,local-path)
       ,@use-package-args)))

(global-auto-revert-mode 1)

(provide 'portable)
