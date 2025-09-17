;;; --use-directory-slideshow__media@@20250503T072035.el --- use-directory-slideshow -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-directory-slideshow
;; keywords: :media:
;; date: [2025-05-03 Sat 07:20]
;; identifier: 20250503T072035

;;; Code:
(require '--portable__utility@@20250505T200406)

(use-package-local-or-remote
 directory-slideshow
 "~/code/my-emacs-packages/directory-slideshow/"
 "Duncan-Britt/directory-slideshow"
 :custom
 ((directory-slideshow-include-directories? t)
  (directory-slideshow-preview-next-slide? nil))
 :config
 (defun my/slideshow-text-adjustment ()
   (if (derived-mode-p 'text-mode)
       (progn
         (text-scale-set 2)
         (olivetti-mode 1)
         (olivetti-set-width 60))
     (when (derived-mode-p 'prog-mode)
       (text-scale-set 1))))

 (add-hook 'directory-slideshow-after-slide-render-hook #'my/slideshow-text-adjustment))

(provide '--use-directory-slideshow__media@@20250503T072035)
;;; --use-directory-slideshow__media@@20250503T072035.el ends here
