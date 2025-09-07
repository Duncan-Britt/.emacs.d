;;; --my-org-tags-link__links_org_organization_search@@20250907T073056.el --- my-org-tags-link -*- lexical-binding: t -*-

;;; Commentary:
;; title: my-org-tags-link
;; keywords: :links:org:organization:search:
;; date: [2025-09-07 Sun 07:30]
;; identifier: 20250907T073056

;;; Code:
(defun my/org-tags-link (tags-string link-text todo-only)
  "Insert org-mode link text in buffer at point matching tags
defined in TAGS-STRING. Link, when visited, will produce a sparse
tree of org headings matching TAGS-STRING.

When called interactively, prompt the user for tags."
  (interactive
   (let* ((buffer-tags (mapcan #'identity (org-get-buffer-tags)))
          (tags-list-included
           (completing-read-multiple "With tag(s): " buffer-tags nil t nil))
          (tags-list-excluded
           (completing-read-multiple "Excluding tag(s): " buffer-tags nil t nil)))
     (list
      ;; tags-string
      (concat (apply #'concat
                     (mapcar (lambda (tag)
                               (concat "+" tag))
                             tags-list-included))
              (apply #'concat
                     (mapcar (lambda (tag)
                               (concat "-" tag))
                             tags-list-excluded)))
      ;; link-text
      (read-string "Description: ")
      ;; todo only?
      (intern (completing-read "Todo only? " '(t nil))))))
  (insert (format "[[elisp:(org-match-sparse-tree %s \"%s\")][%s]]" todo-only tags-string link-text)))

;; (my/org-tags-link "+music+life+architecture+society-beauty-learning-language" "Test description" t)[[elisp:(org-match-sparse-tree t "+music+life+architecture+society-beauty-learning-language")][Test description]] ;;=> nil

(provide '--my-org-tags-link__links_org_organization_search@@20250907T073056)
;;; --my-org-tags-link__links_org_organization_search@@20250907T073056.el ends here
