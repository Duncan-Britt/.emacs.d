;;; --plantuml__org_programming_systemsdesign@@20250503T082447.el --- use-plantuml-mode -*- lexical-binding: t -*-

;;; Commentary:
;; title: plantuml
;; keywords: :org:programming:systemsdesign:
;; date: [2025-05-03 Sat 08:24]
;; identifier: 20250503T082447

;; ┌──────────┐
;; │ PlantUML │
;; └──────────┘
;;; Code:
(use-package plantuml-mode
  :ensure t)

(use-package org
  :ensure nil
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/Downloads/installers/plantuml-mit-1.2025.1.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((plantuml . t)))))

(provide '--plantuml__org_programming_systemsdesign@@20250503T082447)
;;; --plantuml__org_programming_systemsdesign@@20250503T082447.el ends here
