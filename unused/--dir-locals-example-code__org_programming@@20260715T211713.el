;;; --dir-locals-example-code__org_programming@@20260715T211713.el --- dir-locals-example-code -*- lexical-binding: t -*-

;;; Commentary:
;; title: dir-locals-example-code
;; keywords: :org:programming:
;; date: [2026-07-15 Wed 21:17]
;; identifier: 20260715T211713

;;; Code:
((org-mode . ((eval . (add-hook 'org-insert-heading-hook
                        (lambda ()
                          (save-excursion
                            (org-back-to-heading)
                            (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %T]"))))
                        nil 'local)))))
(provide '--dir-locals-example-code__org_programming@@20260715T211713)
;;; --dir-locals-example-code__org_programming@@20260715T211713.el ends here
