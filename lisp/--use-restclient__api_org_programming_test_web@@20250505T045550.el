;;; --use-restclient__api_org_programming_test_web@@20250505T045550.el --- use-restclient -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-restclient
;; keywords: :api:org:programming:test:web:
;; date: [2025-05-05 Mon 04:55]
;; identifier: 20250505T045550

;; ┌─────────┐
;; │ Example │
;; └─────────┘
;; #+begin_src restclient :async
;;   POST http://localhost:8000/users/login/
;;   Content-Type: application/json

;;   {
;;           "email": "bobo@baggins.com",
;;           "password": "123456"
;;   }
;; #+end_src
;;; Code:
(use-package restclient
  ;; see examples:
  ;; https://github.com/pashky/restclient.el/blob/master/examples/httpbin
  :ensure t)

(use-package ob-restclient
  :ensure t)

(provide '--use-restclient__api_org_programming_test_web@@20250505T045550)
;;; --use-restclient__api_org_programming_test_web@@20250505T045550.el ends here
