;;; --use-lua-mode__lua_programming@@20251106T195609.el --- use-lua-mode -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-lua-mode
;; keywords: :lua:programming:
;; date: [2025-11-06 Thu 19:56]
;; identifier: 20251106T195609

;;; Code:
(use-package lua-mode
  :ensure t
  :config
  (setq lua-default-application "love")
  (setq lua-default-command-switches '("." "--headless")))

(provide '--use-lua-mode__lua_programming@@20251106T195609)
;;; --use-lua-mode__lua_programming@@20251106T195609.el ends here
