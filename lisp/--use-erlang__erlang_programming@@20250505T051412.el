;;; --use-erlang__erlang_programming@@20250505T051412.el --- use-erlang -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-erlang
;; keywords: :erlang:programming:
;; date: [2025-05-05 Mon 05:14]
;; identifier: 20250505T051412

;; https://www.erlang.org/docs/24/man/erlang.el
;;; Code:
(use-package erlang
  :ensure t
  :hook ((erlang-mode . subword-mode)))

;; LSP: https://whatsapp.github.io/erlang-language-platform/
;; edts           Erlang Development Tool Suite
;; erlstack-mode  Minor mode for analysing Erlang stacktraces
;; lfe-mode       Flavoured Erlang mode
;; earl           Erlang distribution protocol implementation

(provide '--use-erlang__erlang_programming@@20250505T051412)
;;; --use-erlang__erlang_programming@@20250505T051412.el ends here
