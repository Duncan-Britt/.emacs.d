;;; --use-gptel__programming_prose@@20250503T075715.el --- use-gptel -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-gptel
;; keywords: :programming:prose:
;; date: [2025-05-03 Sat 07:57]
;; identifier: 20250503T075715

;; ┌────────┐
;; │ LLM UI │
;; └────────┘
;;; Code:
(use-package gptel
  :after transient
  :ensure t
  :bind (("C-c RET" . gptel-send))
  :config
  (require 'safe)

  (setq gptel--known-backends (assoc-delete-all "ChatGPT" gptel--known-backends))

  ;; (setq gptel-model 'claude-3-7-sonnet-20250219
  ;;       gptel-backend
  ;;       (gptel-make-anthropic "Claude"
  ;;         :stream t :key *a-gptel-token*))

  (gptel-make-anthropic "Claude"
          :stream t :key *a-gptel-token*)

  (gptel-make-gemini "Gemini"
    :key *gem* ;; :key can be a function that returns the API key.
    :stream t)

  (setq gptel-model 'deepseek-coder
        gptel-backend
        (gptel-make-openai "DeepSeek" ; Any name you want
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key *deep-token* ; can be a function that returns the key
          :models '(deepseek-chat deepseek-coder)))

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(qwen2.5-coder:14b qwen2.5-coder:32b aya:latest))

  (setq gptel-use-tools nil)
  (setq gptel-default-mode 'org-mode))
(provide '--use-gptel__programming_prose@@20250503T075715)
;;; --use-gptel__programming_prose@@20250503T075715.el ends here
