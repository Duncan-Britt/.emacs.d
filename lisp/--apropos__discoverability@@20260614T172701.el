;;; --apropos__discoverability@@20260614T172701.el --- apropos -*- lexical-binding: t -*-

;;; Commentary:
;; title: apropos
;; keywords: :discoverability:
;; date: [2026-06-14 Sun 17:27]
;; identifier: 20260614T172701

;; https://karthinks.com/software/even-more-batteries-included-with-emacs/#the-apropos-family
;;; Code:
(defvar-keymap help-apropos-map
  :doc "Keymap for apropos subcommands."
  "a"   #'apropos
  "l"   #'apropos-library
  "f"   #'apropos-function
  "x"   #'apropos-command
  "v"   #'apropos-variable
  "V"   #'apropos-local-variable
  "u"   #'apropos-user-option
  "d"   #'apropos-documentation
  "C-f" #'customize-apropos-faces
  "g"   #'customize-apropos-groups
  "o"   #'customize-apropos-options
  "c"   #'customize-apropos
  "i"   #'info-apropos)
(keymap-set help-map "a" help-apropos-map)

(provide '--apropos__discoverability@@20260614T172701)
;;; --apropos__discoverability@@20260614T172701.el ends here
