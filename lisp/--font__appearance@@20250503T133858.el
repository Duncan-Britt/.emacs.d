;;; --font__appearance@@20250503T133858.el --- font -*- lexical-binding: t -*-

;;; Commentary:
;; title: font
;; keywords: :appearance:
;; date: [2025-05-03 Sat 13:38]
;; identifier: 20250503T133858

;;; Code:
(require '--portable__utility@@20250505T200406)

(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'org-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                      ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                      "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                      "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                      "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                      "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                      "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                      "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                      ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                      "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                      "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                      "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                      "\\\\" "://"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (ligature-set-ligatures 'tla+-mode '("<>" "<=" ">=" "~>" "/=" "/\\" "\\/" "=>" "==" "->"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((Iosevka-regular
           :default-height 130
           :default-family "Iosevka Duncan"
           :fixed-pitch-family "Iosevka Duncan"
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-height 1.0
           :mode-line-active-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion"
           :header-line-height 1.0)
          (Iosevka-share-screen
           :default-height 150
           :default-family "Iosevka Duncan"
           :fixed-pitch-family "Iosevka Duncan"
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-height 1.0
           :mode-line-active-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion"
           :header-line-height 1.0)
          (Adelle-regular
           :default-height 130
           :default-family "Iosevka Duncan"
           :fixed-pitch-family "Iosevka Duncan"
           :fixed-pitch-height 1.0
           :org-level-1-family "Symbola" ;; <-- Requires `fontaine-org'.
           :org-level-2-family "Symbola"
           :org-level-3-family "Symbola"
           :org-level-4-family "Symbola"
           :org-level-5-family "Symbola"
           :org-level-6-family "Symbola"
           :org-level-7-family "Symbola"
           :org-level-8-family "Symbola"
           :variable-pitch-family  "Adelle" ;"Symbola" ;"Antykwa Poltawskiego"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :mode-line-active-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion"
           :header-line-height 1.0)
          (Adelle-share-screen
           :default-height 150
           :default-family "Iosevka Duncan"
           :fixed-pitch-family "Iosevka Duncan"
           :fixed-pitch-height 1.0
           :org-level-1-family "Symbola" ;; <-- Requires `fontaine-org'.
           :org-level-2-family "Symbola"
           :org-level-3-family "Symbola"
           :org-level-4-family "Symbola"
           :org-level-5-family "Symbola"
           :org-level-6-family "Symbola"
           :org-level-7-family "Symbola"
           :org-level-8-family "Symbola"
           :variable-pitch-family  "Adelle" ;"Symbola" ;"Antykwa Poltawskiego"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :mode-line-active-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion"
           :header-line-height 1.0)
          (Athelas-regular
           :default-height 130
           :default-family "Iosevka Duncan"
           :fixed-pitch-family "Iosevka Duncan"
           :fixed-pitch-height 1.0
           :org-level-1-family "Symbola" ;; <-- Requires `fontaine-org'.
           :org-level-2-family "Symbola"
           :org-level-3-family "Symbola"
           :org-level-4-family "Symbola"
           :org-level-5-family "Symbola"
           :org-level-6-family "Symbola"
           :org-level-7-family "Symbola"
           :org-level-8-family "Symbola"
           :variable-pitch-family  "Athelas" ;"ETBembo"
           :variable-pitch-height 1.0
           :mode-line-active-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion"
           :header-line-height 1.0)
          (Athelas-share-screen
           :default-height 150
           :default-family "Iosevka Duncan"
           :fixed-pitch-family "Iosevka Duncan"
           :fixed-pitch-height 1.0
           :org-level-1-family "Symbola" ;; <-- Requires `fontaine-org'.
           :org-level-2-family "Symbola"
           :org-level-3-family "Symbola"
           :org-level-4-family "Symbola"
           :org-level-5-family "Symbola"
           :org-level-6-family "Symbola"
           :org-level-7-family "Symbola"
           :org-level-8-family "Symbola"
           :variable-pitch-family  "Athelas" ;"ETBembo"
           :variable-pitch-height 1.0
           :mode-line-active-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-active-height 1.0
           :mode-line-inactive-family "Iosevka Comfy Motion" ; falls back to :default-family
           :mode-line-inactive-height 1.0
           :header-line-family "Iosevka Comfy Motion"
           :header-line-height 1.0)))
  (fontaine-mode 1))

(provide '--font__appearance@@20250503T133858)
;;; --font__appearance@@20250503T133858.el ends here
