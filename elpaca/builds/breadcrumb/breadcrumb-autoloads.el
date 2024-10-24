;;; breadcrumb-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from breadcrumb.el

(autoload 'breadcrumb-imenu-crumbs "breadcrumb" "\
Describe point inside the Imenu tree of current file.")
(autoload 'breadcrumb-project-crumbs "breadcrumb" "\
Describing the current file inside project.")
(autoload 'breadcrumb-local-mode "breadcrumb" "\
Header lines with breadcrumbs.

This is a minor mode.  If called interactively, toggle the
`Breadcrumb-Local mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `breadcrumb-local-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(put 'breadcrumb-mode 'globalized-minor-mode t)
(defvar breadcrumb-mode nil "\
Non-nil if Breadcrumb mode is enabled.
See the `breadcrumb-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `breadcrumb-mode'.")
(custom-autoload 'breadcrumb-mode "breadcrumb" nil)
(autoload 'breadcrumb-mode "breadcrumb" "\
Toggle Bc-Local mode in all buffers.
With prefix ARG, enable Breadcrumb mode if ARG is positive; otherwise,
disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Bc-Local mode is enabled in all buffers where
`bc--turn-on-local-mode-on-behalf-of-global-mode' would do it.

See `bc-local-mode' for more information on Bc-Local mode.

(fn &optional ARG)" t)
(autoload 'breadcrumb-jump "breadcrumb" "\
Like \\[execute-extended-command] `imenu', but breadcrumb-powered." t)
 (register-definition-prefixes "breadcrumb" '("breadcrumb-"))

;;; End of scraped data

(provide 'breadcrumb-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; breadcrumb-autoloads.el ends here
