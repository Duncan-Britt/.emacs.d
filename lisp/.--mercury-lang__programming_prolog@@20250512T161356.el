;;; --mercury-lang__programming_prolog@@20250512T161356.el --- mercury-lang -*- lexical-binding: t -*-

;;; Commentary:
;; title: mercury-lang
;; keywords: :programming:prolog:
;; date: [2025-05-12 Mon 16:13]
;; identifier: 20250512T161356

;; ┌─────────┐
;; │ Mercury │
;; └─────────┘
;;; Code:
;; (add-to-list 'load-path
;; 	     "/usr/local/mercury-22.01.8/lib/mercury/elisp")
;; (autoload 'mdb "gud" "Invoke the Mercury debugger" t)
;; (with-eval-after-load 'info
;;   (add-to-list 'Info-directory-list "/usr/local/mercury-22.01.8/share/info"))

;; (add-to-list 'auto-mode-alist '("\\.m\\'" . mercury-mode))
;; (autoload 'mercury-mode "mercury-mode" "Mercury mode" t)

;; (pp Info-directory-list) =>
;; ("/Users/duncan/.emacs.d/elpaca/builds/hyperbole/man/"
;; "/Users/duncan/.emacs.d/elpaca/builds/sweeprolog"
;; "/Users/duncan/.emacs.d/elpaca/builds/rg"
;; "/Users/duncan/.emacs.d/elpaca/builds/orderless"
;; "/Users/duncan/.emacs.d/elpaca/builds/magit"
;; "/Users/duncan/.emacs.d/elpaca/builds/magit-section"
;; "/Users/duncan/.emacs.d/elpaca/builds/with-editor"
;; "/Users/duncan/.emacs.d/elpaca/builds/hyperbole"
;; "/Users/duncan/.emacs.d/elpaca/builds/embark"
;; "/Users/duncan/.emacs.d/elpaca/builds/casual"
;; "/Users/duncan/.emacs.d/elpaca/builds/transient"
;; "/Users/duncan/.emacs.d/elpaca/builds/dash"
;; "/Users/duncan/.emacs.d/elpaca/builds/compat"
;; "/Users/duncan/.emacs.d/elpaca/builds/elpaca"
;; "/Users/duncan/Downloads/installers/emacs-emacs-29.4/info"
;; "/usr/local/mercury-22.01.8/share/info"
;; "/Users/duncan/Downloads/installers/emacs-emacs-29.4/info/"
;; "/Users/duncan/Downloads/installers/emacs-emacs-29.4/info/"
;; "/usr/local/share/info/")
;; "(\"/Users/duncan/.emacs.d/elpaca/builds/hyperbole/man/\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/sweeprolog\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/rg\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/orderless\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/magit\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/magit-section\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/with-editor\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/hyperbole\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/embark\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/casual\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/transient\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/dash\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/compat\"
;; \"/Users/duncan/.emacs.d/elpaca/builds/elpaca\"
;; \"/Users/duncan/Downloads/installers/emacs-emacs-29.4/info\"
;; \"/usr/local/mercury-22.01.8/share/info\"
;; \"/Users/duncan/Downloads/installers/emacs-emacs-29.4/info/\"
;; \"/Users/duncan/Downloads/installers/emacs-emacs-29.4/info/\"
;; \"/usr/local/share/info/\") "

;; -- Installation complete.

;; -- Don't forget to add /usr/local/mercury-22.01.8/bin to your PATH,
;; -- /usr/local/mercury-22.01.8/share/man to your MANPATH,
;; -- and /usr/local/mercury-22.01.8/share/info to your INFOPATH,
;; -- and to add the following lines to the `.emacs' file
;; -- in your home directory:
;; 	(add-to-list 'load-path
;; 		"/usr/local/mercury-22.01.8/lib/mercury/elisp")
;;         (autoload 'mdb "gud" "Invoke the Mercury debugger" t)

;; I just did the emacs stuff above, but can you help me with path stuff?
;; I'm using zsh (.zshrc) on my mac. I don't know what Manpath and infopath are.
;; But I'd like to learn more and understand.

;; To add the Mercury paths to your zsh configuration, add these lines to your `.zshrc`:

;; ```zsh
;; # Mercury paths
;; export PATH="/usr/local/mercury-22.01.8/bin:$PATH"
;; export MANPATH="/usr/local/mercury-22.01.8/share/man:$MANPATH"
;; export INFOPATH="/usr/local/mercury-22.01.8/share/info:$INFOPATH"
;; ```

;; About these paths:
;; - `PATH`: Where your shell looks for executable programs
;; - `MANPATH`: Where the `man` command looks for manual pages
;; - `INFOPATH`: Where the GNU Info system looks for documentation

;; After adding these to your `.zshrc`, run `source ~/.zshrc` to apply the changes to your current terminal session.

;; How can I see my MANPATH and INFOPATH ?

;; To check your current paths, use these commands in your terminal:

;; ```zsh
;; echo $MANPATH    # Shows manual page paths
;; echo $INFOPATH   # Shows info documentation paths
;; echo $PATH       # Shows executable search paths
;; ```

;; If `MANPATH` or `INFOPATH` are empty, they may be inheriting defaults from system paths. You can verify with:

;; ```zsh
;; man -w           # Shows effective manpath
;; info --path      # Shows effective infopath
;; ```

;; (provide '--mercury-lang__programming_prolog@@20250512T161356)
;;; --mercury-lang__programming_prolog@@20250512T161356.el ends here
