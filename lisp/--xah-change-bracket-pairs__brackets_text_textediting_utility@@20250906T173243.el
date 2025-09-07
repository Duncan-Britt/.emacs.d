;;; --xah-change-bracket-pairs__brackets_text_textediting_utility@@20250906T173243.el --- xah-change-bracket-pairs -*- lexical-binding: t -*-

;;; Commentary:
;; title: xah-change-bracket-pairs
;; keywords: :brackets:text:textediting:utility:
;; date: [2025-09-06 Sat 17:32]
;; identifier: 20250906T173243

;;; Code:
(defun xah-change-bracket-pairs (FromChars ToChars)
  "Change bracket pairs to another type or none.
For example, change all parenthesis () to square brackets [].
Works on current block or selection.

In lisp code, FromChars is a string with at least 2 spaces.
e.g.
paren ( )
french angle ‹ ›
double bracket [[ ]]
etc.
It is split by space, and last 2 items are taken as left and right brackets.

ToChars is similar, with a special value of
none
followed by 2 spaces.
,it means replace by empty string.

URL `http://xahlee.info/emacs/emacs/elisp_change_brackets.html'
Created: 2020-11-01
Version: 2025-03-25"
  (interactive
   (let ((xbrackets
          '(
            "square [ ]"
            "brace { }"
            "paren ( )"
            "less greater than < >"
            "QUOTATION MARK \" \""
            "APOSTROPHE ' '"
            "emacs ` '"
            "GRAVE ACCENT ` `"
            "double square [[ ]]"
            "tilde ~ ~"
            "equal = ="
            "double curly quote “ ”"
            "single curly quote ‘ ’"
            "french angle quote ‹ ›"
            "french double angle « »"
            "corner 「 」"
            "white corner 『 』"
            "lenticular 【 】"
            "white lenticular 〖 〗"
            "title angle 〈 〉"
            "double angle 《 》"
            "tortoise 〔 〕"
            "white tortoise 〘 〙"
            "white square 〚 〛"
            "white paren ⦅ ⦆"
            "white curly bracket ⦃ ⦄"
            "pointing angle 〈 〉"
            "angle with dot ⦑ ⦒"
            "curved angle ⧼ ⧽"
            "math square ⟦ ⟧"
            "math angle ⟨ ⟩"
            "math double angle ⟪ ⟫"
            "math flattened parenthesis ⟮ ⟯"
            "math white tortoise shell ⟬ ⟭"
            "heavy single quotation mark ornament ❛ ❜"
            "heavy double turned comma quotation mark ornament ❝ ❞"
            "medium parenthesis ornament ❨ ❩"
            "medium flattened parenthesis ornament ❪ ❫"
            "medium curly ornament ❴ ❵"
            "medium pointing angle ornament ❬ ❭"
            "heavy pointing angle quotation mark ornament ❮ ❯"
            "heavy pointing angle ornament ❰ ❱"
            "none  "
            )))
     (let ((completion-ignore-case t))
       (list
        (completing-read "Replace this:" xbrackets nil t nil nil (car xbrackets))
        (completing-read "To:" xbrackets nil t nil nil (car (last xbrackets)))))))
  (let (xbeg xend xleft xright xtoL xtoR)
    (seq-setq (xbeg xend) (if (region-active-p) (list (region-beginning) (region-end)) (list (save-excursion (if (re-search-backward "\n[ \t]*\n" nil 1) (match-end 0) (point))) (save-excursion (if (re-search-forward "\n[ \t]*\n" nil 1) (match-beginning 0) (point))))))
    (let ((xsFrom (last (split-string FromChars " ") 2))
          (xsTo (last (split-string ToChars " ") 2)))

      ;; (when (< (length xsFrom) 3)
      ;; (error "cannot find input brackets %s" xsFrom))

      ;; (when (< (length xsTo) 3)
      ;;   (message "replace blacket is empty string")
      ;;   (setq xsTo (list "" "" "")))

      (setq xleft (car xsFrom)  xright (car (cdr xsFrom))
            xtoL (car xsTo) xtoR (car (cdr xsTo)))

      (save-excursion
        (save-restriction
          (narrow-to-region xbeg xend)
          (let ((case-fold-search nil))
            (if (string-equal xleft xright)
                (let ((xx (regexp-quote xleft)))
                  (goto-char (point-min))
                  (while
                      (re-search-forward
                       (format "%s\\([^%s]+?\\)%s" xx xx xx)
                       nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match (concat xtoL "\\1" xtoR) t)))
              (progn
                (progn
                  (goto-char (point-min))
                  (while (search-forward xleft nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match xtoL t t)))
                (progn
                  (goto-char (point-min))
                  (while (search-forward xright nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match xtoR t t)))))))))))
(provide '--xah-change-bracket-pairs__brackets_text_textediting_utility@@20250906T173243)
;;; --xah-change-bracket-pairs__brackets_text_textediting_utility@@20250906T173243.el ends here
