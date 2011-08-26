;;; btext-mode.el

(defvar btext-syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?_ "_" table)

    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?~ "." table)

    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?# "<" table)

    table)
  "Syntax table used in basic text buffers.")

(define-derived-mode btext-mode fundamental-mode "btext"
  "Mode for editing basic text with a sane syntax table"
  :abbrev-table nil
  :syntax-table btext-syntax-table
  (setq comment-start "#")
  (turn-on-font-lock)
  (font-lock-fontify-buffer))

(add-to-list 'auto-mode-alist '("\\.txt" . btext-mode))
(add-to-list 'magic-fallback-mode-alist (cons "." 'btext-mode) 'append)

(provide 'btext-mode)
