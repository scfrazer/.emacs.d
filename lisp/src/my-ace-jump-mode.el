;;; my-ace-jump-mode.el

(require 'ace-jump-mode)

(defvar my-ace-jump-syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)

    (modify-syntax-entry ?_ "_" table)

    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?#  "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?/  "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?` "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?~ "." table)

    table))

(defadvice ace-jump-do (around my-ace-jump-do activate)
  (with-syntax-table my-ace-jump-syntax-table
    ad-do-it))

(provide 'my-ace-jump-mode)
