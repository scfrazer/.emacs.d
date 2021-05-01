;;; dump-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(defgroup dump-mode nil
  "*DUMP log mode."
  :group 'compilation)

(defcustom dump-mode-hook nil
  "*List of functions to call on entry to dump-mode mode."
  :group 'dump-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

(defvar dump-mode-font-lock-keywords
  '(
    ("\\(^[a-zA-Z_][a-zA-Z0-9_]+\\):"
     (1 'font-lock-function-name-face))
    ("^[0-9]+:.+"
     (0 'font-lock-comment-face))
    ("^\\s-+\\([a-fA-F0-9]+\\):\\s-+\\(\\([a-fA-F0-9]+\\s-+\\)+\\)\\('.+'\\)"
     (1 'font-lock-string-face)
     (2 'font-lock-builtin-face)
     (4 'font-lock-string-face))
    ("^\\s-+\\([a-fA-F0-9]+\\):\\s-+\\(\\([a-fA-F0-9]+\\s-+\\)+\\)\\([a-zA-Z_.][a-zA-Z0-9_.]+\\)"
     (1 'font-lock-string-face)
     (2 'font-lock-builtin-face)
     (4 'font-lock-keyword-face))
    ("%[a-zA-Z0-9_]+"
     (0 'font-lock-variable-name-face))
    (";.+"
     (0 'font-lock-comment-face))
    )
  "Font locking for 'dump-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar dump-mode-map nil "'dump-mode' keymap.")
(if (not dump-mode-map)
    (let ((map (make-keymap)))
      (setq dump-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar dump-mode-syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)

    (modify-syntax-entry ?_ "_" table)

    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?` "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?~ "." table)

    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?/  ". 12" table)
    (modify-syntax-entry ?\n ">" table)

    table)
  "Syntax table used in dump-mode buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defun dump-mode ()
  "dump-mode is a major mode for browsing ARC dump files.\n\n
\\{dump-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'dump-mode)
  (setq mode-name "dump")

  (set-syntax-table dump-mode-syntax-table)

  (use-local-map dump-mode-map)

  (setq comment-start ";")

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(dump-mode-font-lock-keywords t))
  (turn-on-font-lock)
  (font-lock-ensure)

  (setq truncate-lines t)
  (setq imenu-generic-expression (list '(nil "\\(^[a-zA-Z_][a-zA-Z0-9_]+\\):" 1)))

  (run-mode-hooks 'dump-mode-hook))

(provide 'dump-mode)
