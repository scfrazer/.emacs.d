;;; as-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(defgroup as-mode nil
  "*AS log mode."
  :group 'compilation)

(defcustom as-mode-hook nil
  "*List of functions to call on entry to as-mode mode."
  :group 'as-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

(defface as-mode-source-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Source name"
  :group 'faces)

(defface as-mode-dest-name-face
  '((t (:inherit font-lock-constant-face)))
  "Destination name"
  :group 'faces)

(defvar as-mode-font-lock-keywords
  '(
    ("\\(^[a-zA-Z_][a-zA-Z0-9_]+\\):"
     (1 'font-lock-function-name-face))
    ("^\\s-*\\(\\.[a-zA-Z_][a-zA-Z0-9_]+\\)"
     (1 'font-lock-preprocessor-face))
    ("^\\s-*\\([a-zA-Z][a-zA-Z0-9_.]+\\)"
     (1 'font-lock-keyword-face))
    ("\\*[a-zA-Z0-9_]+"
     (0 'as-mode-source-name-face))
    ("\\^[a-zA-Z0-9_]+"
     (0 'as-mode-dest-name-face))
    ("#.*"
     (0 'font-lock-comment-face))
    )
  "Font locking for 'as-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar as-mode-map nil "'as-mode' keymap.")
(if (not as-mode-map)
    (let ((map (make-keymap)))
      (setq as-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar as-mode-syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)

    (modify-syntax-entry ?_ "_" table)

    (modify-syntax-entry ?! "." table)
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
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?` "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?~ "." table)

    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?/  ". 12" table)
    (modify-syntax-entry ?\n ">" table)

    table)
  "Syntax table used in as-mode buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defun as-mode ()
  "as-mode is a major mode for browsing assembler files.\n\n
\\{as-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'as-mode)
  (setq mode-name "as")

  (set-syntax-table as-mode-syntax-table)

  (use-local-map as-mode-map)

  (setq comment-start "#")

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(as-mode-font-lock-keywords t))
  (turn-on-font-lock)
  (font-lock-ensure)

  (setq truncate-lines t)
  (setq imenu-generic-expression (list '(nil "\\(^[a-zA-Z_][a-zA-Z0-9_]+\\):" 1)))

  (run-mode-hooks 'as-mode-hook))

(provide 'as-mode)
