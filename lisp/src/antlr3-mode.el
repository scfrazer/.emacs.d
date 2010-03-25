;;; antlr3-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(defgroup antlr3-mode nil
  "*antlr3 mode."
  :group 'programming)

(defcustom antlr3-indent-offset 4
  "*Indentation offset for `antlr3-mode'"
  :group 'antlr3-mode
  :type 'integer)

(defcustom antlr3-mode-hook nil
  "*List of functions to call on entry to `antlr3-mode'."
  :group 'antlr3-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar antlr3-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?\= "." table)
    (modify-syntax-entry ?\@ "." table)
    (modify-syntax-entry ?\: "." table)
    (modify-syntax-entry ?\| "." table)
    (modify-syntax-entry ?\* "." table)
    (modify-syntax-entry ?\+ "." table)
    (modify-syntax-entry ?\- "." table)
    (modify-syntax-entry ?\> "." table)
    (modify-syntax-entry ?\? "." table)
    (modify-syntax-entry ?\^ "." table)
    (modify-syntax-entry ?\! "." table)
    (modify-syntax-entry ?\~ "." table)
    (modify-syntax-entry ?\. "." table)

    (modify-syntax-entry ?\\ "\\" table)

    (modify-syntax-entry ?\'  "\"" table)

    (modify-syntax-entry ?\_  "w" table)

    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    table)
  "Syntax table for `antlr3-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defun antlr3-indent-line ()
  "Indent current line for `antlr3-mode'."
  (interactive)
  (let (indent-col)
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (progn
            (backward-up-list 1)
            (if (looking-at "{")
                (progn
                  (back-to-indentation)
                  (setq indent-col (+ (current-column) antlr3-indent-offset)))
              (setq indent-col (current-column))))
        (error
         (if (looking-at "^\\([a-z]\\|\\s-*//\\)")
             (setq indent-col 0)
           (setq indent-col antlr3-indent-offset)))))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "}") (>= indent-col antlr3-indent-offset))
        (setq indent-col (- indent-col antlr3-indent-offset))))
    (indent-line-to indent-col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imenu support

(defun antlr3-mode-imenu-create-index-function ()
  "Create imenu index."
  (let ((case-fold-search nil)
        (final-alist '())
        (lexer-alist '())
        (parser-alist '()))
    (goto-char (point-min))
    (while (re-search-forward "^\\([A-Z][a-zA-Z0-9_]*\\)\\s-*:" nil t)
      (let ((name (match-string 1))
            (pos (match-beginning 0)))
        (push (cons name pos) lexer-alist)))
    (goto-char (point-min))
    (while (re-search-forward "^\\([a-z][a-zA-Z0-9_]*\\)\\s-*:" nil t)
      (let ((name (match-string 1))
            (pos (match-beginning 0)))
        (push (cons name pos) parser-alist)))
    (when parser-alist
      (push (cons "Parser rules" (antlr3-mode-sort-alist-by-car-string parser-alist)) final-alist))
    (when lexer-alist
      (push (cons "Lexer rules" (antlr3-mode-sort-alist-by-car-string lexer-alist)) final-alist))
    final-alist))

(defun antlr3-mode-sort-alist-by-car-string (alist)
  (sort alist '(lambda (x y) (string< (car x) (car y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font-lock

; (concat "\\<" (regexp-opt '("grammar" "options" "tokens" "fragment") t) "\\>")

(defvar antlr3-mode-font-lock-keywords
  '(
    ("\\<\\(fragment\\|grammar\\|\\(?:optio\\|toke\\)ns\\)\\>"
     0 font-lock-keyword-face)
    ("^[a-zA-Z0-9_]+"
     0 font-lock-function-name-face)
    ("^@[a-zA-Z0-9_:]+"
     0 font-lock-keyword-face)
    ("\\<[A-Z][a-zA-Z0-9_]*\\>"
     0 font-lock-variable-name-face)
    ("\\<\\([a-z][a-zA-Z0-9_]*\\)="
     1 font-lock-constant-face)
    ("$[a-z][a-zA-Z0-9_]*"
     0 font-lock-constant-face)
   )
  "Keyword highlighting specification for `antlr3-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar antlr3-mode-map nil "`antlr3-mode' keymap.")
(if (not antlr3-mode-map)
    (let ((map (make-keymap)))
      (setq antlr3-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode startup

(defun antlr3-mode ()
  "antlr3-mode is a major mode for editing ANTLR 3 files.\n\n
\\{antlr3-mode-map}"
  (interactive)

  (kill-all-local-variables)
  (setq major-mode 'antlr3-mode)
  (setq mode-name "antlr3")

  (use-local-map antlr3-mode-map)

  (set-syntax-table antlr3-mode-syntax-table)

  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "/\\*+[ \t]*\\|//[ \t]*")

  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  (make-local-variable 'antlr3-indent-offset)
  (set (make-local-variable 'indent-line-function) 'antlr3-indent-line)

  (setq imenu-create-index-function #'antlr3-mode-imenu-create-index-function)

  (set (make-local-variable 'font-lock-defaults) '(antlr3-mode-font-lock-keywords))
  (turn-on-font-lock)

  (run-hooks 'antlr3-mode-hook))

(provide 'antlr3-mode)

;;; antlr3-mode.el ends here.
