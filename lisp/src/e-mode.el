;;; e-mode.el --- Major mode for editing Specman 'e' files

;; Copyright (C) 2007  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 01 Jan 2007
;; Version: 1.0
;; Keywords: programming
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;  e-mode is a major mode for editing code written in the Specman 'e' language
;;
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;; (autoload 'e-mode "e-mode" "Specman 'e' code editing mode" t)
;; (add-to-list 'auto-mode-alist '("\\.e$" . e-mode))

;;; Code:

(defconst e-mode-version "1.0"
  "Version of this Specman 'e' mode.")

(defconst e-mode-running-xemacs
  (string-match "XEmacs" emacs-version)
  "Are we running XEmacs?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

;;;###autoload
(defgroup e-mode nil
  "Specman 'e' mode."
  :group 'languages)

;;;###autoload
(defcustom e-mode-basic-offset 4
  "*Indentation of statements with respect to containing block."
  :group 'e-mode
  :type 'integer)

;;;###autoload
(defcustom e-mode-try-to-guess-indent t
  "*Try to guess what to set `e-mode-basic-offset' to when opening a file."
  :group 'e-mode
  :type 'boolean)

;;;###autoload
(defcustom e-mode-continued-line-offset 4
  "*Extra indentation of continued statements."
  :group 'e-mode
  :type 'integer)

;;;###autoload
(defcustom e-mode-constraint-line-offset 0
  "*Extra indentation of constraints."
  :group 'e-mode
  :type 'integer)

;;;###autoload
(defcustom e-mode-line-up-bracket t
  "*Non-nil means indent items in brackets relative to the '['.
Otherwise indent them as usual."
  :group 'e-mode
  :type 'boolean)

;;;###autoload
(defcustom e-mode-line-up-paren t
  "*Non-nil means indent items in parentheses relative to the '('.
Otherwise indent them as usual."
  :group 'e-mode
  :type 'boolean)

;;;###autoload
(defcustom e-mode-curly-opener-is-electric t
  "*Non-nil means reindent the current line after entering '{'."
  :group 'e-mode
  :type 'boolean)

;;;###autoload
(defcustom e-mode-curly-closer-is-electric t
  "*Non-nil means reindent the current line after entering '}'."
  :group 'e-mode
  :type 'boolean)

;;;###autoload
(defface e-mode-preprocessor-face
  '((t (:foreground "yellow4")))
  "Face for preprocessor directives."
  :group 'e-mode)

;;;###autoload
(defcustom e-mode-imenu-flatten nil
  "*Non-nil means flatten the heirarchical imenu output."
  :group 'e-mode
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions

(defun e-mode-in-external-comment-p ()
  "Return true if point is in an external comment"
  (save-excursion
    (or (not (re-search-backward "^\\(<'\\|'>\\)" nil t))
        (looking-at "'>"))))

(defun e-mode-in-line-comment-p ()
  "Return true if point is in a // or -- comment"
  (save-excursion
    (re-search-backward "\\(--\\|//\\)" (point-at-bol) t)))

(defun e-mode-in-string-p ()
  "Return true if point is in a string"
  (if e-mode-running-xemacs
      (save-excursion
        (condition-case nil
            (let ((pos (point)))
              (backward-up-list)
              (nth 3 (parse-partial-sexp (point) pos)))
          (error nil)))
    (nth 3 (syntax-ppss))))

(defsubst e-mode-re-search-backward (REGEXP BOUND NOERROR)
  "Like re-search-backward, but skips over matches in comments or strings."
  (when (catch 'done
          (while (re-search-backward REGEXP BOUND NOERROR)
            (when (equal 'in-code (e-mode-skip-backward-comment-or-string BOUND NOERROR))
              (throw 'done t))))
    (point)))

(defun e-mode-skip-backward-comment-or-string (BOUND NOERROR)
  "If in a string or comment, move to the beginning of it.
Returns 'in-code if point is not in a string or comment"
  (save-match-data
    (if (e-mode-in-external-comment-p)
        (re-search-backward "^'>" BOUND NOERROR)
      (cond
       ((e-mode-in-string-p)
        (search-backward "\"" BOUND NOERROR))
       ((e-mode-in-line-comment-p)
        (let (pos)
          (save-excursion
            (beginning-of-line)
            (re-search-forward "\\(--\\|//\\)")
            (backward-char 2)
            (setq pos (point)))
          (if (>= pos (or BOUND 0))
              (goto-char pos)
            (if (not NOERROR)
                (error "Search failed")
              (unless (equal NOERROR t)
                (goto-char BOUND))
              nil))))
       (t
        'in-code)))))

(defsubst e-mode-re-search-forward (REGEXP BOUND NOERROR)
  "Like re-search-forward, but skips over matches in comments or strings."
  (when (catch 'done
          (while (re-search-forward REGEXP BOUND NOERROR)
            (when (equal 'in-code (e-mode-skip-forward-comment-or-string BOUND NOERROR))
              (throw 'done t))))
    (point)))

(defun e-mode-skip-forward-comment-or-string (BOUND NOERROR)
  "If in a string or comment, move to the end of it.
Returns 'in-code if point is not in a string or comment"
  (save-match-data
    (if (e-mode-in-external-comment-p)
        (re-search-forward "^<'" BOUND NOERROR)
      (cond
       ((e-mode-in-string-p)
        (search-forward "\"" BOUND NOERROR))
       ((e-mode-in-line-comment-p)
        (forward-line 1))
       (t
        'in-code)))))

(defun e-mode-beginning-of-code ()
  "Move backward to beginning of code block."
  (re-search-backward "^<'" nil 'move))

(defun e-mode-beginning-of-defun (&optional arg)
  "Go to beginning of method."
  (interactive)
  (let ((pos (point))
        (done nil))
    (condition-case nil
        (while (and (not done) (not (bobp)))
          (backward-up-list)
          (when (= (char-after) ?\{)
            (when (looking-back "is\\(\\s-+\\(also\\|first\\|only\\|empty\\|undefined\\)\\)?[ \t\n]*")
              (goto-char (match-beginning 0))
              (beginning-of-line)
              (setq done t))))
      (goto-char pos)
      (error "Not inside a method"))))

(defun e-mode-end-of-defun (&optional arg)
  "Go to end of method."
  (interactive)
  (let ((pos (point)))
    (e-mode-beginning-of-defun)
    (if (e-mode-re-search-forward "\\(is\\|also\\|first\\|only\\)[ \t\n]*{" nil t)
        (progn
          (backward-char)
          (forward-sexp)
          (end-of-line))
      (goto-char pos)
      (error "Couldn't find end of method"))))

(defun e-mode-narrow-to-scope ()
  "Narrow buffer to enclosing scope."
  (interactive)
  (let (start)
    (save-excursion
      (condition-case nil
          (while (and (not (backward-up-list))
                      (not (= (char-after) ?\{))
                      (not (bobp))))
        (error "Not inside any scope"))
      (setq start (point-at-bol))
      (forward-sexp)
      (forward-line 1)
      (narrow-to-region start (point)))))

(defun e-mode-guess-indent ()
  "Try to guess what to set `e-mode-basic-offset' to."
  (let ((offset 0))
    (save-excursion
      (goto-char (point-min))
      (while (and (equal offset 0)
                  (not (eobp)))
        (when (e-mode-re-search-forward "{\\s-*$" nil 'go)
          (skip-syntax-forward " >" nil)
          (setq offset (- (point) (point-at-bol)))))
      (when (> offset 0)
        (setq e-mode-basic-offset offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font-lock

(defvar e-mode-keywords
  (concat "\\_<\\("
          (regexp-opt
           '(
             "all_values" "and" "as_a" "assert" "assume" "async" "attribute" "before" "bind" "bits" "break"
             "call_port" "case" "check" "compute" "consume" "cover" "cross" "cycle" "def_err" "default" "define"
             "detach" "do" "dut_error" "dut_errorf" "else" "emit" "event" "event_port" "eventually" "exec" "expect"
             "extend" "fail" "file" "for" "force" "gen" "gen_before_subtypes" "get_config" "get_enclosing_unit"
             "if" "ignore" "import" "in" "in_sequence" "in_unit" "inout" "is" "keep" "keeping" "like" "line"
             "list" "matching" "me" "message" "messagef" "nand" "new" "nor" "not" "nxor" "of" "or" "out" "package"
             "print" "private" "protected" "range" "ranges" "reference" "release" "repeat" "reset_gen_before_subtypes"
             "return" "routine" "select" "sequence" "set_config" "set_config_max" "soft" "start" "step" "struct"
             "sync" "that" "then" "to" "transition" "try_enclosing_unit" "type" "unit" "until" "using" "var" "wait"
             "when" "while" "with" "within"
             "on" "each" "from" "continue" "item" "method_type" "const"
             ))
          "\\)\\_>"))

(defvar e-mode-builtin-types
  (concat "\\_<\\("
          (regexp-opt
           '(
             "bit" "bits" "bool" "byte" "bytes" "file" "int" "nibble" "string" "uint" "time"
             "simple_port" "buffer_port" "method_port" "event_port"
             "it" "index" "result"
             ))
          "\\)\\_>"))

(defvar e-mode-const
  (concat "\\_<\\("
          (regexp-opt
           '(
             "TRUE" "FALSE" "UNDEF" "NULL"
             "empty" "undefined" "others" "edges" "max"
              "fs" "ps" "ns" "us" "ms" "sec" "min" "hr"
             ))
          "\\)\\_>"))

(defvar e-mode-builtin-methods
  (concat "\\_<\\("
          (regexp-opt
           '(
             "abs" "add" "add0" "agent" "agent_code" "all" "all_indices" "and_all" "append" "appendf" "apply"
             "as_a" "average" "bin" "bind" "bitwise_and" "bitwise_nand" "bitwise_nor" "bitwise_or" "bitwise_xnor"
             "bitwise_xor" "check" "clear" "copy" "count" "crc_32" "crc_32_flip" "crc_8" "dec" "deep_compare"
             "deep_compare_physical" "deep_copy" "delete" "div_round_up" "do_bind" "do_pack" "do_print"
             "do_unpack" "e_path" "even" "exists" "extract" "fast_delete" "field" "finalize" "first"
             "first_index" "full_hdl_path" "get_all_units" "get_enclosing_unit" "get_indices" "get_parent_unit"
             "get_ports" "get_ports_recursively" "get_unit" "has" "hdl_path" "hex" "ilog10" "ilog2" "init"
             "insert" "ipow" "is_a_permutation" "is_all_iterations" "is_empty" "isqrt" "key" "key_exists"
             "key_index" "last" "last_index" "max" "max_index" "max_value" "min" "min_index" "min_value" "odd"
             "or_all" "out" "outf" "pack" "pack_options" "pop" "pop0" "post_generate" "pre_generate" "print_line"
             "product" "push" "push0" "quit" "quote" "rerun" "resize" "reverse" "run" "set_unit" "size" "sort"
             "sort_by_field" "source_location" "source_method" "split" "str_chop" "str_empty" "str_exactly"
             "str_expand_dots" "str_insensitive" "str_join" "str_len" "str_lower" "str_match" "str_pad"
             "str_replace" "str_split" "str_split_all" "str_sub" "str_upper" "sum" "swap" "to_string" "top"
             "top0" "try_enclosing_unit" "unique" "unpack" "visualize" "writef"
             "read_only" "value" "reset_soft"
             "connect_pointers" "connect_ports" "check_generation" "do_bind_unit" "remap_hdl_path" "disconnect"
             "disconnect_bound_set" "get_bound_set" "is_connected" "delay"
             ))
          "\\)[ \t]*("))

(defvar e-mode-font-lock-keywords
  (list
   ;; Line comments
   (cons "\\(\\(//\\|--\\).*$\\)" '(0 font-lock-comment-face))
   ;; Builtin types
   (cons e-mode-builtin-types '(0 font-lock-type-face))
   ;; Constants
   (cons e-mode-const '(0 font-lock-constant-face))
   ;; Verilog defines
   (cons "\\_<`[a-zA-Z0-9_]+\\_>" '(0 font-lock-constant-face))
   ;; Sub-types
   (cons "\\_<\\([a-zA-Z][a-zA-Z0-9_]*'[a-zA-Z0-9_]+\\)\\_>" '(0 font-lock-type-face keep))
   ;; Preprocessor directives
   (cons "^\\s-*\\(#\\(ifn?def\\|define\\|undef\\|else\\)\\)" '(1 'e-mode-preprocessor-face))
   ;; Keywords
   (cons e-mode-keywords '(0 font-lock-keyword-face keep))
   ;; Special symbols
   (cons "[@$]" '(0 font-lock-keyword-face))
   ;; Keywords that include spaces
   (cons
    (concat
     "\\_<"
     "\\(down[ \t]+to\\)\\|"
     "\\(all[ \t]+of\\)\\|"
     "\\(first[ \t]+of\\)\\|"
     (concat "\\(is[ \t]+\\("
             (regexp-opt '("a" "also" "first" "inline" "instance" "only"))
             "\\)\\)\\|")
     "\\(is[ \t]+c[ \t]+routine\\)\\|"
     "\\(is[ \t]+not[ \t]+a\\)\\|"
     "\\(state[ \t]+machine\\)\\|"
     "\\(using[ \t]+\\(also\\|index\\)\\)\\|"
     "\\(verilog[ \t]+\\(code\\|function\\|import\\|simulator\\)\\)\\|"
     "\\(vhdl[ \t]+simulator\\)"
     "\\_>"
     )
    '(0 font-lock-keyword-face keep)))
  "Default highlighting for Specman 'e' mode.")

(defvar e-mode-font-lock-keywords-1
  (append e-mode-font-lock-keywords
          (list
           ;; Builtin methods
           (cons e-mode-builtin-methods '(1 font-lock-builtin-face))))
  "Subdued level highlighting for Specman 'e' mode.")

(defvar e-mode-font-lock-keywords-2
  (append e-mode-font-lock-keywords-1
          (list
           ;; Methods
           (cons
            "^\\s-*\\(\\(package\\|protected\\|private\\)\\s-+\\)?\\([a-zA-Z0-9_]+\\)\\s-*(\\s-*\\([a-zA-Z0-9_]+\\s-*:\\|)\\s-*\\(is\\>\\|[@:]\\)\\)"
            '(3 font-lock-function-name-face))
           (cons
            "^\\s-*method_type\\s-+\\([a-zA-Z0-9_]+\\)"
            '(1 font-lock-function-name-face))
           ;; Fields
           (cons
            "^\\s-*\\(\\(var\\|package\\|protected\\|private\\|const\\)\\s-+\\)?[!%]*\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\(\\s-*\\[[^]]+\\]\\)?\\s-*:[^:]"
            '(3 font-lock-variable-name-face))))
  "Medium level highlighting for Specman 'e' mode.")

(defvar e-mode-font-lock-keywords-3
  (append e-mode-font-lock-keywords-2
          (list
           ;; External comments
           '(e-mode-fontify-external-comment (0 'font-lock-comment-face t))))
  "Gaudy level highlighting for Specman 'e' mode.")

(defun e-mode-fontify-external-comment (limit)
  "Fontify external comments."
  (if (e-mode-in-external-comment-p)
      (let ((start (point)))
        (re-search-forward "^<'" limit 'move)
        (set-match-data (list start (point)))
        t)
    (when (re-search-forward "^'>" limit 'move)
      (save-excursion
        (beginning-of-line)
        (looking-at "'>")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defun e-mode-indent-line ()
  "Indent the current line."
  (interactive)
  (let ((pos (- (point-max) (point)))
        (indent (e-mode-get-indent)))
    (unless (= indent (current-indentation))
      (back-to-indentation)
      (delete-region (point-at-bol) (point))
      (indent-to indent))
    (if (< (- (point) (point-at-bol)) indent)
        (back-to-indentation)
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos))))))

(defun e-mode-get-indent ()
  "Return how much the current line should be indented."
  (save-excursion
    (beginning-of-line)
    (cond
     ((or (e-mode-in-string-p)
          (e-mode-in-external-comment-p)
          (looking-at "\\(<'\\|'>\\)"))
      0)
     ((e-mode-get-indent-if-in-paren))
     ((e-mode-get-indent-if-opener))
     ((e-mode-get-indent-if-closer))
     ((e-mode-get-normal-indent)))))

(defun e-mode-get-indent-if-in-paren ()
  "Get amount to indent if in parentheses or brackets."
  (save-excursion
      (condition-case nil
          (let ((at-closer (looking-at "[ \t]*[])]"))
                offset)
            (backward-up-list)
            (unless (or (= (char-after) ?{)
                        (and (= (char-after) ?\[) (not e-mode-line-up-bracket))
                        (and (= (char-after) ?\() (not e-mode-line-up-paren)))
              (setq offset (current-column))
              (unless at-closer
                (forward-char)
                (if (and (> (skip-syntax-forward " >" (point-at-eol)) 0)
                         (not (looking-at "\\(--\\|//\\)")))
                    (setq offset (current-column))
                  (setq offset (1+ offset))))
              offset))
        (error nil))))

(defun e-mode-get-indent-if-opener ()
  "Get amount to indent if looking at a opening brace."
  (when (looking-at "^[ \t]*{")
    (e-mode-beginning-of-statement)
    (if (looking-at "{")
        (e-mode-get-normal-indent)
      (current-column))))

(defun e-mode-get-indent-if-closer ()
  "Get amount to indent if looking at a closing brace."
  (when (looking-at "^[ \t]*}")
    (backward-up-list)
    (e-mode-beginning-of-statement)
    (current-column)))

(defun e-mode-beginning-of-statement ()
  "Move to beginning of statement."
  (let ((limit (point)))
    (e-mode-re-search-backward "\\(;\\|{\\|<'\\)" nil 'move)
     (if (and (looking-at ";")
              (re-search-backward "\\(for\\|in\\)\\s-*{" (point-at-bol) t))
         (back-to-indentation)
      (when (looking-at "[;{]")
        (forward-char)
        (skip-syntax-forward " >" limit)
        (while (cond
                ((looking-at "\\(--\\|//\\)")
                 (forward-line)
                 (skip-syntax-forward " >" limit))
                (t nil)))))))

(defun e-mode-get-normal-indent ()
  "Normal indent for a line."
  (save-excursion
    (condition-case nil
        (let ((bol (point))
              (offset 0))
          (e-mode-beginning-of-statement)
          (when (looking-at "\\s-*keep\\s-")
            (setq offset e-mode-constraint-line-offset))
          (if (= (char-after) ?})
              (progn
                (backward-up-list)
                (e-mode-beginning-of-statement)
                (setq offset (current-column)))
            (when (> bol (point-at-bol))
              (setq offset e-mode-continued-line-offset))
            (backward-up-list)
            (e-mode-beginning-of-statement)
            (+ (current-column) e-mode-basic-offset offset)))
      (error 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imenu support

;; TODO
;; ports
;; cover
;; method_type
;; Nest subtypes?

(if e-mode-running-xemacs
    (fset 'e-mode-match-string 'match-string)
  (fset 'e-mode-match-string 'match-string-no-properties))

(defvar e-mode-imenu-type-alist '()
  "Variable to keep imenu type alist")

(defvar e-mode-imenu-struct-alist '()
  "Variable to keep imenu struct/unit alist")

(defun e-mode-sort-alist-by-car-string (alist)
  (sort alist '(lambda (x y) (string< (car x) (car y)))))

(defun e-mode-trim-trailing-whitespace (string)
  (if e-mode-running-xemacs
      (replace-in-string string "[ \t\n]+$" "")
    (replace-regexp-in-string "[ \t\n]+$" "" string)))

(defun e-mode-imenu-create-parse-types (end)
  (while (re-search-forward "^\\s-*\\(package\\s-+\\)?\\(type\\|extend\\)[ \t\n]+\\([a-zA-Z0-9_:]+\\)[ \t\n]*:" end t)
    (let ((type-name (e-mode-match-string 3)) (type-def (e-mode-match-string 2))
          (type-access (e-mode-match-string 1)) (type-pos (match-beginning 0)))
      (if (and (string-match "extend" type-def) (not e-mode-imenu-flatten))
          (setq type-def " <extend>")
        (setq type-def ""))
      (if (and type-access (not e-mode-imenu-flatten))
          (setq type-access (concat " [" (e-mode-trim-trailing-whitespace type-access) "]"))
        (setq type-access ""))
      (push (cons (concat type-name type-access type-def) type-pos) e-mode-imenu-type-alist))))

(defun e-mode-imenu-create-parse-fields (end)
  (let ((field-alist '()))
    (save-excursion
      (while (re-search-forward "^\\s-*\\(\\(package\\|protected\\|private\\|const\\)\\s-+\\)?\\([!%]*\\)\\([a-zA-Z_][a-zA-Z0-9_]*\\(\\s-*\\[[^]]+\\]\\)?\\)[ \t\n]*:[ \t\n]*\\([^{\n]*\\);" end t)
        (let ((field-access (e-mode-match-string 2))
              (field-mods (e-mode-match-string 3))
              (field-name (e-mode-match-string 4))
              (field-type (e-mode-match-string 6))
              (field-pos (match-beginning 0)))
          (unless (e-mode-in-bracket-p)
            (when (and field-mods (not e-mode-imenu-flatten))
              (setq field-name (concat field-mods field-name)))
            (if (and field-access (not e-mode-imenu-flatten))
                (setq field-access (concat " [" (e-mode-trim-trailing-whitespace field-access) "]"))
              (setq field-access ""))
            (if (and (string-match "\\_<is[ \t]+instance\\_>" field-type) (not e-mode-imenu-flatten))
                (setq field-type " <instance>")
              (setq field-type ""))
            (push (cons (concat field-name field-type field-access) field-pos) field-alist)))))
    field-alist))

(defun e-mode-in-bracket-p ()
  "Return true if point is in brackets."
  (save-excursion
    (condition-case nil
        (progn (backward-up-list)
               (= (char-after) ?\[))
      (error nil))))

(defun e-mode-imenu-create-parse-methods (end)
  (let ((method-alist '()))
    (save-excursion
      (while (re-search-forward "^\\s-*\\(\\(package\\|protected\\|private\\)\\s-+\\)?\\([a-zA-Z0-9_]+\\)\\s-*([^)]*?)\\s-*\\(.*?[ \t\n]+\\)?is\\(\\s-+\\(also\\|first\\|only\\|empty\\|undefined\\)\\)?[ \t\n]*[{;]" end t)
        (let ((method-name (e-mode-match-string 3)) (method-pos (match-beginning 0))
              (method-access (e-mode-match-string 2)) (method-ret-and-tcm (e-mode-match-string 4)))
          (if (and method-access (not e-mode-imenu-flatten))
              (setq method-access (concat " [" (e-mode-trim-trailing-whitespace method-access) "]"))
            (setq method-access ""))
          (if (and method-ret-and-tcm
                   (string-match "@[a-zA-Z0-9_.]+" method-ret-and-tcm)
                   (not e-mode-imenu-flatten))
              (setq method-ret-and-tcm (concat " <" (match-string 0 method-ret-and-tcm) ">"))
            (setq method-ret-and-tcm ""))
          (push (cons (concat method-name method-ret-and-tcm method-access) method-pos) method-alist))))
    method-alist))

(defun e-mode-imenu-create-parse-ons (end)
  (let ((on-alist '()))
    (save-excursion
      (while (re-search-forward "^\\s-*on\\s-+\\([a-zA-Z0-9_]+\\)[ \t\n]+{" end t)
        (push (cons (e-mode-match-string 1) (match-beginning 0)) on-alist)))
    on-alist))

(defun e-mode-imenu-create-parse-events (end)
  (let ((event-alist '()))
    (save-excursion
      (while (re-search-forward "^\\s-*event\\s-+\\([a-zA-Z0-9_]+\\)" end t)
        (push (cons (e-mode-match-string 1) (match-beginning 0)) event-alist)))
    event-alist))

(defun e-mode-imenu-create-parse-outer-base-structs (end-outer-defn)
  (while (re-search-forward "^\\s-*\\(package\\)?\\(struct\\|unit\\)[ \t\n]+\\([a-zA-Z0-9_:]+\\)\\([ \t\n]+like[ \t\n]+[a-zA-Z0-9_:]+\\)?[ \t\n]*{" end-outer-defn t)
    (let ((name (e-mode-match-string 3)) (kind (e-mode-match-string 1)) (pos (match-beginning 0)))
      (e-mode-imenu-create-parse-struct name kind pos))))

(defun e-mode-imenu-create-parse-outer-extend-structs (end-outer-defn)
  (while (re-search-forward "^\\s-*extend[ \t\n]+\\(\\([a-zA-Z0-9_']+[ \t\n]+\\)*\\)\\([a-zA-Z0-9_:]+\\)[ \t\n]*{" end-outer-defn t)
    (let ((name (e-mode-match-string 3)) (kind (e-mode-match-string 1)) (pos (match-beginning 0)))
      (e-mode-imenu-create-parse-struct name kind pos))))

(defun e-mode-imenu-create-parse-inner-structs (end-outer-defn)
  (while (re-search-forward "^\\s-*when[ \t\n]+\\(\\([a-zA-Z0-9_']+[ \t\n]+\\)+\\)\\([a-zA-Z0-9_:]+\\)[ \t\n]*{" end-outer-defn t)
    (let ((name (e-mode-match-string 3)) (kind (e-mode-match-string 1)) (pos (match-beginning 0)))
      (e-mode-imenu-create-parse-struct name kind pos))))

(defun e-mode-imenu-create-parse-struct (name kind pos)
  (let ((end-defn) (end-current) (start-next)
        (struct-alist '()) (field-alist '()) (method-alist '()) (on-alist '()) (event-alist '()))
    (save-excursion
      (backward-char)
      (forward-sexp)
      (setq end-defn (point)))
    (while (not (= end-defn (point)))
      (save-excursion
        (if (re-search-forward "^\\s-*when[ \t\n]+\\([a-zA-Z0-9_']+[ \t\n]+\\)*[a-zA-Z0-9_:]+[ \t\n]*{" end-defn t)
            (progn (setq end-current (match-beginning 0))
                   (backward-char)
                   (forward-sexp)
                   (setq start-next (point))
                   (goto-char end-current)
                   (e-mode-imenu-create-parse-inner-structs start-next))
          (setq end-current end-defn)
          (setq start-next end-defn)))
      (setq field-alist (append field-alist (e-mode-imenu-create-parse-fields end-current)))
      (setq event-alist (append event-alist (e-mode-imenu-create-parse-events end-current)))
      (setq method-alist (append method-alist (e-mode-imenu-create-parse-methods end-current)))
      (setq on-alist (append on-alist (e-mode-imenu-create-parse-ons end-current)))
      (goto-char start-next))
    (if e-mode-imenu-flatten
        (progn
          (let ((num-dups (e-mode-imenu-count-dups name)))
            (when (> num-dups 0)
              (setq name (concat name "<" (number-to-string num-dups) ">"))))
          (push (cons name pos) e-mode-imenu-struct-alist)
          (e-mode-imenu-add-flattened-struct name field-alist)
          (e-mode-imenu-add-flattened-struct name event-alist)
          (e-mode-imenu-add-flattened-struct name method-alist)
          (e-mode-imenu-add-flattened-struct name on-alist))
      (push (cons "*Definition*" pos) struct-alist)
      (when on-alist
        (push (cons "Ons" (e-mode-sort-alist-by-car-string on-alist)) struct-alist))
      (when method-alist
        (push (cons "Methods" (e-mode-sort-alist-by-car-string method-alist)) struct-alist))
      (when event-alist
        (push (cons "Events" (e-mode-sort-alist-by-car-string event-alist)) struct-alist))
      (when field-alist
        (push (cons "Fields" (e-mode-sort-alist-by-car-string field-alist)) struct-alist))
      (when (and kind (not (string= kind "")))
        (setq kind (e-mode-trim-trailing-whitespace kind))
        (if (string= kind "package")
            (setq name (concat name " [package]"))
          (setq name (concat name " <" kind ">"))))
      (push (cons name struct-alist) e-mode-imenu-struct-alist))))

(defun e-mode-imenu-count-dups (name)
  (let ((num 0))
    (mapc (lambda (item)
            (if (string= name (car item))
                (setq num (1+ num))
              (when (and (string-match "^\\(.+\\)<[0-9]+>$" (car item))
                         (string= name (e-mode-match-string 1 (car item))))
                (setq num (1+ num)))))
          e-mode-imenu-struct-alist)
    num))

(defun e-mode-imenu-add-flattened-struct (name alist)
  (let (qualified-name num-dups)
    (mapc (lambda (item)
            (setq qualified-name (concat name "::" (car item)))
            (setq num-dups (e-mode-imenu-count-dups qualified-name))
            (when (> num-dups 0)
              (setq qualified-name (concat qualified-name "<" (number-to-string num-dups) ">")))
            (push (cons qualified-name (cdr item)) e-mode-imenu-struct-alist))
          alist)))

(defun e-mode-imenu-create-index-function ()
  "Create E imenu index."
  (let ((final-alist '()) start end)
    (setq e-mode-imenu-type-alist '())
    (setq e-mode-imenu-struct-alist '())
    (goto-char (point-min))
    (while (re-search-forward "^<'" nil t)
      (setq start (point))
      (save-excursion
        (if (re-search-forward "^'>" nil t)
            (setq end (point))
          (setq end (point-max))))
      (e-mode-imenu-create-parse-types end)
      (goto-char start)
      (save-excursion (e-mode-imenu-create-parse-outer-base-structs end))
      (e-mode-imenu-create-parse-outer-extend-structs end))
    (if e-mode-imenu-flatten
        (progn
          (mapc (lambda (x) (push x final-alist)) e-mode-imenu-type-alist)
          (mapc (lambda (x) (push x final-alist)) e-mode-imenu-struct-alist)
          (setq final-alist (e-mode-sort-alist-by-car-string final-alist)))
      (when e-mode-imenu-struct-alist
        (push (cons "Structs/Units" (e-mode-sort-alist-by-car-string e-mode-imenu-struct-alist)) final-alist))
      (when e-mode-imenu-type-alist
        (push (cons "Types" (e-mode-sort-alist-by-car-string e-mode-imenu-type-alist)) final-alist)))
    final-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation support

(require 'compile)
(setq compilation-error-regexp-alist
      (add-to-list 'compilation-error-regexp-alist
                   '("^[ \t]+\\*\\*\\* Error[^\t]+\\s-+at line \\([0-9]+\\) in[ \t\n]+\\(.*\\)\\s-*$" 2 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find-file-at-point support

(require 'ffap)

(defun ffap-e-mode (name)
  (let ((ffap-e-mode-path
         (when (getenv "SPECMAN_PATH")
           (split-string (getenv "SPECMAN_PATH") ":"))))
    (push "." ffap-e-mode-path)
    (ffap-locate-file name (list ".e") ffap-e-mode-path)))

(setq ffap-alist (append (list '(e-mode . ffap-e-mode)) ffap-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Align support

(require 'align)

(defvar align-e-mode-rules-list
  `(
    (e-mode-define
     (regexp . "\\(define\\|event\\)\\(\\s-+\\)\\S-+\\(\\s-+\\)\\S-")
     (group . (2 3)))

    (e-mode-comparison
     (regexp . "[^ \t\n!=~><]\\(\\s-*\\)[!=~><]+\\(\\s-*\\)\\S-")
     (group . (1 2)))

    (e-mode-verilog
     (regexp . "\\(verilog\\).*'.*'\\(\\s-+\\)\\S-")
     (group . (2)))

    (e-mode-declaration
     (regexp . "[^ \t\n:]\\(\\s-*\\):\\(\\s-*\\)\\S-")
     (group . (1 2)))
    )
  "Specman 'e' alignment rules.")

(defvar align-exclude-e-mode-rules-list
  `(
    (exc-dq-string
     (regexp . "\"\\([^\"\n]+\\)\"")
     (repeat . t)
     (modes . align-dq-string-modes))

    (exc-open-comment
     (regexp . ,(function (lambda (end reverse)
                            (funcall (if reverse 're-search-backward 're-search-forward)
                                     (concat "[^ \t\n\\\\]" (regexp-quote comment-start)
                                             "\\(.+\\)$") end t))))
     (modes . align-open-comment-modes))

    (e-mode-slice
     (regexp . "\\(\\[[0-9]+:[0-9]+\\]\\)")
     (group . 1))
    )
  "Specman 'e' alignment exclusion rules.")

(put 'align-e-mode-rules-list 'risky-local-variable t)
(put 'align-exclude-e-mode-rules-list 'risky-local-variable t)

(add-to-list 'align-dq-string-modes 'e-mode)
(add-to-list 'align-open-comment-modes 'e-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev table

(defvar e-mode-abbrev-table nil
  "*Abbrev table in use in e-mode buffers.")

(define-abbrev-table 'e-mode-abbrev-table ())

(defun e-mode-abbrev-get-prefix ()
  (save-match-data
    (if (string-match ".*/\\(.*?\\)/e/" buffer-file-name)
        (match-string 1 buffer-file-name)
      "no_prefix")))

(defun e-mode-abbrev-find-previous-field ()
  (save-excursion
    (beginning-of-line)
    (catch 'found
      (while (not (bobp))
        (forward-line -1)
        (when (looking-at "^\\s-*keep\\s-+\\(soft\\s-+\\)?\\([a-zA-Z0-9_]+\\)")
          (throw 'found (e-mode-match-string 2)))
        (when (looking-at "^\\s-*\\(\\(package\\|protected\\|private\\)\\s-+\\)?[!%]*\\s-*\\([a-zA-Z][a-zA-Z0-9_]*\\)\\s-*\\(\\[[0-9]*\\]\\)?\\s-*:")
          (throw 'found (e-mode-match-string 3)))))))

;; Abbrev expand { into {\n<indent>\n};

(defun e-mode-abbrev-hook ()
  (when (and (string= major-mode "e-mode")
             (looking-back "{"))
    (insert "\n\n};")
    (e-mode-indent-line)
    (forward-line -1)
    (e-mode-indent-line)))

(add-hook 'pre-abbrev-expand-hook 'e-mode-abbrev-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar e-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    (modify-syntax-entry ?\! "." table)
    (modify-syntax-entry ?\# "." table)
    (modify-syntax-entry ?\$ "." table)
    (modify-syntax-entry ?\% "." table)
    (modify-syntax-entry ?\& "." table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\* "." table)
    (modify-syntax-entry ?\+ "." table)
    (modify-syntax-entry ?\, "." table)
    (modify-syntax-entry ?\. "." table)
    (modify-syntax-entry ?\: "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?\< "." table)
    (modify-syntax-entry ?\= "." table)
    (modify-syntax-entry ?\> "." table)
    (modify-syntax-entry ?\? "." table)
    (modify-syntax-entry ?\@ "." table)
    (modify-syntax-entry ?\` "." table)
    (modify-syntax-entry ?\| "." table)
    (modify-syntax-entry ?\~ "." table)
    (modify-syntax-entry ?^ "." table)

    (modify-syntax-entry ?\\ "\\" table)
    (if e-mode-running-xemacs
        (progn
          (modify-syntax-entry ?/  ". 12" table)
          (modify-syntax-entry ?-  ". 56" table)
          (modify-syntax-entry ?\n "> 37" table))
      (modify-syntax-entry ?/  ". 12" table)
      (modify-syntax-entry ?-  ". 12" table)
      (modify-syntax-entry ?\n "> 34" table))
    table)
  "Syntax table used in e-mode buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode map

(defun e-mode-electric-curly-opener()
  (interactive)
  (when e-mode-curly-opener-is-electric
    (e-mode-indent-line))
  (insert "{"))

(defun e-mode-electric-curly-closer()
  (interactive)
  (insert "}")
  (when e-mode-curly-closer-is-electric
    (e-mode-indent-line)))

(defvar e-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "{" 'e-mode-electric-curly-opener)
    (define-key map "}" 'e-mode-electric-curly-closer)
    (define-key map (kbd "C-x n s") 'e-mode-narrow-to-scope)
    map)
  "Keymap used in Specman 'e' mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; E mode

;;;###autoload
(defun e-mode ()
  "Major mode for editing Specman 'e' code.

Key Bindings:

\\{e-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'e-mode)
  (setq mode-name "e-mode")

  (use-local-map e-mode-map)

  (set-syntax-table e-mode-syntax-table)

  (setq local-abbrev-table e-mode-abbrev-table)

  (make-local-variable 'e-mode-basic-offset)
  (make-local-variable 'e-mode-continued-line-offset)
  (make-local-variable 'e-mode-constraint-line-offset)
  (make-local-variable 'e-mode-line-up-bracket)
  (make-local-variable 'e-mode-line-up-paren)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (setq comment-start "// "
        comment-end ""
        comment-multi-line t
        comment-start-skip "\\(//\\|--\\)[ \t]*"
        parse-sexp-ignore-comments t
        indent-line-function 'e-mode-indent-line)

  (set (make-local-variable 'beginning-of-defun-function) 'e-mode-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'e-mode-end-of-defun)

  ;; Indent

  (when e-mode-try-to-guess-indent
    (e-mode-guess-indent))

  ;; Imenu

  (setq imenu-create-index-function #'e-mode-imenu-create-index-function)
  (imenu-add-to-menubar "Specman Index")

  ;; Font-lock

  (setq font-lock-defaults
        '((e-mode-font-lock-keywords
           e-mode-font-lock-keywords-1
           e-mode-font-lock-keywords-2
           e-mode-font-lock-keywords-3)
          nil ;; nil means highlight strings & comments as well as keywords
          nil ;; nil means keywords must match case
          nil ;; use minimal syntax table for font lock
          e-mode-beginning-of-code ;; function to move to beginning of reasonable region to highlight
          ))
  (turn-on-font-lock)

  ;; Align rules

  (setq align-mode-rules-list align-e-mode-rules-list)
  (setq align-exclude-rules-list align-exclude-e-mode-rules-list)

  ;; Hooks

  (run-hooks 'e-mode-hook))

(provide 'e-mode)
;;; e-mode.el ends here
