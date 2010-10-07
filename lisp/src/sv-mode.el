;;; sv-mode.el --- Major mode for editing SystemVerilog files

;; Copyright (C) 2010  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 10 Aug 2010
;; Version: 1.2
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
;; sv-mode is a major mode for editing code written in the SystemVerilog
;; language.
;;
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;; (autoload 'sv-mode "sv-mode" "SystemVerilog code editing mode" t)
;; (add-to-list 'auto-mode-alist '("\\.sv$" . sv-mode))
;; (add-to-list 'auto-mode-alist '("\\.svh$" . sv-mode))
;;
;; Default keybindings:
;;
;; C-c C-j : Jump to matching begin/end/class/endclass/fork/join/etc.
;; C-c C-u : Move up out of the current scope
;; C-c C-d : Move down out of the current scope
;; C-c C-a : Move to beginning of current statement
;; C-c C-p : Move to beginning of previous block
;; C-c C-n : Move to end of next block
;; C-c C-o : Switch to "other" file, i.e. between .sv <-> .svh files
;; C-c C-s : Create skeleton task/function implementation in .sv file
;;           from prototype on current line in .svh file
;;
;; This mode supports the insertion of Doxygen comments if you use the
;; doxymacs package.

;;; Code:

(require 'custom)
(require 'find-file)

(defconst sv-mode-version "1.2"
  "Version of sv-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

;;;###autoload
(defgroup sv-mode nil
  "SystemVerilog mode."
  :group 'languages)

;;;###autoload
(defcustom sv-mode-basic-offset 3
  "*Indentation of statements with respect to containing block."
  :group 'sv-mode
  :type 'integer)

;;;###autoload
(defcustom sv-mode-line-up-bracket t
  "*Non-nil means indent items in brackets relative to the '['.
Otherwise indent them as usual."
  :group 'sv-mode
  :type 'boolean)

;;;###autoload
(defcustom sv-mode-line-up-paren t
  "*Non-nil means indent items in parentheses relative to the '('.
Otherwise indent them as usual."
  :group 'sv-mode
  :type 'boolean)

;;;###autoload
(defcustom sv-mode-finish-skeleton-function
  'sv-mode-default-finish-skeleton-function
  "*Function to call to finish task/function skeleton creation."
  :group 'sv-mode
  :type 'function)

;;;###autoload
(defcustom sv-mode-doxymacs-blank-singleline-comment-template
  '("/** " > p " */" > n )
  "*Doxymacs blank single line comment template."
  :group 'sv-mode
  :type 'sexp)

;;;###autoload
(defcustom sv-mode-doxymacs-blank-multiline-comment-template
  '("/**" > n "* " p > n "*/" > n )
  "*Doxymacs blank multi-line comment template."
  :group 'sv-mode
  :type 'sexp)

;;;###autoload
(defcustom sv-mode-doxymacs-function-comment-template
  '((let* ((proto (sv-mode-parse-prototype))
           (args (cdr (assoc 'args proto)))
           (ret (cdr (assoc 'ret proto))))
      (list 'l
            "/**" '> 'n
            " * " 'p '> 'n
            (when args
              '(l " *" '> 'n))
            (doxymacs-parm-tempo-element (mapcar 'car args))
            (when (and ret (not (string= ret "void")))
              '(l " *" '> 'n " * " (doxymacs-doxygen-command-char)
                  "return " (p "Returns: ") > n))
            " */" '> 'n 'n)))
  "*Doxymacs function comment template."
  :group 'sv-mode
  :type 'sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables/constants

(defconst sv-mode-end-regexp
  (concat "\\_<\\("
          (regexp-opt '("end" "endcase" "endclass" "endclocking" "endconfig"
                        "join" "join_any" "join_none" "endfunction"
                        "endgenerate" "endgroup" "endinterface" "endmodule"
                        "endpackage" "endprimitive" "endprogram" "endproperty"
                        "endspecify" "endsequence" "endtable" "endtask"
                        ;; AOP
                        "endextends"))
          "\\_>\\)")
  "End keyword regexp.")

(defconst sv-mode-begin-regexp
  (concat "\\_<\\("
          (regexp-opt '("begin" "case" "class" "clocking" "config" "fork"
                        "function" "generate" "covergroup" "interface" "module"
                        "package" "primitive" "program" "property" "specify"
                        "sequence" "table" "task"
                        ;; AOP
                        "extends"))
          "\\_>\\)")
  "Begin keyword regexp.")

(defconst sv-mode-end-to-begin-alist
  '(("end" . "begin")
    ("endcase" . "case")
    ("endclass" . "class")
    ("endclocking" . "clocking")
    ("endconfig" . "config")
    ("endfunction" . "function")
    ("endgenerate" . "generate")
    ("endgroup" . "covergroup")
    ("endinterface" . "interface")
    ("endmodule" . "module")
    ("endpackage" . "package")
    ("endprimitive" . "primitive")
    ("endprogram" . "program")
    ("endproperty" . "property")
    ("endspecify" . "specify")
    ("endsequence" . "sequence")
    ("endtable" . "table")
    ("endtask" . "task")
    ("join" . "fork")
    ("join_any" . "fork")
    ("join_none" . "fork")
    ;; AOP
    ("endextends" . "extends"))
  "Alist from ending keyword to begin regexp.")

(defconst sv-mode-begin-to-end-alist
  '(("begin" . "end")
    ("case" . "endcase")
    ("class" . "endclass")
    ("clocking" . "endclocking")
    ("config" . "endconfig")
    ("function" . "endfunction")
    ("generate" . "endgenerate")
    ("covergroup" . "endgroup")
    ("interface" . "endinterface")
    ("module" . "endmodule")
    ("package" . "endpackage")
    ("primitive" . "endprimitive")
    ("program" . "endprogram")
    ("property" . "endproperty")
    ("specify" . "endspecify")
    ("sequence" . "endsequence")
    ("table" . "endtable")
    ("task" . "endtask")
    ("fork" . "join\\|join_any\\|join_none")
    ;; AOP
    ("extends" . "endextends"))
  "Alist from beginning keyword to end regexp.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock

(defvar sv-mode-keywords
  (concat "\\_<\\("
          (regexp-opt
           '("alias" "always" "always_comb" "always_ff" "always_latch" "and"
             "assert" "assign" "assume" "automatic" "before" "begin" "bind"
             "bins" "binsof" "break" "buf" "bufif0" "bufif1" "case" "casex"
             "casez" "cell" "class" "clocking" "cmos" "config" "const"
             "constraint" "context" "continue" "cover" "covergroup" "coverpoint"
             "cross" "deassign" "default" "defparam" "design" "disable" "dist"
             "do" "edge" "else" "end" "endcase" "endclass" "endclocking"
             "endconfig" "endfunction" "endgenerate" "endgroup" "endinterface"
             "endmodule" "endpackage" "endprimitive" "endprogram" "endproperty"
             "endspecify" "endsequence" "endtable" "endtask" "enum" "expect"
             "export" "extends" "extern" "final" "first_match" "for" "force"
             "foreach" "forever" "fork" "forkjoin" "function" "generate"
             "genvar" "if" "iff" "ifnone" "ignore_bins" "illegal_bins" "import"
             "incdir" "include" "initial" "inout" "input" "inside" "instance"
             "interface" "intersect" "join" "join_any" "join_none" "large"
             "liblist" "library" "local" "localparam" "macromodule" "matches"
             "medium" "modport" "module" "nand" "negedge" "new" "nmos" "nor"
             "noshowcancelled" "not" "notif0" "notif1" "or" "output" "package"
             "packed" "parameter" "pmos" "posedge" "primitive" "priority"
             "program" "property" "protected" "pulldown" "pullup"
             "pulsestyle_onevent" "pulsestyle_ondetect" "pure" "rand" "randc"
             "randcase" "randsequence" "rcmos" "ref" "release" "repeat" "return"
             "rnmos" "rpmos" "rtran" "rtranif0" "rtranif1" "scalared" "sequence"
             "showcancelled" "small" "solve" "specify" "specparam" "static"
             "struct" "super" "table" "tagged" "task" "this" "throughout"
             "timeprecision" "timeunit" "tran" "tranif0" "tranif1" "type"
             "typedef" "union" "unique" "use" "var" "vectored" "virtual" "void"
             "wait" "wait_order" "while" "wildcard" "with" "within" "xnor"
             "xor"))
          "\\)\\_>"))

(defvar sv-mode-builtin-types
  (concat "\\_<\\("
          (regexp-opt
           '(
             "bit" "byte" "chandle" "event" "int" "integer" "logic" "longint"
             "real" "realtime" "reg" "shortint" "shortreal" "signed" "string"
             "time" "tri" "tri0" "tri1" "triand" "trior" "trireg" "unsigned"
             "wand" "wire" "wor"
             "semaphore" "mailbox"
             ))
          "\\)\\_>"))

(defvar sv-mode-const
  (concat "\\_<\\("
          (regexp-opt
           '(
             "highz0" "highz1" "null" "pull0" "pull1" "strong0" "strong1"
             "supply0" "supply1" "weak0" "weak1"
             ))
          "\\)\\_>"))

(defvar sv-mode-directives
  (concat "\\_<`\\("
          (regexp-opt
           '("celldefine" "default_nettype" "define" "else" "elsif"
             "endcelldefine" "endif" "ifdef" "ifndef" "include" "line"
             "nounconnected_drive" "resetall" "timescale" "unconnected_drive"
             "undef"))
          "\\)\\_>"))

(defvar sv-mode-builtin-methods
  (concat "\\_<[$]\\("
          (regexp-opt
           '("assertkill" "assertoff" "asserton" "bits" "bitstoshortreal"
             "cast" "coverage_control" "coverage_get" "coverage_get_max"
             "coverage_merge" "coverage_save" "dimensions" "display" "dumpfile"
             "dumpports" "dumpportsall" "dumpportsflush" "dumpportslimit"
             "dumpportsoff" "dumpportson" "dumpvars" "error" "exit" "fatal"
             "fclose" "fdisplay" "ferror" "fflush" "fgetc" "fgets" "finish"
             "fmonitor" "fopen" "fread" "fscanf" "fseek" "fstrobe" "ftell"
             "fwrite" "high" "increment" "info" "isunbounded" "isunknown"
             "left" "low" "monitor" "onehot" "onehot0" "random" "readmemb"
             "readmemh" "realtime" "rewind" "right" "sformat" "shortrealtobits"
             "signed" "size" "srandom" "sscanf" "stop" "strobe" "swrite"
             "swriteb" "swriteh" "swriteo" "time" "timeformat" "typename"
             "typeof" "ungetc" "unsigned" "urandom" "urandom_range" "void"
             "warning" "write" "writememb" "writememh"
             ;; Not in the LRM, but most simulators support
             "psprintf"))
          "\\)\\_>"))

(defvar sv-mode-aop-keywords
  (concat "\\_<\\("
          (regexp-opt
           '( "after" "around" "before" "dominates" "endextends" "hide"
              "proceed" "rules" "virtuals" ))
          "\\)\\_>"))

(defvar sv-mode-font-lock-keywords
  (list
   ;; Keywords
   (cons sv-mode-keywords '(0 font-lock-keyword-face keep))
   ;; Builtin types
   (cons sv-mode-builtin-types '(0 font-lock-type-face))
   ;; Constants
   (cons sv-mode-const '(0 font-lock-constant-face))
   ;; Preprocessor directives
   (cons sv-mode-directives '(0 font-lock-preprocessor-face)))
  "Default highlighting for sv-mode.")

(defvar sv-mode-font-lock-keywords-1
  (append sv-mode-font-lock-keywords
          (list
           ;; Builtin methods
           (cons sv-mode-builtin-methods '(0 font-lock-builtin-face))
           ;; Macros
           (cons "`[a-zA-Z0-9_]+" '(0 font-lock-variable-name-face))
           ;; Defines
           (cons "^\\s-*`\\(define\\|ifdef\\|ifndef\\|undef\\)\\s-+\\([a-zA-Z0-9_]+\\)"
                 '(2 font-lock-variable-name-face))))
  "Subdued level highlighting for sv-mode.")

(defvar sv-mode-font-lock-keywords-2
  (append sv-mode-font-lock-keywords-1
          (list
           ;; Scope resolution
           (cons "\\([a-zA-Z0-9_]+\\)::" '(1 font-lock-type-face))
           ;; Tasks/functions/programs
           (cons "^\\s-*\\(\\(extern\\|local\\|protected\\|virtual\\|forkjoin\\|before\\|after\\|around\\)\\s-+\\)*\\(task\\|function\\|program\\)\\s-+.*?\\([a-zA-Z0-9_]+\\)\\s-*[(;]"
                 '(4 font-lock-function-name-face t))
           ;; Labels
           (cons (concat sv-mode-end-regexp "\\s-*:\\s-*\\([a-zA-Z0-9_]+\\)")
                 '(2 font-lock-constant-face t))))
  "Medium level highlighting for sv-mode.")

(defvar sv-mode-font-lock-keywords-3
  (append sv-mode-font-lock-keywords-2
          (list
           ;; AOP keywords
           (cons sv-mode-aop-keywords '(0 font-lock-keyword-face keep))
           ;; Clocking
           (cons "#+[0-9]+"
                 '(0 font-lock-constant-face))
           (cons "@"
                 '(0 font-lock-constant-face))
           ;; Time
           (cons "\\_<[0-9.]+[munpf]?s"
                 '(0 font-lock-constant-face))
           ;; User types
           (cons "^\\s-*\\(\\(typedef\\|virtual\\)\\s-+\\)*\\(class\\|struct\\|enum\\|module\\|interface\\)\\s-+\\([a-zA-Z0-9_]+\\)"
                 '(4 font-lock-type-face))
           (cons "\\_<extends\\s-+\\([a-zA-Z0-9_:]+\\)"
                 '(1 font-lock-type-face))))
  "Gaudy level highlighting for sv-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitutions

(defsubst sv-mode-in-comment-or-string ()
  "Return 'comment if point is inside a comment, 'string if point is inside a
string, or nil if neither."
  (let ((pps (syntax-ppss)))
    (catch 'return
      (when (nth 4 pps)
        (throw 'return 'comment))
      (when (nth 3 pps)
        (throw 'return 'string)))))

(defsubst sv-mode-in-comment ()
  "Return non-nil if inside a comment."
  (nth 4 (syntax-ppss)))

(defsubst sv-mode-re-search-forward (REGEXP &optional BOUND NOERROR)
  "Like `re-search-forward', but skips over comments and strings.
Never throws an error; if NOERROR is anything other nil or t
move to limit of search and return nil."
  (let ((pos (point)))
    (when (equal NOERROR t)
      (setq NOERROR nil))
    (condition-case nil
        (catch 'done
          (while (re-search-forward REGEXP BOUND NOERROR)
            (unless (sv-mode-in-comment-or-string)
              (throw 'done (point)))))
      (error
       (goto-char pos)
       nil))))

(defsubst sv-mode-re-search-backward (REGEXP &optional BOUND NOERROR)
  "Like `re-search-backward', but skips over comments and strings.
Never throws an error; if NOERROR is anything other nil or t
move to limit of search and return nil."
  (let ((pos (point)))
    (when (equal NOERROR t)
      (setq NOERROR nil))
    (condition-case nil
        (catch 'done
          (while (re-search-backward REGEXP BOUND NOERROR)
            (unless (sv-mode-in-comment-or-string)
              (throw 'done (point)))))
      (error
       (goto-char pos)
       nil))))

(defsubst sv-mode-trim-whitespace (string)
  "Trim leading/trailing whitespace from a string."
  (when string
    (replace-regexp-in-string "\\(^[ \t]*\\|[ \t]*$\\)" "" string)))

(defsubst sv-mode-trim-brackets (string)
  "Trim any bracketed expression from a string."
  (when string
    (replace-regexp-in-string "\\[[^]]*\\]" "" string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun sv-mode-backward-sexp ()
  "Go backward over s-expression.  Matching of begin/task/module/etc. to
end/endtask/endmodule/etc. is done if you are on or after the end expression."
  (backward-sexp)
  (when (looking-at sv-mode-end-regexp)
    (let* ((end-string (match-string-no-properties 0))
           (begin-string (cdr (assoc end-string sv-mode-end-to-begin-alist)))
           (regexp (concat "\\_<\\(" end-string "\\|" begin-string "\\)\\_>"))
           (depth 1))
      (while (and (> depth 0) (sv-mode-re-search-backward regexp nil t))
        (if (looking-at end-string)
            (setq depth (1+ depth))
          (unless (sv-mode-decl-only-p)
            (setq depth (1- depth))))))))

(defun sv-mode-forward-sexp ()
  "Go forward over s-expression.  Matching of begin/task/module/etc. to
end/endtask/endmodule/etc. is done if you are on or before the begin
expression."
  (forward-sexp)
  (when (looking-back sv-mode-begin-regexp (line-beginning-position))
    (let* ((begin-string (match-string-no-properties 0))
           (end-string (cdr (assoc begin-string sv-mode-begin-to-end-alist)))
           (regexp (concat "\\_<\\(" begin-string "\\|" end-string "\\)\\_>"))
           (depth 1))
      (unless (and (string= begin-string "extends")
                   (sv-mode-class-decl-p))
        (while (and (> depth 0) (sv-mode-re-search-forward regexp nil t))
          (if (string= (match-string-no-properties 0) begin-string)
              (progn (backward-char)
                     (unless (sv-mode-decl-only-p)
                       (setq depth (1+ depth))))
            (setq depth (1- depth))))))))

(defun sv-mode-decl-only-p ()
  "Looks backward from point to see if an item is just a declaration."
  (save-excursion
    (let ((pos (point)))
      (sv-mode-beginning-of-statement)
      (re-search-forward "\\_<\\(extern\\|typedef\\)\\_>" pos t))))

(defun sv-mode-class-decl-p ()
  "Is the current statement a class declaration?"
  (save-excursion
    (sv-mode-beginning-of-statement)
    (looking-at "class")))

(defun sv-mode-determine-end-expr ()
  "Determine what the next appropriate end expression should be."
  (let (begin-type end-type label)
    (save-excursion
      (sv-mode-beginning-of-scope)
      (when (looking-at "[[({]")
        (error "Inside open parentheses, backets, or braces"))
      (re-search-forward "\\sw+" (line-end-position) t)
      (setq begin-type (match-string-no-properties 0))
      (setq end-type (cdr (assoc begin-type sv-mode-begin-to-end-alist)))
      (when (member begin-type (list "task" "function" "program"))
        (re-search-forward "\\([a-zA-Z0-9_]+\\)\\s-*[(;]" nil t)
        (setq label (match-string-no-properties 1)))
      (when (member begin-type (list "class" "module" "interface" "extends"))
        (re-search-forward "[a-zA-Z0-9_]+" nil t)
        (setq label (match-string-no-properties 0))))
    (concat end-type (if label (concat " : " label) ""))))

(defun sv-mode-get-namespaces ()
  "Return a list with all encompassing namespaces in out-to-in order."
  (let ((namespaces '())
        begin-type)
    (save-excursion
      (while (sv-mode-beginning-of-scope)
        (unless (looking-at "[[({]")
          (re-search-forward "\\sw+" (line-end-position))
          (setq begin-type (match-string-no-properties 0))
          (when (member begin-type (list "task" "function" "program"))
            (re-search-forward "\\([a-zA-Z0-9_]+\\)\\s-*[(;]" nil t)
            (push (match-string-no-properties 1) namespaces))
          (when (member begin-type (list "class" "module" "interface" "extends"))
            (re-search-forward "[a-zA-Z0-9_]+" nil t)
            (push (match-string-no-properties 0) namespaces)))
        (beginning-of-line)))
    namespaces))

(defun sv-mode-parse-prototype ()
  "Parse a function/task prototype and return an alist with the structure:
'(('type . TYPE)
  ('name . NAME)
  ('ret . TYPE)
  ('args . '((NAME . TYPE)
             (NAME . TYPE))))"
  (let ((type "")
        (name "")
        (ret nil)
        (args '())
        pos)
    (save-excursion
      (sv-mode-beginning-of-statement)
      (re-search-forward "task\\|function")
      (setq type (match-string-no-properties 0))
      (re-search-forward "\\s-+.*?\\([a-zA-Z0-9_:]+\\)\\s-*[(;]")
      (setq name (match-string-no-properties 1))
      (when (string= type "function")
        (goto-char (match-beginning 1))
        (setq pos (point))
        (re-search-backward "\\_<\\(task\\|function\\|static\\|automatic\\)\\_>")
        (forward-word)
        (re-search-forward ".*" pos)
        (setq ret (match-string-no-properties 0))
        (setq ret (sv-mode-trim-whitespace ret))
        (when (string= ret "")
          (setq ret nil))
        (re-search-forward "[(;]"))
      (when (= (char-before) ?\()
        (re-search-forward "\\([^)]*\\))")
        (let ((arg-string (match-string-no-properties 1))
              arg-strings arg-name arg-type)
          (setq arg-string (replace-regexp-in-string "\n" " " arg-string))
          (setq arg-strings (split-string arg-string "," t))
          (dolist (str arg-strings)
            (when (string-match "\\(.+\\)=" str)
              (setq str (match-string 1 str)))
            (setq str (sv-mode-trim-whitespace str))
            (if (string-match "\\(.+\\)\\s-+\\(.+\\)$" str)
                (setq arg-type (match-string 1 str)
                      arg-name (match-string 2 str))
              (setq arg-name str))
            (setq args (cons (cons (sv-mode-trim-whitespace arg-name)
                                   (sv-mode-trim-whitespace arg-type)) args))))
        (when args
          (setq args (nreverse args)))))
      (list (cons 'type type)
            (cons 'name name)
            (cons 'ret ret)
            (cons 'args args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

(defun sv-mode-beginning-of-statement ()
  "Move to beginning of statement."
  (interactive)
  (skip-syntax-forward " >")
  (let* ((limit (point))
         (regexp (concat "[;{}]\\|\\_<\\(begin\\|fork\\|do\\|case\\)\\_>\\|"
                         (regexp-opt '("`define" "`else" "`elsif" "`endif"
                                       "`ifdef" "`ifndef" "`include"
                                       "`timescale" "`undef")) ".+\\|"
                                       sv-mode-end-regexp))
         (matched (sv-mode-re-search-backward regexp nil 'go))
         (end (match-end 0)))
    (unless (and (looking-at ";")
                 (re-search-backward "\\_<for\\_>\\s-*(" (line-beginning-position) t))
      (when matched
        (if (looking-at "case")
            (forward-sexp 2)
          (goto-char end)
          (when (looking-at "\\s-*:\\s-*[a-zA-Z0-9_]+")
            (goto-char (match-end 0))))
        (skip-syntax-forward " >" limit)
        (while (and (or (looking-at "//\\|/\\*") (sv-mode-in-comment-or-string))
                    (not (eobp)))
          (forward-line)
          (skip-syntax-forward " >" limit))))
    (back-to-indentation)))

(defun sv-mode-jump-other-end ()
  "Jumps to the opposite begin/end expression from the one point is at."
  (interactive)
  (let ((pos (point)))
    (skip-syntax-backward "w_")
    (if (re-search-forward
         (concat sv-mode-begin-regexp "\\|" sv-mode-end-regexp) nil t)
        (progn
          (backward-word)
          (if (looking-at sv-mode-end-regexp)
              (progn
                (forward-char)
                (sv-mode-backward-sexp))
            (when (looking-at sv-mode-begin-regexp)
              (sv-mode-forward-sexp)
              (backward-word))))
      (goto-char pos))))

(defun sv-mode-beginning-of-block ()
  "Go to the beginning of the previous block."
  (interactive)
  (beginning-of-line)
  (when (sv-mode-re-search-backward sv-mode-end-regexp nil 'go)
    (forward-word)
    (sv-mode-backward-sexp)
    (sv-mode-beginning-of-statement)
    (while (and (looking-at "else") (not (bobp)))
      (when (sv-mode-re-search-backward sv-mode-end-regexp nil 'go)
        (forward-word)
        (sv-mode-backward-sexp)
        (sv-mode-beginning-of-statement)))
    (beginning-of-line)))

(defun sv-mode-end-of-block ()
  "Go to the end of the next block."
  (interactive)
  (beginning-of-line)
  (when (sv-mode-re-search-forward sv-mode-begin-regexp nil 'go)
    (backward-word)
    (sv-mode-forward-sexp)
    (while (and (looking-at "[ \t\n]*else") (not (eobp)))
      (when (sv-mode-re-search-forward sv-mode-begin-regexp nil 'go)
        (backward-word)
        (sv-mode-forward-sexp)))
    (forward-line)))

(defun sv-mode-beginning-of-scope ()
  "Like `backward-up-list', but matches begin/task/module/etc. and
end/endtask/endmodule/etc. also."
  (interactive)
  (condition-case nil
      (progn (backward-up-list) t)
    (error
     (let ((pos (point))
           (regexp (concat sv-mode-begin-regexp "\\|" sv-mode-end-regexp))
           (depth 1))
       (while (and (> depth 0) (sv-mode-re-search-backward regexp nil t))
         (if (looking-at "end\\|join")
             (setq depth (1+ depth))
           (let ((begin-string (match-string-no-properties 0)))
             (unless (or (sv-mode-decl-only-p)
                         (and (string= begin-string "extends")
                              (sv-mode-class-decl-p)))
               (setq depth (1- depth))))))
       (when (> depth 0)
         (goto-char pos)
         (when (interactive-p)
           (error "Unbalanced parentheses or begin/end constructs")))
       (= depth 0)))))

(defun sv-mode-end-of-scope ()
  "Like `up-list', but matches begin/task/module/etc. and
end/endtask/endmodule/etc. also."
  (interactive)
  (condition-case nil
      (progn (up-list) t)
    (error
     (let ((pos (point))
           (regexp (concat sv-mode-begin-regexp "\\|" sv-mode-end-regexp))
           done)
       (while (and (not done) (sv-mode-re-search-forward regexp nil t))
         (backward-word)
         (if (not (looking-at "end\\|join"))
             (if (sv-mode-decl-only-p)
                 (forward-word)
               (sv-mode-forward-sexp))
           (forward-word)
           (setq done t)))
       (unless done
         (goto-char pos)
         (when (interactive-p)
           (error "Unbalanced parentheses or begin/end constructs")))
       done))))

(defun sv-mode-create-skeleton-from-prototype ()
  "Turn a task/function prototype into a skeleton implementation."
  (interactive)
  (let ((proto (sv-mode-parse-prototype))
        pos lim next-func-type namespaces next-func-re)
    (save-excursion
      (setq namespaces (sv-mode-get-namespaces))
      (sv-mode-re-search-forward ";" nil t)
      (setq pos (point))
      (sv-mode-end-of-scope)
      (setq lim (point))
      (goto-char pos)
      (when (sv-mode-re-search-forward
             "\\(task\\|function\\)\\s-+.*?\\([a-zA-Z0-9_]+\\)\\s-*[(;]" lim t)
        (setq next-func-type (match-string-no-properties 1))
        (setq next-func-re (match-string-no-properties 2))
        (dolist (ns (reverse namespaces))
          (setq next-func-re (concat ns "::" next-func-re)))
        (setq next-func-re (concat next-func-type ".+\\_<" next-func-re "\\_>")))
      (ff-get-other-file)
      (goto-char (point-min))
      (if (not next-func-re)
          (goto-char (point-max))
        (when (sv-mode-re-search-forward next-func-re nil 'go)
          (sv-mode-beginning-of-statement)
          (sv-mode-re-search-backward
           (concat";\\|" (regexp-opt '("`define" "`else" "`elsif" "`endif"
                                       "`ifdef" "`ifndef" "`include"
                                       "`timescale" "`undef")) ".+\\|"
                                       sv-mode-end-regexp))
          (forward-line 1)))
      (insert "\n")
      (insert (cdr (assoc 'type proto)) " ")
      (when (cdr (assoc 'ret proto))
        (insert (cdr (assoc 'ret proto)) " "))
      (dolist (ns namespaces)
        (insert ns "::"))
      (insert (cdr (assoc 'name proto)) "(")
      (dolist (arg (cdr (assoc 'args proto)))
        (insert (cdr arg) " " (car arg) ", "))
      (when (cdr (assoc 'args proto))
        (delete-char -2))
      (insert ");\n")
      (insert (sv-mode-determine-end-expr) "\n")
      (forward-line -2)
      (funcall sv-mode-finish-skeleton-function proto namespaces))))

(defun sv-mode-default-finish-skeleton-function (proto namespaces)
  "Default finish task/function skeleton function.  PROTO is the parsed
function/task prototype, and NAMESPACES is the list of namespaces."
  (save-excursion
    (end-of-line)
    (insert "\n// @todo Implement this " (cdr (assoc 'type proto)))
    (sv-mode-indent-line)
    (forward-line -2)
    (insert "\n")
    (sv-mode-indent-line)
    (insert-char ?/ (- 80 (- (point) (point-at-bol))))
    (insert "\n// ")
    (sv-mode-indent-line)
    (dolist (ns namespaces)
      (insert ns "::"))
    (insert (cdr (assoc 'name proto)))
    (insert "\n")))

(defun sv-mode-electric-star ()
  "Auto-indent when in comments."
  (interactive)
  (insert "*")
  (when (sv-mode-in-comment)
    (sv-mode-indent-line)))

(defun sv-mode-indent-new-comment-line ()
  "Break line at point and indent if in comment."
  (interactive)
  (when (sv-mode-in-comment)
    (let ((single-line (save-excursion
                         (re-search-backward "//" (line-beginning-position) t))))
      (newline)
      (if single-line
          (insert "// ")
        (insert "* "))
      (just-one-space 1)
      (sv-mode-indent-line))))

(defun sv-mode-fill-paragraph (&optional arg)
  "Fill paragraph function.
Optional ARG means justify paragraph as well."
  (if (not (sv-mode-in-comment))
      t
    (save-excursion
      (beginning-of-line)
      (unless (search-forward "//" (line-end-position) t)
        (beginning-of-line)
        (let (fill-prefix from to)
          (if (search-forward "/*" (line-end-position) t)
              (progn
                (backward-char 2)
                (setq from (point))
                (skip-syntax-forward "^w_")
                (setq fill-prefix (make-string (current-column) 32)))
            (re-search-backward "^[* \t]*$\\|/\\*")
            (forward-line 1)
            (setq from (point))
            (setq fill-prefix (buffer-substring-no-properties
                               (point)
                               (progn (skip-syntax-forward "^w_") (point)))))
          (re-search-forward "^[* \t]*$\\|\\*/" nil t)
          (unless (looking-back "\\*/" (line-beginning-position))
            (beginning-of-line))
          (setq to (point))
          (fill-region from to arg)
          t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defun sv-mode-indent-line ()
  "Indent the current line."
  (interactive)
  (let ((pos (- (point-max) (point)))
        (indent (sv-mode-get-indent)))
    (unless (= indent (current-indentation))
      (back-to-indentation)
      (delete-region (line-beginning-position) (point))
      (indent-to indent))
    (if (< (- (point) (line-beginning-position)) indent)
        (back-to-indentation)
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos))))))

(defun sv-mode-get-indent ()
  "Return how much the current line should be indented."
  (save-excursion
    (beginning-of-line)
    (let ((in-comment-or-string (sv-mode-in-comment-or-string)))
      (cond
       ((bobp) 0)
       ((equal in-comment-or-string 'string) 0)
       ((equal in-comment-or-string 'comment)
        (sv-mode-get-indent-in-comment))
       ((sv-mode-get-indent-if-in-paren))
       ((sv-mode-get-indent-if-opener))
       ((sv-mode-get-indent-if-closer))
       ((sv-mode-get-normal-indent))))))

(defun sv-mode-get-indent-in-comment ()
  "Get amount to indent when in comment."
  (let ((starts-with-* (looking-at "\\s-*\\*")))
    (save-excursion
      (forward-line -1)
      (while (and (looking-at "^\\s-*$") (not (bobp)))
        (forward-line -1))
      (if (sv-mode-in-comment-or-string)
          (back-to-indentation)
        (re-search-forward "/\\*")
        (backward-char))
;;       (unless (and starts-with-* (= (char-after) ?*))
;;         (skip-syntax-forward "^w_" (line-end-position)))
      (current-column))))

(defun sv-mode-get-indent-if-in-paren ()
  "Get amount to indent if in parentheses, brackets, or braces"
  (save-excursion
    (condition-case nil
        (let ((at-closer (looking-at "[ \t]*[])}]"))
              offset)
          (backward-up-list)
          (if (= (char-after) ?{)
              (progn
                (sv-mode-beginning-of-statement)
                (when (looking-at "\\(typedef\\s-+\\)?enum")
                  (setq offset (current-column))
                  (unless at-closer
                    (setq offset (+ offset sv-mode-basic-offset)))
                  offset))
            (unless (or (and (= (char-after) ?\[) (not sv-mode-line-up-bracket))
                        (and (= (char-after) ?\() (not sv-mode-line-up-paren)))
              (setq offset (current-column))
              (unless at-closer
                (forward-char)
                (if (and (> (skip-syntax-forward " >" (line-end-position)) 0)
                         (not (looking-at "//\\|/\\*")))
                    (setq offset (current-column))
                  (setq offset (1+ offset))))
              offset)))
      (error nil))))

(defun sv-mode-get-indent-if-opener ()
  "Get amount to indent if looking at a opening item."
  (when (looking-at "^[ \t]*\\({\\|\\_<begin\\_>\\)")
    (sv-mode-beginning-of-statement)
    (current-column)))

(defun sv-mode-get-indent-if-closer ()
  "Get amount to indent if looking at a closing item."
  (if (looking-at "^[ \t]*}")
      (progn
        (backward-up-list)
        (sv-mode-beginning-of-statement)
        (current-column))
    (when (looking-at (concat "^[ \t]*" sv-mode-end-regexp))
      (forward-word)
      (sv-mode-backward-sexp)
      (sv-mode-beginning-of-statement)
      (current-column))))

(defun sv-mode-get-normal-indent ()
  "Normal indent for a line."
  (save-excursion
    (let ((bol (point))
          (offset 0))
      (sv-mode-beginning-of-statement)
      (when (> bol (line-beginning-position))
        (setq offset sv-mode-basic-offset))
      (if (not (sv-mode-beginning-of-scope))
          0
        (sv-mode-beginning-of-statement)
        (+ (current-column) sv-mode-basic-offset offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar sv-mode-syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?\$ "_" table)
    (modify-syntax-entry ?\` "_" table)

    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?# "." table)
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

    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23" table)
    (modify-syntax-entry ?\n "> b" table)

    table)
  "Syntax table used in sv-mode buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev table

(defvar sv-mode-abbrev-table nil
  "*Abbrev table in use in sv-mode buffers.")

(define-abbrev-table 'sv-mode-abbrev-table ())

(define-abbrev sv-mode-abbrev-table
  "beg"
  ""
  (lambda()
    (insert "begin")
    (sv-mode-indent-line)
    (insert "\n\nend")
    (sv-mode-indent-line)
    (forward-line -1)
    (sv-mode-indent-line)))

(define-abbrev sv-mode-abbrev-table
  "end"
  ""
  (lambda()
    (insert (sv-mode-determine-end-expr))
    (sv-mode-indent-line)))

(define-abbrev sv-mode-abbrev-table
  "sup"
  ""
  (lambda()
    (insert "super." (car (last (sv-mode-get-namespaces))) "(")
    (let (args)
      (save-excursion
        (while (and (sv-mode-beginning-of-scope)
                    (not (looking-at "task\\|function"))))
        (setq args (cdr (assoc 'args (sv-mode-parse-prototype)))))
      (dolist (arg args)
        (insert (sv-mode-trim-brackets (car arg)) ", "))
      (when args
        (delete-char -2))
      (insert ");"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imenu

;; TODO Make this cooler, e.g. heirarchical in speedbar, find properties, etc.

(defun sv-mode-imenu-create-index-function ()
  "Create sv-mode Imenu index."
  (let ((item-alist '())
        item-type)
    (goto-char (point-min))
    (while (sv-mode-re-search-forward
            "\\_<\\(class\\|struct\\|enum\\|module\\|interface\\|task\\|function\\|program\\)\\_>"
            nil 'go)
      (setq item-type (match-string-no-properties 1))
      (if (member item-type (list "class" "struct" "enum" "module" "interface"))
          (when (or (not (string= item-type "class"))
                    (save-excursion
                      (backward-word 2)
                      (not (looking-at "typedef"))))
            (sv-mode-re-search-forward "[ \t\n]+\\([a-zA-Z0-9_]+\\)")
            (push (cons (concat (match-string-no-properties 1) " <" item-type ">")
                        (match-beginning 1))
                  item-alist))
        (sv-mode-re-search-forward "\\(.\\|\n\\)+?\\([a-zA-Z0-9_:]+\\)\\s-*[(;]")
        (push (cons (concat (match-string-no-properties 2) " <" item-type ">")
                    (match-beginning 2))
              item-alist)))
    (nreverse item-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Align

(require 'align)

(defvar sv-mode-align-rules-list
  `(
    (sv-mode-class-item
     (regexp . "\\(\\s-+\\)\\([a-zA-Z0-9_:]+\\)\\s-*\\([(;]\\|\\[$\\]\\)")
     (group . (1)))

    (sv-mode-assignment
     (regexp . "\\(\\s-*\\)\\(<=\\|=\\)\\(\\s-*\\)")
     (group . (1 3))
     (valid . ,(function
                (lambda ()
                  (condition-case nil
                      (save-excursion
                        (backward-up-list)
                        (not (= (char-after) ?\()))
                    (error t))))))

    (sv-mode-comment
     (regexp . "\\(\\s-*\\)/[/*]"))
    )
  "sv-mode alignment rules.")

(defvar sv-mode-align-exclude-rules-list nil
  "sv-mode alignment exclusion rules.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode map

(defvar sv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "*") 'sv-mode-electric-star)
    (define-key map (kbd "C-M-j") 'sv-mode-indent-new-comment-line)
    (define-key map (kbd "C-c C-j") 'sv-mode-jump-other-end)
    (define-key map (kbd "C-c C-u") 'sv-mode-beginning-of-scope)
    (define-key map (kbd "C-c C-d") 'sv-mode-end-of-scope)
    (define-key map (kbd "C-c C-a") 'sv-mode-beginning-of-statement)
    (define-key map (kbd "C-c C-p") 'sv-mode-beginning-of-block)
    (define-key map (kbd "C-c C-n") 'sv-mode-end-of-block)
    (define-key map (kbd "C-c C-s") 'sv-mode-create-skeleton-from-prototype)
    (define-key map (kbd "C-c C-o") 'ff-get-other-file)
    map)
  "Keymap used in sv-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SystemVerilog mode

;;;###autoload
(defun sv-mode ()
  "Major mode for editing SystemVerilog code.

Key Bindings:

\\{sv-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sv-mode)
  (setq mode-name "sv-mode")

  (use-local-map sv-mode-map)

  (set-syntax-table sv-mode-syntax-table)

  (setq local-abbrev-table sv-mode-abbrev-table)

  (make-local-variable 'sv-mode-basic-offset)
  (make-local-variable 'sv-mode-continued-line-offset)
  (make-local-variable 'sv-mode-line-up-bracket)
  (make-local-variable 'sv-mode-line-up-paren)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'fill-paragraph-function)
  (setq comment-start "// "
        comment-end ""
        comment-multi-line t
        comment-start-skip "//[ \t]*"
        parse-sexp-ignore-comments t
        indent-line-function 'sv-mode-indent-line
        fill-paragraph-function 'sv-mode-fill-paragraph)

  ;; Font-lock

  (setq font-lock-defaults
        '((sv-mode-font-lock-keywords
           sv-mode-font-lock-keywords-1
           sv-mode-font-lock-keywords-2
           sv-mode-font-lock-keywords-3)
          nil ;; nil means highlight strings & comments as well as keywords
          nil ;; nil means keywords must match case
          nil ;; use minimal syntax table for font lock
          nil ;; TODO Function to move to beginning of reasonable region to highlight
          ))
  (turn-on-font-lock)

  ;; Other-file

  (setq ff-other-file-alist '(("\\.sv$" (".svh"))
                              ("\\.svh$" (".sv"))))

  ;; Imenu

  (setq imenu-generic-expression nil
        imenu-create-index-function 'sv-mode-imenu-create-index-function)

  ;; Align rules

  (setq align-mode-rules-list sv-mode-align-rules-list)
  (setq align-mode-exclude-rules-list sv-mode-align-exclude-rules-list)

  ;; Doxygen

  (set (make-local-variable 'doxymacs-JavaDoc-blank-singleline-comment-template)
       sv-mode-doxymacs-blank-singleline-comment-template)

  (set (make-local-variable 'doxymacs-JavaDoc-blank-multiline-comment-template)
       sv-mode-doxymacs-blank-multiline-comment-template)

  (set (make-local-variable 'doxymacs-JavaDoc-function-comment-template)
       sv-mode-doxymacs-function-comment-template)

  ;; Hooks

  (run-hooks 'sv-mode-hook))

(provide 'sv-mode)
;;; sv-mode.el ends here
