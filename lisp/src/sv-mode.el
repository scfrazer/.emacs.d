;;; sv-mode.el --- Major mode for editing SystemVerilog files

;; Copyright (C) 2010  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 10 Aug 2010
;; Version: 2.0
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
;; C-c C-O : Look for function definition/implementation in "other" file
;; C-c C-s : Create (or update) skeleton task/function implementation in
;;           .sv file from prototype on current line in .svh file
;; TODO C-c C-r: Rename function in .svh/.sv -- fix endfunction and any super calls
;;
;; TODO C-x n s, M-a, M-e and such
;; TODO UVM utils and end/super etc insertion
;; TODO Package/tag guessing functions
;; TODO Abbrevs
;;
;; This mode supports the insertion of Doxygen comments if you use the
;; doxymacs package.

;;; Code:

(require 'custom)
(require 'find-file)

(defconst sv-mode-version "2.0"
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
(defcustom sv-mode-line-up-brace t
  "*Non-nil means indent items in parentheses relative to the '{'.
Otherwise indent them as usual."
  :group 'sv-mode
  :type 'boolean)

;;;###autoload
(defcustom sv-mode-macros-without-semi
  '("`uvm_[a-z_]+")
  "*List of regexps of macros that do not have to end with a semicolon"
  :group 'sv-mode
  :type '(repeat regexp))

;;;###autoload
(defcustom sv-mode-macros-begin
  '("`uvm_field_utils_begin"
    "`uvm_object_utils_begin"
    "`uvm_object_param_utils_begin"
    "`uvm_component_utils_begin"
    "`uvm_component_param_utils_begin")
  "*List of macros that start a block"
  :group 'sv-mode
  :type '(repeat string))

;;;###autoload
(defcustom sv-mode-macros-end
  '("`uvm_field_utils_end"
    "`uvm_object_utils_end"
    "`uvm_component_utils_end")
  "*List of macros that end a block"
  :group 'sv-mode
  :type '(repeat string))

;;;###autoload
(defcustom sv-mode-macros-begin-to-end-alist
  '(("`uvm_field_utils_begin" . "`uvm_field_utils_end")
    ("`uvm_object_utils_begin" . "`uvm_object_utils_end")
    ("`uvm_object_param_utils_begin" . "`uvm_object_utils_end")
    ("`uvm_component_utils_begin" . "`uvm_component_utils_end")
    ("`uvm_component_param_utils_begin" . "`uvm_component_utils_end"))
  "*Alist mapping begin macros to end macros."
  :group 'sv-mode
  :type 'alist)

;;;###autoload
(defcustom sv-mode-macros-end-to-begin-alist
  '(("`uvm_field_utils_end" . "`uvm_field_utils_begin")
    ("`uvm_object_utils_end" . "`uvm_object_\\(param_\\)?utils_begin")
    ("`uvm_component_utils_end" . "`uvm_component_\\(param_\\)?utils_begin"))
  "*Alist mapping end macros to begin macros."
  :group 'sv-mode
  :type 'alist)

;;;###autoload
(defcustom sv-mode-opener-is-electric t
  "*Non-nil means reindent the current line after entering 'begin' or '{'."
  :group 'sv-mode
  :type 'boolean)

;;;###autoload
(defcustom sv-mode-closer-is-electric t
  "*Non-nil means reindent the current line after entering 'end' or '}'."
  :group 'sv-mode
  :type 'boolean)

;;;###autoload
(defcustom sv-mode-highlight-match-delay 0.5
  "*Time in seconds to delay before showing matching scope begin/end."
  :group 'sv-mode
  :type 'number)

;;;###autoload
(defcustom sv-mode-guess-package-name-function 'sv-mode-guess-package-name
  "*Function to guess the package name for the current file."
  :group 'sv-mode
  :type 'function)

;;;###autoload
(defcustom sv-mode-guess-uvm-tag-function 'sv-mode-guess-uvm-tag
  "*Function to guess the UVM message tag for the current file."
  :group 'sv-mode
  :type 'function)

;;;###autoload
(defcustom sv-mode-uvm-info-function 'sv-mode-uvm-info
  "*Function to call to insert a `uvm_info message."
  :group 'sv-mode
  :type 'function)

;;;###autoload
(defcustom sv-mode-uvm-err-function 'sv-mode-uvm-err
  "*Function to call to insert a `uvm_(warning|error|fatal) message."
  :group 'sv-mode
  :type 'function)

;;;###autoload
(defcustom sv-mode-finish-skeleton-function
  'sv-mode-default-finish-skeleton-function
  "*Function to call to finish task/function skeleton creation."
  :group 'sv-mode
  :type 'function)

;;;###autoload
(defcustom sv-mode-doxymacs-blank-singleline-comment-template
  '("//! " > p (unless (looking-at "\\s-*$") 'n))
  "*Doxymacs blank single line comment template."
  :group 'sv-mode
  :type 'sexp)

;;;###autoload
(defcustom sv-mode-doxymacs-blank-multiline-comment-template
  '("/*!" > n "* " p > n "*/" > (unless (looking-at "\\s-*$") 'n))
  "*Doxymacs blank multi-line comment template."
  :group 'sv-mode
  :type 'sexp)

;;;###autoload
(defcustom sv-mode-doxymacs-function-comment-template
  '((let* ((proto (sv-mode-parse-prototype))
           (args (cdr (assoc 'args proto)))
           (ret (cdr (assoc 'ret proto))))
      (list 'l
            "/*!" '> 'n
            " * \\brief " 'p '> 'n
            (when args
              '(l " *" '> 'n))
            (doxymacs-parm-tempo-element (mapcar 'car args))
            (when (and ret (not (string= ret "void")))
              '(l " *" '> 'n " * " (doxymacs-doxygen-command-char)
                  "return " (p "Returns: ") > n))
            " */" '>
            (unless (looking-at "\\s-*$") 'n))))
  "*Doxymacs function comment template."
  :group 'sv-mode
  :type 'sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables/constants

(defvar sv-mode-begin-regexp nil
  "Begin keyword regexp (calculated at mode start).")

(defvar sv-mode-end-regexp nil
  "End keyword regexp (calculated at mode start).")

(defvar sv-mode-begin-end-regexp nil
  "Begin/end regexp (calculated at mode start).")

(defvar sv-mode-begin-to-end-alist nil
  "Alist from beginning keyword to end regexp (calculated at mode start).")

(defvar sv-mode-end-to-begin-alist nil
  "Alist from ending keyword to begin regexp (calculated at mode start).")

(defvar sv-mode-macros-without-semi-regexp nil
  "Macros without semicolon regexp (calculated at mode start).")

(defvar sv-mode-eos-regexp nil
  "End of scope regexp (calculated at mode start).")

(defvar sv-mode-package-name nil
  "Package name for current file.")
(make-variable-buffer-local 'sv-mode-package-name)

(defvar sv-mode-uvm-tag nil
  "Tag to use in UVM messages.")
(make-variable-buffer-local 'sv-mode-uvm-tag)

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
             "semaphore" "mailbox" "process"
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
             "psprintf" "sformatf"))
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
           (cons "^\\s-*\\(\\(static\\|extern\\|pure\\|local\\|protected\\|virtual\\|forkjoin\\|before\\|after\\|around\\)\\s-+\\)*\\(task\\|function\\|program\\)\\s-+.*?\\([a-zA-Z0-9_]+\\)\\s-*[(;]"
                 '(4 font-lock-function-name-face t))
           ;; Instances
           (cons "^\\s-*\\([a-zA-Z0-9_:]+\\)\\s-+\\(#\\s-*([^)]*)\\s-+\\)?\\([a-zA-Z0-9_]+\\)\\s-*("
                 '((1 font-lock-type-face)
                   (3 font-lock-variable-name-face)))
           ;; Labels
           (cons (concat "\\_<"
                         (regexp-opt '("begin" "end" "fork" "join" "join_any" "join_none"
                                       "endclass" "endclocking" "endconfig" "endfunction"
                                       "endgroup" "endinterface" "endmodule" "endpackage"
                                       "endprimitive" "endprogram" "endproperty"
                                       "endsequence" "endtask"))
                         "\\_>\\s-*:\\s-*\\([a-zA-Z0-9_]+\\)")
                 '(1 font-lock-constant-face t))
           ;; Parameters
           (cons "\\(#\\)[^#0-9]" '(1 font-lock-variable-name-face))))
  "Medium level highlighting for sv-mode.")

(defvar sv-mode-font-lock-keywords-3
  (append sv-mode-font-lock-keywords-2
          (list
           ;; Warn you about hex ... since I do it all the time
           (cons "\\_<0x[0-9a-fA-F]+\\_>"
                 '(0 font-lock-warning-face))
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
    (let* ((exact-end-string (match-string-no-properties 0))
           (end-string (if (member exact-end-string (list "join" "join_any" "join_none"))
                           "join\\|join_any\\|join_none"
                         exact-end-string))
           (begin-string (cdr (assoc exact-end-string sv-mode-end-to-begin-alist)))
           (regexp (concat "\\_<\\(" end-string "\\|" begin-string "\\)\\_>"))
           (depth 1))
      (while (and (> depth 0) (sv-mode-re-search-backward regexp nil 'go))
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
        (while (and (> depth 0) (sv-mode-re-search-forward regexp nil 'go))
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
      (re-search-forward "\\_<\\(extern\\|typedef\\|import\\)\\_>" pos t))))

(defun sv-mode-class-decl-p ()
  "Is the current statement a class declaration?"
  (save-excursion
    (sv-mode-beginning-of-statement)
    (looking-at "\\(virtual\\s-+\\)?class")))

(defun sv-mode-determine-end-expr ()
  "Determine what the next appropriate end expression should be."
  (let (begin-type end-type label)
    (save-excursion
      (sv-mode-beginning-of-scope)
      (when (looking-at "[[({]")
        (error "Inside open parentheses, backets, or braces"))
      (re-search-forward "[`a-zA-Z0-9_]+" (line-end-position) t)
      (setq begin-type (match-string-no-properties 0))
      (setq end-type (cdr (assoc begin-type sv-mode-begin-to-end-alist)))
      ;; No labels allowed in AOP files ... this is hacky
      (when (and buffer-file-name
                 (not (equal (file-name-extension buffer-file-name) "aop")))
        (when (member begin-type (list "task" "function" "program" "package"))
          (re-search-forward "\\([a-zA-Z0-9_]+\\)\\s-*[(;]" nil t)
          (setq label (match-string-no-properties 1)))
        (when (member begin-type (list "class" "module" "interface"))
          (re-search-forward "[a-zA-Z0-9_]+" nil t)
          (setq label (match-string-no-properties 0)))))
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
          (when (member begin-type (list "class" "module" "interface"))
            (re-search-forward "[a-zA-Z0-9_]+" nil t)
            (push (match-string-no-properties 0) namespaces))
          (when (string= begin-type "extends")
            (re-search-backward "class[ \t\n]+\\([a-zA-Z0-9_]+\\)" nil t)
            (push (match-string-no-properties 1) namespaces)))
        (beginning-of-line)))
    namespaces))

(defun sv-mode-get-class-type ()
  "Return the enclosing class type and any parameters."
  (let (name parameters)
    (save-excursion
      (save-restriction
        (widen)
        (sv-mode-beginning-of-defun)
        (when (looking-at "\\(virtual\\s-+\\)?class\\s-+\\([a-zA-Z0-9_]+\\)")
          (setq name (match-string-no-properties 2))
          (goto-char (match-end 0))
          (when (looking-at "\\s-*#\\s-*(")
            (goto-char (1- (match-end 0)))
            (let ((end-pos (save-excursion (forward-sexp) (point))))
              (while (re-search-forward "type\\s-+\\([`a-zA-Z0-9_]+\\)" end-pos t)
                (push (match-string-no-properties 1) parameters)))))))
    (list name (nreverse parameters))))

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
        (setq pos (point))
        (re-search-forward ")\\s-*;")
        (let ((arg-string (buffer-substring-no-properties pos (match-beginning 0)))
              arg-strings arg-name arg-type)
          (setq arg-string (replace-regexp-in-string "\n" " " arg-string))
          (with-temp-buffer
            (insert arg-string)
            (goto-char (point-min))
            (setq pos (point))
            (while (not (eobp))
              (forward-sexp)
              (when (looking-at "\\s-*,")
                (push (buffer-substring-no-properties pos (point)) arg-strings)
                (search-forward ",")
                (setq pos (point))))
            (push (buffer-substring-no-properties pos (point)) arg-strings))
          (dolist (str arg-strings)
            (when (string-match "\\(.+\\)=" str)
              (setq str (match-string 1 str)))
            (setq str (sv-mode-trim-whitespace str))
            (if (string-match "\\(.+\\)\\s-+\\(.+\\)$" str)
                (setq arg-type (match-string 1 str)
                      arg-name (match-string 2 str))
              (setq arg-name str))
            (unless (string= str "")
              (setq args (cons (cons (sv-mode-trim-whitespace arg-name)
                                     (sv-mode-trim-whitespace arg-type)) args)))))))
    (list (cons 'type type)
          (cons 'name name)
          (cons 'ret ret)
          (cons 'args args))))

(defun sv-mode-guess-package-name ()
  "Guess the package name for the current file.
By default assumes if a file named '*_pkg.sv' exists in the current directory,
that is the package name.  Users should change
`sv-mode-guess-package-name-function' to point to their own function."
  (unless sv-mode-package-name
    (let ((filenames (file-expand-wildcards "*_pkg.sv")))
      (if filenames
          (progn
            (string-match "\\(.+\\)\.sv" (car filenames))
            (setq sv-mode-package-name (match-string 1 (car filenames))))
        (setq sv-mode-package-name "")))))

(defun sv-mode-get-package-name (&optional with-scope-resolution)
  "Return the package name.  If WITH-SCOPE-RESOLUTION is non-nil, suffix it
with the :: operator (if there is a package)."
  (sv-mode-guess-package-name)
  (if with-scope-resolution
      (if (string= sv-mode-package-name "")
          ""
        (concat sv-mode-package-name "::"))
    sv-mode-package-name))

(defun sv-mode-guess-uvm-tag ()
  "Guess the UVM message tag for the current file.
By default looks for a already existing tag in the current file, then
uses the package name if one can be found (see
`sv-mode-guess-package-name-function'), then gives up and returns a
default.  Users should change `sv-mode-guess-uvm-tag-function' to point
to their own function."
  ;; Look in file first
  (unless sv-mode-uvm-tag
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward "`uvm_\\(info\\|warning\\|error\\|fatal\\).+?\"\\([a-zA-Z0-9_]+\\)" nil t)
          (setq sv-mode-uvm-tag (match-string-no-properties 2))))))
  ;; Use the package name if one can be found
  (unless sv-mode-uvm-tag
    (let ((pkg (sv-mode-get-package-name)))
      (unless (string= pkg "")
        (setq sv-mode-uvm-tag (upcase pkg)))))
  ;; Give up
  (unless sv-mode-uvm-tag
    (setq sv-mode-uvm-tag "MSG")))

(defun sv-mode-uvm-info (verbosity)
  "Insert a `uvm_info message.
Users should change `sv-mode-uvm-info-function' to point to their
own function.  This function can be called through abbrevs."
  (interactive "sVerbosity? ")
  (sv-mode-guess-uvm-tag)
  (insert "`uvm_info(\"" sv-mode-uvm-tag "\", \"TODO\", " verbosity ");")
  (sv-mode-indent-line)
  (search-backward "TODO"))

(defun sv-mode-uvm-err (type)
  "Insert a `uvm_(warning|error|fatal) message.
Users should change `sv-mode-uvm-err-function' to point to their
own function.  This function can be called through abbrevs."
  (interactive "sType? ")
  (sv-mode-guess-uvm-tag)
  (insert "`uvm_" type "(\"" sv-mode-uvm-tag "\", \"TODO\");")
  (sv-mode-indent-line)
  (search-backward "TODO"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

(defvar sv-mode-bos-regexp nil) ;; Calculate this when the mode starts

(defun sv-mode-beginning-of-statement ()
  "Move to beginning of statement."
  (interactive)
  (skip-syntax-forward " >")
  ;; TODO Optimize this by narrowing buffer to current class/function/task/etc.
  (condition-case nil (backward-up-list 1000) (error nil))
  (let ((limit (point)) matched end)
    ;; Look for previous statement or delimiter
    (while (not (or matched (bobp)))
      (setq matched (sv-mode-re-search-backward sv-mode-bos-regexp nil 'go))
      (when matched
        (setq end (match-end 0))
        ;; Go backward over sexp's to avoid problems with 'for', constraints, and macros
        (when (looking-at "[})]")
          (forward-char)
          (backward-sexp)
          (if (= (char-after) ?\()
              (setq matched nil)
            ;; Constraints can end with just a curly brace and no semicolon
            (backward-sexp 2)
            (if (looking-at "constraint")
                (forward-sexp 3)
              (setq matched nil))))))
    ;; Found an anchor?
    (when matched
      (cond
       ;; Case
       ((looking-at "case")
        (forward-sexp 2))
       ;; Macros without semicolons
       ((looking-at sv-mode-macros-without-semi-regexp)
        (goto-char end)
        (let ((num-sexps 1))
          (when (looking-at "\\s-*(")
            (forward-sexp 1)
            (setq num-sexps 2))
          ;; If started inside the current macro, go back to beginning
          (when (> (point) limit)
            (backward-sexp num-sexps))))
       ;; Everything else
       (t
        (goto-char end)
        (when (looking-at "\\s-*:\\s-*[a-zA-Z0-9_]+")
          (goto-char (match-end 0))))))
    ;; Skip over comments and whitespace
    (forward-comment (buffer-size))))

(defun sv-mode-jump-other-end ()
  "Jumps to the opposite begin/end expression from the one point is at."
  (interactive)
  (let ((pos (point)))
    (skip-syntax-backward "w_")
    (if (re-search-forward sv-mode-begin-end-regexp nil t)
        (progn
          (skip-syntax-backward "w_")
          (if (looking-at sv-mode-end-regexp)
              (progn
                (forward-char)
                (sv-mode-backward-sexp))
            (when (looking-at sv-mode-begin-regexp)
              (sv-mode-forward-sexp)
              (skip-syntax-backward "w_"))))
      (goto-char pos))))

(defun sv-mode-beginning-of-block ()
  "Go to the beginning of the previous block."
  (interactive)
  (beginning-of-line)
  (if (looking-at "\\s-*`\\(else\\|endif\\)")
      (sv-mode-backward-ifdef)
    (when (sv-mode-re-search-backward sv-mode-end-regexp nil 'go)
      (forward-word)
      (sv-mode-backward-sexp)
      (sv-mode-beginning-of-statement)
      (while (and (looking-at "else") (not (bobp)))
        (when (sv-mode-re-search-backward sv-mode-end-regexp nil 'go)
          (forward-word)
          (sv-mode-backward-sexp)
          (sv-mode-beginning-of-statement)))
      (beginning-of-line))))

(defun sv-mode-backward-ifdef ()
  "Go from `else/`endif to `ifdef/`ifndef"
  (interactive)
  (when (looking-at "\\s-*`\\(else\\|endif\\)")
    (let ((depth 1) str)
      (catch 'done
        (while (sv-mode-re-search-backward "^\\s-*`\\(if\\|else\\|endif\\)" nil 'move)
          (setq str (match-string-no-properties 1))
          (cond ((string= str "endif")
                 (setq depth (1+ depth)))
                ((string= str "if")
                 (setq depth (1- depth))
                 (when (= depth 0)
                   (throw 'done t)))
                ((when (= depth 1)
                   (throw 'done t)))))))
    (beginning-of-line)))

(defun sv-mode-end-of-block ()
  "Go to the end of the next block."
  (interactive)
  (beginning-of-line)
  (if (looking-at "\\s-*`\\(ifn?def\\|else\\)")
      (sv-mode-forward-ifdef)
    (when (sv-mode-re-search-forward sv-mode-begin-regexp nil 'go)
      (backward-word)
      (sv-mode-forward-sexp)
      (while (and (looking-at "[ \t\n]*else") (not (eobp)))
        (when (sv-mode-re-search-forward sv-mode-begin-regexp nil 'go)
          (backward-word)
          (sv-mode-forward-sexp)))
      (forward-line))))

(defun sv-mode-forward-ifdef ()
  "Go from `ifdef/`ifndef/`else to `else/`endif"
  (interactive)
  (when (looking-at "\\s-*`\\(ifn?def\\|else\\)")
    (forward-line)
    (let ((depth 1) str)
      (catch 'done
        (while (sv-mode-re-search-forward "^\\s-*`\\(if\\|else\\|endif\\)" nil 'move)
          (setq str (match-string-no-properties 1))
          (cond ((string= str "if")
                 (setq depth (1+ depth)))
                ((string= str "endif")
                 (setq depth (1- depth))
                 (when (= depth 0)
                   (throw 'done t)))
                ((when (= depth 1)
                   (throw 'done t)))))))
    (beginning-of-line)))

(defun sv-mode-beginning-of-scope ()
  "Like `backward-up-list', but matches begin/task/module/etc. and
end/endtask/endmodule/etc. also."
  (interactive)
  (condition-case nil
      (progn (backward-up-list) t)
    (error
     (let ((pos (point))
           (depth 1))
       (while (and (> depth 0) (sv-mode-re-search-backward sv-mode-begin-end-regexp nil t))
         (if (looking-at sv-mode-eos-regexp)
             (setq depth (1+ depth))
           (let ((begin-string (match-string-no-properties 0)))
             (unless (or (sv-mode-decl-only-p)
                         (and (string= begin-string "extends")
                              (sv-mode-class-decl-p))
                         (and (string= begin-string "fork")
                              (looking-back "\\_<\\(disable\\|wait\\)\\_>\\s-+"
                                            (line-beginning-position)))
                         (and (string= begin-string "function")
                              (looking-back "\\_<with\\_>\\s-+"
                                            (line-beginning-position))))
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
     (let ((pos (point)) done)
       (while (and (not done) (sv-mode-re-search-forward sv-mode-begin-end-regexp nil t))
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

(defun sv-mode-beginning-of-defun (&optional arg)
  "Go to beginning of function/task/class."
  (interactive)
  (let (pos)
    (save-excursion
      (end-of-line)
      (while (not (looking-at "task\\|function\\|class"))
        (unless (sv-mode-beginning-of-scope)
          (error "Not inside a defun")))
      (setq pos (point)))
    (goto-char pos)
    (beginning-of-line)
    t))

(defun sv-mode-end-of-defun (&optional arg)
  "Go to end of function/task/class."
  (interactive)
  (let (pos)
    (save-excursion
      (sv-mode-beginning-of-defun)
      (sv-mode-jump-other-end)
      (setq pos (line-end-position)))
    (goto-char pos)
    t))

(defun sv-mode-narrow-to-scope ()
  "Narrow buffer to enclosing scope."
  (interactive)
  (let (start)
    (save-excursion
      (end-of-line)
      (unless (sv-mode-beginning-of-scope)
        (error "Not inside any scope"))
      (setq start (line-beginning-position))
      (sv-mode-jump-other-end)
      (forward-line 1)
      (narrow-to-region start (point)))))

(defun sv-mode-goto-function-other-file ()
  "Go to function/task in other file."
  (interactive)
  (let* ((proto (sv-mode-parse-prototype))
         (name (cdr (assoc 'name proto)))
         (in-impl (string-match "::" name))
         (namespaces nil)
         (this-func-re "\\(task\\|function\\)\\s-+.*?"))
    (if (not in-impl)
        (setq namespaces (sv-mode-get-namespaces))
      (setq namespaces (split-string name "::" t))
      (setq name (car (last namespaces)))
      (setq namespaces (nbutlast namespaces)))
    (ff-get-other-file)
    (goto-char (point-min))
    (dolist (ns namespaces)
      (if in-impl
          (unless (re-search-forward (concat "\\_<class\\s-+" ns "\\_>"))
            (error "Couldn't find function/task implementation"))
        (setq this-func-re (concat this-func-re ns "::"))))
    (setq this-func-re (concat this-func-re name "\\s-*[(;]"))
    (if (sv-mode-re-search-forward this-func-re nil t)
        (beginning-of-line)
      (error "Couldn't find function/task implementation"))))

(defun sv-mode-create-skeleton-from-prototype ()
  "Turn a task/function prototype into a skeleton implementation."
  (interactive)
  (let ((proto (sv-mode-parse-prototype))
        pos lim next-func-type namespaces next-func-re this-func-re)
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
      ;; If task/function already exists, update it
      (setq this-func-re "\\(task\\|function\\)\\s-+.*?")
      (dolist (ns namespaces)
        (setq this-func-re (concat this-func-re ns "::")))
      (setq this-func-re (concat this-func-re (cdr (assoc 'name proto)) "\\s-*[(;]"))
      (if (sv-mode-re-search-forward this-func-re nil t)
          ;; TODO Fix any super calls
          (progn
            (sv-mode-re-search-backward "task\\|function" nil t)
            (beginning-of-line)
            (setq pos (point))
            (sv-mode-re-search-forward ";" nil t)
            (delete-region pos (point))
            (sv-mode-insert-prototype proto namespaces))
        ;; Task/function doesn't exist, insert a blank one
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
        (sv-mode-insert-prototype proto namespaces)
        (insert "\n" (sv-mode-determine-end-expr) "\n")
        (forward-line -2)
        (funcall sv-mode-finish-skeleton-function proto namespaces)))))

(defun sv-mode-insert-prototype (proto namespaces)
  "Insert prototype"
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
  (insert ");"))

(defun sv-mode-default-finish-skeleton-function (proto namespaces)
  "Default finish task/function skeleton function.  PROTO is the parsed
function/task prototype, and NAMESPACES is the list of namespaces."
  (save-excursion
    (end-of-line)
    (insert "\n//! \\todo Implement this " (cdr (assoc 'type proto)))
    (sv-mode-indent-line)
    (forward-line -2)
    (insert "\n")
    (sv-mode-indent-line)
    (insert-char ?/ (- 80 (- (point) (point-at-bol))))
;;     (insert "\n// ")
;;     (sv-mode-indent-line)
;;     (dolist (ns namespaces)
;;       (insert ns "::"))
;;     (insert (cdr (assoc 'name proto)))
    (insert "\n")))

(defun sv-mode-electric-star ()
  "Auto-indent when in comments."
  (interactive)
  (insert "*")
  (when (sv-mode-in-comment)
    (sv-mode-indent-line)))

(defun sv-mode-electric-begin ()
  "Auto-indent after the word 'begin'."
  (interactive)
  (insert "n")
  (when (and sv-mode-opener-is-electric
             (looking-back "\\_<begin" (point-at-bol))
             (not (sv-mode-in-comment-or-string)))
    (sv-mode-indent-line)))

(defun sv-mode-electric-end ()
  "Auto-indent after the word 'end'."
  (interactive)
  (insert "d")
  (when (and sv-mode-closer-is-electric
             (looking-back "\\_<end" (point-at-bol))
             (not (sv-mode-in-comment-or-string)))
    (let (indent-line)
      (save-excursion
        (save-restriction
          (widen)
          (beginning-of-line)
          (when (looking-at "^[ \t]*end")
            (forward-word)
            (sv-mode-backward-sexp)
            (sv-mode-beginning-of-statement)
            (setq indent-line (or (> (current-column) 0)
                                  (looking-at "\\_<fork\\_>"))))))
      (when indent-line
        (sv-mode-indent-line)))))

(defun sv-mode-electric-curly-open ()
  "Auto-indent after entering '{'."
  (interactive)
  (insert "{")
  (when (and sv-mode-opener-is-electric
             (not (sv-mode-in-comment-or-string)))
    (sv-mode-indent-line)))

(defun sv-mode-electric-curly-close ()
  "Auto-indent after entering '}'."
  (interactive)
  (insert "}")
  (when (and sv-mode-closer-is-electric
             (not (sv-mode-in-comment-or-string)))
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

(defun sv-mode-insert-end ()
  "Insert the appropriate end statement."
  (interactive)
  (insert (sv-mode-determine-end-expr))
  (sv-mode-indent-line))

(defun sv-mode-insert-super ()
  "Insert the appropriate super statement."
  (interactive)
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
    (insert ");")))

(defun sv-mode-insert-type ()
  "Insert the class type as a typedef."
  (interactive)
  (let* ((class-type (sv-mode-get-class-type))
         (name (nth 0 class-type))
         (param-list (nth 1 class-type))
         parameters)
    (when param-list
      (setq parameters "#(")
      (dolist (param param-list)
        (setq parameters (concat parameters param ",")))
      (setq parameters (concat (substring parameters 0 -1) ")")))
    (insert "typedef "
            (sv-mode-get-package-name t)
            name)
    (when parameters
      (insert parameters))
    (insert " this_t;")))

(defun sv-mode-uvm-component-utils (&optional begin)
  "Insert the appropriate UVM component utils declaration.
If BEGIN is non-nil, insert begin/end pair."
  (interactive "P")
  (sv-mode-uvm-utils "component" begin))

(defun sv-mode-uvm-object-utils (&optional begin)
  "Insert the appropriate UVM object utils declaration.
If BEGIN is non-nil, insert begin/end pair."
  (interactive "P")
  (sv-mode-uvm-utils "object" begin))

(defun sv-mode-uvm-utils (type begin)
  "Insert the appropriate UVM component/object utils declaration.
TYPE is component/object, and BEGIN non-nil inserts begin/end pair."
  (let* ((class-type (sv-mode-get-class-type))
         (name (nth 0 class-type))
         (param-list (nth 1 class-type))
         parameters)
    (when param-list
      (setq parameters "#(")
      (dolist (param param-list)
        (setq parameters (concat parameters param ",")))
      (setq parameters (concat (substring parameters 0 -1) ")")))
    (insert
     "`uvm_" type
     (if parameters "_param_utils" "_utils")
     (if begin "_begin(" "(")
     (sv-mode-get-package-name t)
     name
     (if parameters parameters "")
     ")")
    (sv-mode-indent-line)
    (when begin
      (let (pos)
        (insert "\n")
        (sv-mode-indent-line)
        (setq pos (point))
        (insert "\n`uvm_" type "_utils_end")
        (sv-mode-indent-line)
        (goto-char pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Idle matching of begin/end pairs

(defvar sv-mode-idle-timer nil
  "Idle timer in sv-mode.")

(defvar sv-mode-scope-overlay-1 nil
  "Overlay 1 used to show scope match.")

(defvar sv-mode-scope-overlay-2 nil
  "Overlay 2 used to show scope match.")

(defun sv-mode-idle-timer-fcn ()
  "In sv-mode, show the matching scope begin/end."
  (when (and (eq major-mode 'sv-mode)
             (not (sv-mode-in-comment-or-string)))
    (save-excursion
      (skip-syntax-backward "w_")
      (unless (and (looking-at "\\_<task\\|function\\_>")
                   (save-excursion
                     (sv-mode-beginning-of-statement)
                     (looking-at "\\_<extern\\|import\\|export\\|covergroup\\_>")))
        (when (looking-at sv-mode-begin-end-regexp)
          (let ((beg-1 (match-beginning 0))
                (end-1 (match-end 0))
                beg-2 end-2)
            (if (looking-at sv-mode-end-regexp)
                (progn
                  (forward-char)
                  (sv-mode-backward-sexp)
                  (when (looking-at sv-mode-begin-regexp)
                    (setq beg-2 (match-beginning 0)
                          end-2 (match-end 0))))
              (sv-mode-forward-sexp)
              (skip-syntax-backward "w_")
              (when (looking-at sv-mode-end-regexp)
                (setq beg-2 (match-beginning 0)
                      end-2 (match-end 0))))
            (if sv-mode-scope-overlay-1
                (move-overlay sv-mode-scope-overlay-1 beg-1 end-1)
              (setq sv-mode-scope-overlay-1 (make-overlay beg-1 end-1)))
            (if beg-2
                (progn
                  (overlay-put sv-mode-scope-overlay-1 'face 'show-paren-match-face)
                  (if sv-mode-scope-overlay-2
                      (move-overlay sv-mode-scope-overlay-2 beg-2 end-2)
                    (setq sv-mode-scope-overlay-2 (make-overlay beg-2 end-2)))
                  (overlay-put sv-mode-scope-overlay-2 'face 'show-paren-match-face))
              (overlay-put sv-mode-scope-overlay-1 'face 'show-paren-mismatch-face)))
          (add-hook 'pre-command-hook 'sv-mode-idle-timer-done))))))

(defun sv-mode-idle-timer-done ()
  "Remove matching scope overlays."
  (remove-hook 'pre-command-hook 'sv-mode-idle-timer-done)
  (when sv-mode-scope-overlay-1
    (delete-overlay sv-mode-scope-overlay-1))
  (when sv-mode-scope-overlay-2
    (delete-overlay sv-mode-scope-overlay-2)))

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
    (save-restriction
      (widen)
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
         ((sv-mode-get-normal-indent)))))))

(defun sv-mode-get-indent-in-comment ()
  "Get amount to indent when in comment."
  (save-excursion
    (forward-line -1)
    (while (and (looking-at "^\\s-*$") (not (bobp)))
      (forward-line -1))
    (if (sv-mode-in-comment-or-string)
        (back-to-indentation)
      (re-search-forward "/\\*")
      (backward-char))
    (current-column)))

(defun sv-mode-get-indent-if-in-paren ()
  "Get amount to indent if in parentheses, brackets, or braces"
  (save-excursion
    (condition-case nil
        (let ((at-closer (looking-at "[ \t]*[])}]"))
              (pos (point))
              beg offset)
          (backward-up-list)
          (if (= (char-after) ?{)
              (if (save-excursion
                    (sv-mode-beginning-of-statement)
                    (setq beg (line-beginning-position)
                          offset (current-column))
                    (looking-at "\\(default\\s-+\\|static\\s-+\\)?constraint"))
                  (sv-mode-get-indent-in-constraint offset beg pos at-closer)
                (when sv-mode-line-up-brace
                  (sv-mode-get-paren-lined-up-offset at-closer)))
            (unless (or (and (= (char-after) ?\[) (not sv-mode-line-up-bracket))
                        (and (= (char-after) ?\() (not sv-mode-line-up-paren)))
              (sv-mode-get-paren-lined-up-offset at-closer))))
      (error nil))))

(defun sv-mode-get-indent-in-constraint (offset beg pos at-closer)
  "Get amount to indent if in a constraint."
  (goto-char pos)
  (let ((at-opener (looking-at "\\s-*{")))
    (forward-comment (* -1 (buffer-size)))
    (unless (or at-opener at-closer (looking-back "[{};,]\\s-*"))
      (setq offset (+ offset sv-mode-basic-offset))))
  (goto-char pos)
  (save-restriction
    (narrow-to-region beg pos)
    (condition-case nil
        (while t
          (backward-up-list)
          (when (looking-at "{\\s-*\\(.\\)")
            (goto-char (match-beginning 1))
            (setq offset (current-column))
            (error nil))
          (setq offset (+ offset sv-mode-basic-offset)))
      (error nil))
    (when at-closer
      (setq offset (- offset sv-mode-basic-offset))))
  offset)

(defun sv-mode-get-paren-lined-up-offset (at-closer)
  "Get amount to indent in a lined-up paren block."
  (let ((offset (current-column)))
    (unless at-closer
      (forward-char)
      (if (and (> (skip-syntax-forward " >" (line-end-position)) 0)
               (not (looking-at "//\\|/\\*")))
          (setq offset (current-column))
        (setq offset (1+ offset))))
    offset))

(defun sv-mode-get-indent-if-opener ()
  "Get amount to indent if looking at a opening item."
  (when (looking-at "^[ \t]*\\({\\|\\_<begin\\_>\\)")
    (sv-mode-beginning-of-statement)
    (when (looking-at "\\_<begin\\_>")
      (skip-syntax-backward " >")
      (back-to-indentation))
    (if (looking-at "\\_<fork\\_>")
        (+ (current-column) sv-mode-basic-offset)
      (current-column))))

(defun sv-mode-get-indent-if-closer ()
  "Get amount to indent if looking at a closing item."
  (if (looking-at "^[ \t]*}")
      (progn
        (backward-up-list)
        (sv-mode-beginning-of-statement)
        (current-column))
    (when (looking-at (concat "^[ \t]*" sv-mode-end-regexp))
      (let ((closer (match-string-no-properties 1)))
        (forward-word)
        (sv-mode-backward-sexp)
        (sv-mode-beginning-of-statement)
        (back-to-indentation)
        (if (and (looking-at "\\_<fork\\_>")
                 (string= closer "end"))
            (+ (current-column) sv-mode-basic-offset)
          (current-column))))))

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
        (unless (looking-back "^\\s-*" (line-beginning-position))
          (sv-mode-beginning-of-statement)
          (back-to-indentation))
        (+ (current-column) sv-mode-basic-offset offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar sv-mode-syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?\$ "_" table)
    (modify-syntax-entry ?\` "_" table)
    (modify-syntax-entry ?_ "_" table)

    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    (modify-syntax-entry ?! "." table)
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
  (lambda ()
    (insert "begin")
    (sv-mode-indent-line)
    (insert "\n\nend")
    (sv-mode-indent-line)
    (forward-line -1)
    (sv-mode-indent-line)))

(define-abbrev sv-mode-abbrev-table
  "end"
  ""
  (lambda() (sv-mode-insert-end)))

(define-abbrev sv-mode-abbrev-table
  "sup"
  ""
  (lambda () (sv-mode-insert-super)))

(define-abbrev sv-mode-abbrev-table
  "pkg"
  ""
  (lambda () (insert (sv-mode-get-package-name))))

(define-abbrev sv-mode-abbrev-table
  "tt"
  ""
  (lambda () (sv-mode-insert-type)))

(define-abbrev sv-mode-abbrev-table
  "infn"
  ""
  (lambda() (funcall sv-mode-uvm-info-function "UVM_NONE")))

(define-abbrev sv-mode-abbrev-table
  "infl"
  ""
  (lambda() (funcall sv-mode-uvm-info-function "UVM_LOW")))

(define-abbrev sv-mode-abbrev-table
  "infm"
  ""
  (lambda() (funcall sv-mode-uvm-info-function "UVM_MEDIUM")))

(define-abbrev sv-mode-abbrev-table
  "infh"
  ""
  (lambda() (funcall sv-mode-uvm-info-function "UVM_HIGH")))

(define-abbrev sv-mode-abbrev-table
  "inff"
  ""
  (lambda() (funcall sv-mode-uvm-info-function "UVM_FULL")))

(define-abbrev sv-mode-abbrev-table
  "war"
  ""
  (lambda() (funcall sv-mode-uvm-err-function "warning")))

(define-abbrev sv-mode-abbrev-table
  "err"
  ""
  (lambda() (funcall sv-mode-uvm-err-function "error")))

(define-abbrev sv-mode-abbrev-table
  "fat"
  ""
  (lambda() (funcall sv-mode-uvm-err-function "fatal")))

(define-abbrev sv-mode-abbrev-table
  "cu"
  ""
  (lambda() (sv-mode-uvm-component-utils)))

(define-abbrev sv-mode-abbrev-table
  "cub"
  ""
  (lambda() (sv-mode-uvm-component-utils t)))

(define-abbrev sv-mode-abbrev-table
  "ou"
  ""
  (lambda() (sv-mode-uvm-object-utils)))

(define-abbrev sv-mode-abbrev-table
  "oub"
  ""
  (lambda() (sv-mode-uvm-object-utils t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imenu

(defvar sv-mode-imenu-simple t
  "*Make a simple imenu display with just classes, tasks, instances, etc.
nil means create a full tree with properties, show arguments/return values for
tasks and functions, etc.")

(defun sv-mode-imenu-create-index-function ()
  "Create sv-mode Imneu index."
  (if sv-mode-imenu-simple
      (sv-mode-imenu-create-index-function-simple)
    (sv-mode-imenu-create-index-function-complex)))

(defun sv-mode-imenu-create-index-function-simple ()
  "Create simple sv-mode Imenu index."
  (let ((item-alist '()) item-type item-name item)
    ;; Look for verification items
    (goto-char (point-min))
    (while (sv-mode-re-search-forward
            "\\_<\\(class\\|struct\\|enum\\|module\\|interface\\|package\\|task\\|function\\|program\\)\\_>"
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
    ;; Look for instances
    (goto-char (point-min))
    (while (sv-mode-re-search-forward
            "^\\s-*\\([a-zA-Z0-9_:]+\\)[ \t\n]+\\(#\\|[a-zA-Z0-9_]+\\)[ \t\n]*(" nil 'go)
      (setq item-type (match-string-no-properties 1))
      (setq item-name (match-string-no-properties 2))
      (if (not (string= item-name "#"))
          (setq item (cons (concat item-name " <" item-type ">") (match-beginning 2)))
        (backward-char)
        (forward-sexp)
        (re-search-forward "[ \t\n]*\\([a-zA-Z0-9_]+\\)" nil t)
        (setq item (cons (concat (match-string-no-properties 1) " <" item-type ">") (match-beginning 1))))
      (unless (string-match sv-mode-keywords item-type)
        (push item item-alist)))
    (nreverse item-alist)))

(defvar sv-mode-imenu-parse-ifdef-names nil
  "Stack of names from finding `ifn?def to fill in for a possible `else.")

(defun sv-mode-imenu-parse (item-alist limit parse-type)
  "Parse the buffer up to LIMIT and add items to ITEM-ALIST.
PARSE-TYPE is 'class, 'module, or nil for anything else."
  (let (item-type (start-pos (point)))
    (while (sv-mode-re-search-forward
            "\\_<\\(`ifn?def\\|`else\\|`endif\\|`include\\|`define\\|class\\|module\\|interface\\|package\\|struct\\|enum\\|task\\|function\\|program\\)\\_>"
            limit 'go)
      (setq item-type (match-string-no-properties 1))
      ;; If parsing a class or module, look for other items in the buffer
      ;; section that was skipped over
      (if (equal parse-type 'class)
          (setq item-alist (sv-mode-imenu-parse-properties item-alist start-pos (match-beginning 0)))
        (when (equal parse-type 'module)
          (setq item-alist (sv-mode-imenu-parse-instances item-alist start-pos (match-beginning 0)))))
      (cond
       ;; `ifdefs
       ((member item-type (list "`ifdef" "`ifndef" "`else"))
        (let (name (pos (match-beginning 1)) sub-list sub-limit)
          (save-excursion
            (beginning-of-line)
            (sv-mode-forward-ifdef)
            (setq sub-limit (point)))
          (if (not (string= item-type "`else"))
              (progn (sv-mode-re-search-forward "[ \t\n]+\\([a-zA-Z0-9_]+\\)")
                     (setq name (match-string-no-properties 1))
                     (setq pos (match-beginning 1))
                     (push (if (string= item-type "`ifdef") (concat "!" name) name)
                           sv-mode-imenu-parse-ifdef-names))
            (if sv-mode-imenu-parse-ifdef-names
                (setq name (car sv-mode-imenu-parse-ifdef-names))
              (setq name "???")))
          (push (cons item-type pos) sub-list)
          (push (cons name (sv-mode-imenu-parse sub-list sub-limit parse-type)) item-alist)))
       ;; `endif
       ((string= item-type "`endif")
        (when sv-mode-imenu-parse-ifdef-names
          (setq sv-mode-imenu-parse-ifdef-names (cdr sv-mode-imenu-parse-ifdef-names))))
       ;; Includes
       ((string= item-type "`include")
        (when (re-search-forward "\".+\"" (line-end-position) 'go)
          (push (cons (concat (match-string-no-properties 0) " : " item-type)
                      (point-at-bol)) item-alist)))
       ;; Defines
       ((string= item-type "`define")
        (re-search-forward "\\s-+\\([a-zA-Z0-9_]+\\)" (line-end-position) 'go)
        (push (cons (concat (match-string-no-properties 1) " : " item-type)
                    (match-beginning 1)) item-alist)
        (while (and (looking-at ".*\\\\\\s-*$") (not (eobp)))
          (forward-line 1))
        (unless (bolp)
          (forward-line 1)))
       ;; Things with other things inside
       ((member item-type (list "class" "module" "interface" "package"))
        (when (or (not (string= item-type "class"))
                  (save-excursion
                    (backward-word 2)
                    (not (looking-at "typedef"))))
          (sv-mode-re-search-forward "[ \t\n]+\\([a-zA-Z0-9_]+\\)")
          (let* ((name (match-string-no-properties 1))
                 (parse-sub-type (cond ((string= item-type "class") 'class)
                                       ((string= item-type "module") 'module)
                                       (t nil)))
                 sub-list sub-limit
                 (depth 1)
                 (regexp (concat "\\_<\\(" item-type "\\|end" item-type "\\)\\_>")))
            (push (cons item-type (match-beginning 1)) sub-list)
            (save-excursion
              (catch 'done
                (while (sv-mode-re-search-forward regexp limit 'go)
                  (if (string= (match-string 1) item-type)
                      (setq depth (1+ depth))
                    (setq depth (1- depth))
                    (when (= depth 0)
                      (setq sub-limit (match-end 0))
                      (throw 'done t))))))
            (sv-mode-re-search-forward ";" nil 'go)
            (push (cons name (sv-mode-imenu-parse sub-list sub-limit parse-sub-type)) item-alist))))
       ;; User types
       ((member item-type (list "struct" "enum"))
        (unless (looking-at "\\s-*{")
          (search-forward "{" nil t)
          (backward-char))
        (when (looking-at "\\s-*{")
          (forward-sexp)
          (when (re-search-forward "\\s-*\\([a-zA-Z0-9_]+\\)" nil t)
            (push (cons (concat (match-string-no-properties 1) " : " item-type)
                        (match-beginning 1)) item-alist)
            (sv-mode-re-search-forward ";" nil 'go))))
       ;; Task/function/program
       ((member item-type (list "task" "function" "program"))
        (let ((qualifiers ""))
          (save-excursion
            (backward-word)
            (catch 'done
              (while (backward-word)
                (unless (sv-mode-in-comment-or-string)
                  (if (looking-at "static\\|extern\\|pure\\|local\\|protected\\|virtual")
                      ;; TODO Need some qualifier for 'pure'
                      (setq qualifiers
                            (concat (substring (match-string-no-properties 0) 0 1) qualifiers))
                    (when (looking-back "^\\s-*import.+" (point-at-bol))
                      (setq qualifiers (concat "i" qualifiers)))
                    (throw 'done t))))))
          (unless (string= qualifiers "")
            (setq qualifiers (concat " [" qualifiers "]")))
          (sv-mode-re-search-forward "\\(.\\|\n\\)+?\\([a-zA-Z0-9_:]+\\)\\s-*[(;]")
          (push (cons (concat (match-string-no-properties 2) " : " item-type qualifiers)
                      (match-beginning 2)) item-alist)
          (backward-char)
          (forward-sexp)
          (unless (string-match "[ei]" qualifiers)
            (sv-mode-re-search-forward (concat "\\_<end" item-type "\\_>") nil 'go)))))
      (setq start-pos (point)))
    (if (equal parse-type 'class)
        (setq item-alist (sv-mode-imenu-parse-properties item-alist start-pos limit))
      (when (equal parse-type 'module)
        (setq item-alist (sv-mode-imenu-parse-instances item-alist start-pos limit)))))
  (nreverse item-alist))

(defun sv-mode-imenu-parse-properties (item-alist start limit)
  "Look for properties."
  (save-excursion
    (goto-char start)
    (let (item-type item-name item cover-type)
      (while (sv-mode-re-search-forward
              "\\([a-zA-Z0-9_:`]+\\(\\s-*\\[[^]]*\\]\\)*\\)\\s-*[=;]\\|\\_<\\(covergroup\\|constraint\\)\\_>"
              limit 'move)
        (setq cover-type (match-string-no-properties 3))
        (condition-case nil
            (cond ((and cover-type (string= cover-type "covergroup"))
                   (backward-sexp)
                   (sv-mode-forward-sexp))
                  ((and cover-type (string= cover-type "constraint"))
                   (forward-sexp 2))
                  (t
                   (let ((name-pos (match-beginning 1)) (qualifiers ""))
                     (setq item-name (sv-mode-trim-whitespace (match-string-no-properties 1)))
                     (backward-char)
                     (save-excursion
                       (sv-mode-beginning-of-statement)
                       (setq item-type (replace-regexp-in-string
                                        "[\t\n]+" " "
                                        (buffer-substring-no-properties (point) name-pos))))
                     (dolist (qualifier (list "static" "local" "protected" "rand"))
                       (when (string-match qualifier item-type)
                         (setq item-type (replace-regexp-in-string qualifier "" item-type)
                               qualifiers (concat qualifiers (substring qualifier 0 1)))))
                     (unless (string= qualifiers "")
                       (setq qualifiers (concat " [" qualifiers "]")))
                     (setq item (cons (concat item-name " : "
                                          (sv-mode-trim-whitespace
                                           (replace-regexp-in-string " +" " " item-type))
                                          qualifiers)
                             name-pos))
                 (push item item-alist)
                 (sv-mode-re-search-forward ";" limit 'move))))
          (error nil)))))
  item-alist)

(defun sv-mode-imenu-parse-instances (item-alist start limit)
  "Look for instances."
  (save-excursion
    (goto-char start)
    (let (item-type item-name item)
      (while (sv-mode-re-search-forward
              "^\\s-*\\([a-zA-Z0-9_:]+\\)[ \t\n]+\\(#\\|[a-zA-Z0-9_]+\\)[ \t\n]*("
              limit 'move)
        (setq item-type (match-string-no-properties 1))
        (setq item-name (match-string-no-properties 2))
        (if (not (string= item-name "#"))
            (setq item (cons (concat item-name " : " item-type) (match-beginning 2)))
          (backward-char)
          (forward-sexp)
          (sv-mode-re-search-forward "[ \t\n]*\\([a-zA-Z0-9_]+\\)[ \t\n]*(" nil 'go)
          (setq item (cons (concat (match-string-no-properties 1) " : " item-type) (match-beginning 1))))
        (unless (string-match sv-mode-keywords item-type)
          (push item item-alist))
        (backward-char)
        (forward-sexp))))
  item-alist)

(defun sv-mode-imenu-create-index-function-complex ()
  "Create complex sv-mode Imenu index."
  (goto-char (point-min))
  (let (item-alist)
    (sv-mode-imenu-parse item-alist nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speedbar

(eval-when-compile (require 'speedbar))

(defvar sv-speedbar-key-map nil
  "sv-mode speedbar keymap.")

(defvar sv-speedbar-cached-data nil
  "Parsed sv-mode data for the current file.")

(defun sv-mode-install-speedbar-variables ()
  "Install speedbar variables."
  (setq sv-speedbar-key-map (speedbar-make-specialized-keymap))
  (define-key sv-speedbar-key-map (kbd "RET") 'speedbar-edit-line)
  (define-key sv-speedbar-key-map (kbd "SPC") 'sv-speedbar-expand-line))

(if (featurep 'speedbar)
    (sv-mode-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'sv-mode-install-speedbar-variables))

(defun sv-speedbar-buttons (buffer)
  "Create a speedbar display to browse a SystemVerilog file
BUFFER is the buffer speedbar is requesting buttons for."
  (let (sb-filename parsed-data)
    (save-excursion
      (goto-char (point-min))
      (setq sb-filename
            (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (with-current-buffer buffer
      (unless (and (string= sb-filename (buffer-file-name)) sv-speedbar-cached-data)
        (unless sv-speedbar-cached-data
          (save-restriction
            (widen)
            (save-excursion
              (setq sv-speedbar-cached-data (sv-mode-imenu-create-index-function-complex)))))
        (setq parsed-data sv-speedbar-cached-data)))
    (when parsed-data
      (erase-buffer)
      (goto-char (point-min))
      (insert (buffer-file-name buffer) "\n\n")
      (sv-speedbar-populate parsed-data 0))))

(defun sv-speedbar-after-save-hook ()
  "Let the speedbar refresh after saving file."
  (when (equal major-mode 'sv-mode)
    (setq sv-speedbar-cached-data nil)))

(add-hook 'after-save-hook 'sv-speedbar-after-save-hook)

(defun sv-speedbar-populate (imenu-lst level)
  "Populate speedbar from imenu list."
  (dolist (item imenu-lst)
    (if (not (imenu--subalist-p item))
        (sv-speedbar-make-static-tag item level)
      (sv-speedbar-make-container-tag item level))))

(defun sv-speedbar-make-static-tag (item level)
  "Make a speedbar static tag line."
  (let ((name (car item)) type face)
    (if (not (string-match "\\(.+\\) : \\(.+\\)" name))
        (speedbar-make-tag-line 'statictag
                                nil nil
                                nil
                                name 'sv-speedbar-go (cdr item)
                                'speedbar-tag-face level)
      (setq type (match-string 2 name))
      (cond ((string-match "task\\|function\\|program" type)
             (setq face (cond ((string-match "\\[.*l.*\\]" type)
                               'font-lock-comment-face)
                              ((string-match "\\[.*p.*\\]" type)
                               'font-lock-string-face)
                              ((string-match "\\[.*s.*\\]" type)
                               'font-lock-keyword-face)
                              (t
                               'font-lock-function-name-face))))
            ((member type (list "struct" "enum"))
             (setq face 'font-lock-type-face))
            ((string= type "`include")
             (setq face 'font-lock-preprocessor-face))
            ((string= type "`define")
             (setq face 'font-lock-constant-face))
            (t
             (setq face (cond ((string-match "\\[.*l.*\\]" type)
                               'font-lock-comment-face)
                              ((string-match "\\[.*p.*\\]" type)
                               'font-lock-string-face)
                              ((string-match "\\[.*s.*\\]" type)
                               'font-lock-keyword-face)
                              (t
                               'font-lock-variable-name-face)))))
      (speedbar-make-tag-line 'statictag
                              nil nil
                              nil
                              name 'sv-speedbar-go (cdr item)
                              face level))))

(defun sv-speedbar-make-container-tag (item level)
  "Make a speedbar container tag line."
  (let* ((type-info (cadr item)) ;; First cons cell in cdr has type/location info
         (name (concat (car item) " : " (car type-info)))
         (location (cdr type-info))
         (face (if (equal (substring (car type-info) 0 1) "`")
                   'font-lock-preprocessor-face
                 'font-lock-type-face)))
    (speedbar-make-tag-line 'curly
                            ?- 'sv-speedbar-expand-line
                            (cddr item)
                            name 'sv-speedbar-go location
                            face level)
    (sv-speedbar-populate (cddr item) (1+ level))))

(defun sv-speedbar-expand-line (text token indent)
  "Expand/contract the item under the cursor."
  (interactive)
  (if (string= "{+}" text)
      ;; Expand
      (progn
        (speedbar-change-expand-button-char ?-)
        (forward-line)
        (speedbar-with-writable
          (save-excursion
            (sv-speedbar-populate token (1+ indent)))))
    ;; Contract
    (speedbar-change-expand-button-char ?+)
    (speedbar-with-writable
      (save-excursion
        (forward-line)
        (while (and (not (eobp))
                    (looking-at "\\([0-9]+\\):")
                    (> (string-to-number (match-string-no-properties 1)) indent))
          (delete-region (point-at-bol) (1+ (point-at-eol))))))))

(defun sv-speedbar-go (text node indent)
  "Goto the current tag."
  (interactive)
  (speedbar-select-attached-frame)
  (raise-frame)
  (select-frame-set-input-focus (selected-frame))
  (goto-char node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Align

(require 'align)

(defvar sv-mode-align-rules-list
  `(
    (sv-mode-class-item
     (regexp . "\\(\\s-+\\)\\([a-zA-Z0-9_:]+\\)\\s-*\\([(;]\\|\\[.+\\]\\)"))

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

    (sv-mode-doxygen
     (regexp . "@param\\s-+[a-zA-Z0-9_]+\\(\\s-+\\)."))

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
    (define-key map (kbd "n") 'sv-mode-electric-begin)
    (define-key map (kbd "d") 'sv-mode-electric-end)
    (define-key map (kbd "{") 'sv-mode-electric-curly-open)
    (define-key map (kbd "}") 'sv-mode-electric-curly-close)
    (define-key map (kbd "C-M-j") 'sv-mode-indent-new-comment-line)
    (define-key map (kbd "C-c C-j") 'sv-mode-jump-other-end)
    (define-key map (kbd "C-c C-u") 'sv-mode-beginning-of-scope)
    (define-key map (kbd "C-c C-d") 'sv-mode-end-of-scope)
    (define-key map (kbd "C-c C-a") 'sv-mode-beginning-of-statement)
    (define-key map (kbd "C-c C-p") 'sv-mode-beginning-of-block)
    (define-key map (kbd "C-c C-n") 'sv-mode-end-of-block)
    (define-key map (kbd "C-c C-s") 'sv-mode-create-skeleton-from-prototype)
    (define-key map (kbd "C-c C-o") 'ff-get-other-file)
    (define-key map (kbd "C-c C-S-o") 'sv-mode-goto-function-other-file)
    (define-key map (kbd "C-x n s") 'sv-mode-narrow-to-scope)
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
  (make-local-variable 'sv-mode-line-up-brace)

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

  (make-local-variable 'sv-speedbar-cached-data)

  (set (make-local-variable 'beginning-of-defun-function) 'sv-mode-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'sv-mode-end-of-defun)

  ;; Font-lock

  (setq font-lock-defaults
        '((sv-mode-font-lock-keywords
           sv-mode-font-lock-keywords-1
           sv-mode-font-lock-keywords-2
           sv-mode-font-lock-keywords-3)
          nil ;; nil means highlight strings & comments as well as keywords
          nil ;; nil means keywords must match case
          nil ;; use minimal syntax table for font lock
          nil ;; Function to move to beginning of reasonable region to highlight
          ))
  (turn-on-font-lock)

  ;; Only calculate these when the mode starts

  (setq sv-mode-begin-regexp
        (concat "\\_<\\("
                (regexp-opt
                 (append (list "begin" "case" "class" "clocking" "config" "fork"
                               "function" "generate" "covergroup" "interface" "module"
                               "package" "primitive" "program" "property" "randsequence"
                               "specify" "sequence" "table" "task"
                               ;; AOP
                               "extends")
                         sv-mode-macros-begin))
                "\\_>\\)"))

  (setq sv-mode-end-regexp
        (concat "\\_<\\("
                (regexp-opt
                 (append (list "end" "endcase" "endclass" "endclocking" "endconfig"
                               "join" "join_any" "join_none" "endfunction"
                               "endgenerate" "endgroup" "endinterface" "endmodule"
                               "endpackage" "endprimitive" "endprogram" "endproperty"
                               "endspecify" "endsequence" "endtable" "endtask"
                               ;; AOP
                               "endextends")
                         sv-mode-macros-end))
                "\\_>\\)"))

  (setq sv-mode-begin-to-end-alist
        (append (list '("begin" . "end")
                      '("case" . "endcase")
                      '("class" . "endclass")
                      '("clocking" . "endclocking")
                      '("config" . "endconfig")
                      '("function" . "endfunction")
                      '("generate" . "endgenerate")
                      '("covergroup" . "endgroup")
                      '("interface" . "endinterface")
                      '("module" . "endmodule")
                      '("package" . "endpackage")
                      '("primitive" . "endprimitive")
                      '("program" . "endprogram")
                      '("property" . "endproperty")
                      '("randsequence" . "endsequence")
                      '("specify" . "endspecify")
                      '("sequence" . "endsequence")
                      '("table" . "endtable")
                      '("task" . "endtask")
                      '("fork" . "join\\|join_any\\|join_none")
                      ;; AOP
                      '("extends" . "endextends"))
                sv-mode-macros-begin-to-end-alist))

  (setq sv-mode-end-to-begin-alist
        (append (list '("end" . "begin")
                      '("endcase" . "case")
                      '("endclass" . "class")
                      '("endclocking" . "clocking")
                      '("endconfig" . "config")
                      '("endfunction" . "function")
                      '("endgenerate" . "generate")
                      '("endgroup" . "covergroup")
                      '("endinterface" . "interface")
                      '("endmodule" . "module")
                      '("endpackage" . "package")
                      '("endprimitive" . "primitive")
                      '("endprogram" . "program")
                      '("endproperty" . "property")
                      '("endspecify" . "specify")
                      '("endsequence" . "randsequence")
                      '("endsequence" . "sequence")
                      '("endtable" . "table")
                      '("endtask" . "task")
                      '("join" . "fork")
                      '("join_any" . "fork")
                      '("join_none" . "fork")
                      ;; AOP
                      '("endextends" . "extends"))
                sv-mode-macros-end-to-begin-alist))

  (setq sv-mode-begin-end-regexp (concat sv-mode-begin-regexp "\\|" sv-mode-end-regexp))

  (setq sv-mode-bos-regexp (concat "[;})]\\|\\_<\\(begin\\|fork\\|do\\|case\\)\\_>\\|"
                                   (regexp-opt '("`define" "`else" "`elsif"
                                                 "`endif" "`ifdef" "`ifndef"
                                                 "`include" "`timescale" "`undef")) ".*\\|"
                                   sv-mode-end-regexp "\\|"
                                   (mapconcat 'identity sv-mode-macros-without-semi "\\|")))

  (setq sv-mode-eos-regexp (concat "end\\|join\\|" (mapconcat 'identity sv-mode-macros-end "\\|")))

  (setq sv-mode-macros-without-semi-regexp (mapconcat 'identity sv-mode-macros-without-semi "\\|"))

  ;; Idle timer

  (when sv-mode-idle-timer
    (cancel-timer sv-mode-idle-timer))
  (setq sv-mode-idle-timer
        (run-with-idle-timer sv-mode-highlight-match-delay t 'sv-mode-idle-timer-fcn))

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

  (set (make-local-variable 'doxymacs-Qt-blank-singleline-comment-template)
       sv-mode-doxymacs-blank-singleline-comment-template)

  (set (make-local-variable 'doxymacs-Qt-blank-multiline-comment-template)
       sv-mode-doxymacs-blank-multiline-comment-template)

  (set (make-local-variable 'doxymacs-Qt-function-comment-template)
       sv-mode-doxymacs-function-comment-template)

  ;; Hooks

  (run-hooks 'sv-mode-hook))

(provide 'sv-mode)
;;; sv-mode.el ends here
