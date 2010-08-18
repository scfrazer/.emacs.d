;;; sv-mode.el -- Mode for editing SystemVerilog files

;; Copyright (C) 2010  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 10 Aug 2010
;; Version: 0.1
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
;; language
;;
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;; (autoload 'sv-mode "sv-mode" "SystemVerilog code editing mode" t)
;; (setq auto-mode-alist
;;        (append (list
;;                 (cons "\\.v\\'" 'sv-mode)
;;                 (cons "\\.sv\\'" 'sv-mode)
;;                 (cons "\\.svh\\'" 'sv-mode))
;;                auto-mode-alist))
;;
;;; Change log:
;;
;; 01 Jan 2007 -- v0.1
;;                Initial creation

(defconst sv-mode-version "0.1"
  "Version of sv-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(defgroup sv-mode nil
  "SystemVerilog mode."
  :group 'languages)

(defcustom sv-mode-basic-offset 4
  "*Indentation of statements with respect to containing block."
  :group 'sv-mode
  :type 'integer)

(defcustom sv-mode-continued-line-offset 4
  "*Extra indentation of continued statements."
  :group 'sv-mode
  :type 'integer)

(defcustom sv-mode-line-up-bracket t
  "*Non-nil means indent items in brackets relative to the '['.
Otherwise indent them as usual."
  :group 'sv-mode
  :type 'boolean)

(defcustom sv-mode-line-up-paren t
  "*Non-nil means indent items in parentheses relative to the '('.
Otherwise indent them as usual."
  :group 'sv-mode
  :type 'boolean)

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
             "coverage_control" "coverage_get" "coverage_get_max"
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
           (cons "^\\s-*\\(\\(extern\\|local\\|protected\\|virtual\\)\\s-+\\)*\\(task\\|function\\|program\\)\\s-+.*?\\([a-zA-Z0-9_]+\\)\\s-*[(;]"
                 '(4 font-lock-function-name-face t))
           ;; Labels
           (cons (concat "\\_<" (regexp-opt
                                 '("endcase" "endclass" "endclocking" "endconfig"
                                   "endfunction" "endgenerate" "endgroup"
                                   "endinterface" "endmodule" "endpackage"
                                   "endprimitive" "endprogram" "endproperty"
                                   "endspecify" "endsequence" "endtable" "endtask"))
                         "\\s-*:\\s-*\\([a-zA-Z0-9_]+\\)")
                 '(1 font-lock-constant-face t))))
  "Medium level highlighting for sv-mode.")

(defvar sv-mode-font-lock-keywords-3
  (append sv-mode-font-lock-keywords-2
          (list
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
           (cons "\\_<extends\\s-+\\([a-zA-Z0-9_]+\\)"
                 '(1 font-lock-type-face))))
  "Gaudy level highlighting for sv-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions

(defsubst sv-mode-in-comment-or-string ()
  "Return 'comment if point is inside a comment, 'string if point is inside a
string, or nil if neither."
  (let ((pps (syntax-ppss)))
    (catch 'return
      (when (nth 4 pps)
        (throw 'return 'comment))
      (when (nth 3 pps)
        (throw 'return 'string)))))

(defsubst sv-mode-re-search-forward (REGEXP &optional BOUND NOERROR)
  "Like re-search-forward, but skips over comments and strings.
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
  "Like as re-search-backward, but skips over comments and strings.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defun sv-mode-indent-line ()
  "Indent the current line."
  (interactive)
  (let ((pos (- (point-max) (point)))
        (indent (sv-mode-get-indent)))
    (unless (= indent (current-indentation))
      (back-to-indentation)
      (delete-region (point-at-bol) (point))
      (indent-to indent))
    (if (< (- (point) (point-at-bol)) indent)
        (back-to-indentation)
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos))))))

(defun sv-mode-get-indent ()
  "Return how much the current line should be indented."
  (save-excursion
    (beginning-of-line)
    (cond
     ((sv-mode-in-comment-or-string) 0)
     ((sv-mode-get-indent-if-in-paren))
     ((sv-mode-get-indent-if-opener))
     ((sv-mode-get-indent-if-closer))
     ((sv-mode-get-normal-indent)))))

(defun sv-mode-get-indent-if-in-paren ()
  "Get amount to indent if in parentheses or brackets."
  (save-excursion
    (condition-case nil
        (let ((at-closer (looking-at "[ \t]*[])]"))
              offset)
          (backward-up-list)
          (unless (or (= (char-after) ?{)
                      (and (= (char-after) ?\[) (not sv-mode-line-up-bracket))
                      (and (= (char-after) ?\() (not sv-mode-line-up-paren)))
            (setq offset (current-column))
            (unless at-closer
              (forward-char)
              (if (and (> (skip-syntax-forward " >" (point-at-eol)) 0)
                       (not (looking-at "//")))
                  (setq offset (current-column))
                (setq offset (1+ offset))))
            offset))
      (error nil))))

(defun sv-mode-get-indent-if-opener () 0)
(defun sv-mode-get-indent-if-closer () 0)
(defun sv-mode-get-normal-indent () 0)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode map

(defvar sv-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in sv-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SystemVerilog mode

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
  (setq comment-start "// "
        comment-end ""
        comment-multi-line t
        comment-start-skip "//[ \t]*"
        parse-sexp-ignore-comments t
        indent-line-function 'sv-mode-indent-line)

  ;; Font-lock

  (setq font-lock-defaults
        '((sv-mode-font-lock-keywords
           sv-mode-font-lock-keywords-1
           sv-mode-font-lock-keywords-2
           sv-mode-font-lock-keywords-3)
          nil ;; nil means highlight strings & comments as well as keywords
          nil ;; nil means keywords must match case
          nil ;; use minimal syntax table for font lock
          nil ;; TODO function to move to beginning of reasonable region to highlight
          ))
  (turn-on-font-lock)

  ;; Other-file

  (setq ff-other-file-alist
        '(("\\.sv$" (".svh"))
          ("\\.svh$" (".sv"))))

  ;; Hooks

  (run-hooks 'sv-mode-hook))

(provide 'sv-mode)
