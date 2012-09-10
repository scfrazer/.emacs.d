;;; uvm-log-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(defgroup uvm-log-mode nil
  "*UVM-LOG log mode."
  :group 'compilation)

(defcustom uvm-log-mode-hook nil
  "*List of functions to call on entry to uvm-log-mode mode."
  :group 'uvm-log-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

(defface uvm-log-mode-timestamp-face
  '((((class color) (background dark)) (:foreground "plum2"))
    (((class color) (background light)) (:foreground "purple3")))
  "Font Lock mode face used to highlight timestamps."
  :group 'uvm-log-mode)

(defface uvm-log-mode-msg-id-face
  '((((class color) (background dark)) (:foreground "cyan3"))
    (((class color) (background light)) (:foreground "cyan3")))
  "Font Lock mode face used to highlight messages IDs."
  :group 'uvm-log-mode)

(defface uvm-log-mode-path-face
  '((((class color) (background dark)) (:foreground "SkyBlue1"))
    (((class color) (background light)) (:foreground "SteelBlue4")))
  "Font Lock mode face used to highlight the component path."
  :group 'uvm-log-mode)

(defface uvm-log-mode-msg-face
  '((((class color) (background dark)) (:foreground "PaleTurquoise2"))
    (((class color) (background light)) (:foreground "tan4")))
  "Font Lock mode face used to highlight messages."
  :group 'uvm-log-mode)

(defface uvm-log-mode-phase-face
  '((((class color) (background dark)) (:foreground "hotpink2"))
    (((class color) (background light)) (:bold t)))
  "Font Lock mode face used to highlight phase change messages."
  :group 'uvm-log-mode)

(defface uvm-log-mode-error-face
  '((((class color) (background dark)) (:foreground "red"))
    (((class color) (background light)) (:foreground "red")))
  "Font Lock mode face used to highlight errors."
  :group 'uvm-log-mode)

(defface uvm-log-mode-warning-face
  '((((class color) (background dark)) (:foreground "yellow2"))
    (((class color) (background light)) (:foreground "yellow4")))
  "Font Lock mode face used to highlight warnings."
  :group 'uvm-log-mode)

(defface uvm-log-mode-debug-face
  '((((class color) (background dark)) (:foreground "red" :background "yellow"))
    (((class color) (background light)) (:foreground "red" :background "yellow")))
  "Font Lock mode face used to highlight debug markers."
  :group 'uvm-log-mode)

(defface uvm-log-mode-debug-msg-face
  '((((class color) (background dark)) (:foreground "DarkOliveGreen3")))
  "Font Lock mode face used to highlight debug messages."
  :group 'uvm-log-mode)

(defface uvm-log-mode-sb-add-face
  '((((class color) (background dark)) (:foreground "gold3")))
  "Font Lock mode face used to highlight send statements."
  :group 'sse-log-mode)

(defface uvm-log-mode-sb-match-face
  '((((class color) (background dark)) (:foreground "chartreuse2"))
    (((class color) (background light)) (:foreground "green3")))
  "Font Lock mode face used to highlight match statements."
  :group 'uvm-log-mode)

;; (defface uvm-log-mode-pass-face
;;   '((((class color) (background dark)) (:foreground "PaleGreen2"))
;;     (((class color) (background light)) (:foreground "green3")))
;;   "Font Lock mode face used to highlight pass statements."
;;   :group 'uvm-log-mode)
;;
;; (defface uvm-log-mode-highlight-phase-face
;;   '((((class color) (background dark)) (:foreground "white" :background "slateblue3"))
;;     (((class color) (background light)) (:foreground "white" :background "slateblue3")))
;;   "Font Lock mode face used to highlight tags."
;;   :group 'uvm-log-mode)

(defvar uvm-log-mode-font-lock-keywords
  '(
;;     ("^\\([0-9.]+[fpnum]?s\\):\\s-+\\(DEBUG-[^ ]+\\)\\s-+\\([^ \n]+\\)\\(.*\\)"
;;      (1 'uvm-log-mode-timestamp-face)
;;      (2 'uvm-log-mode-debug-face)
;;      (3 'uvm-log-mode-debug-msg-face)
;;      (4 'uvm-log-mode-msg-face))
    ("\\*\\{80\\}"
     (0 'uvm-log-mode-phase-face))
    ("\\(\\*\\* Starting phase:\\)\\s-+\\([a-zA-Z0-9_]+\\)"
     (1 'uvm-log-mode-phase-face)
     (2 'font-lock-function-name-face))
;;     ("^\\([0-9.]+[fpnum]?s\\):\\s-+\\(FATAL\\|ERROR\\):\\s-+\\([^ ]+\\)\\s-+\\(.*\\)"
;;      (1 'uvm-log-mode-timestamp-face)
;;      (2 'uvm-log-mode-error-face)
;;      (3 'uvm-log-mode-path-face)
;;      (4 'uvm-log-mode-msg-face))
;;     ("^\\([0-9.]+[fpnum]?s\\):\\s-+\\(WARNING\\):\\s-+\\([^ ]+\\)\\s-+\\(.*\\)"
;;      (1 'uvm-log-mode-timestamp-face)
;;      (2 'uvm-log-mode-warning-face)
;;      (3 'uvm-log-mode-path-face)
;;      (4 'uvm-log-mode-msg-face))
    ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\([a-zA-Z0-9_:-]+\\) \\([^ ]+\\) \\(stream\[[0-9]+\]:\\) \\(Added upstream item #[0-9]+\\) \\(.*\\)"
     (1 'uvm-log-mode-timestamp-face)
     (2 'uvm-log-mode-msg-id-face)
     (3 'uvm-log-mode-path-face)
     (4 'uvm-log-mode-msg-face)
     (5 'uvm-log-mode-sb-add-face)
     (6 'uvm-log-mode-msg-face))
    ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\([a-zA-Z0-9_:-]+\\) \\([^ ]+\\) \\(stream\[[0-9]+\]:\\) \\(Matched upstream item #[0-9]+\\) \\(.*\\)"
     (1 'uvm-log-mode-timestamp-face)
     (2 'uvm-log-mode-msg-id-face)
     (3 'uvm-log-mode-path-face)
     (4 'uvm-log-mode-msg-face)
     (5 'uvm-log-mode-sb-match-face)
     (6 'uvm-log-mode-msg-face))
    ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) \\(\\*\\*[^]]+\\) \\([^ ]+\\) \\(.*\\)"
     (1 'uvm-log-mode-timestamp-face)
     (2 'font-lock-warning-face)
     (3 'uvm-log-mode-path-face)
     (4 'uvm-log-mode-msg-face))
    ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\([a-zA-Z0-9_:-]+\\) \\([^ ]+\\) \\(.*\\)"
     (1 'uvm-log-mode-timestamp-face)
     (2 'uvm-log-mode-msg-id-face)
     (3 'uvm-log-mode-path-face)
     (4 'uvm-log-mode-msg-face))
    )
  "Font locking for 'uvm-log-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun uvm-log-mode-next-phase (arg reset)
  "Goto next TESTFLOW phase (or error)."
  (interactive)
  (let ((phase-or-error-regexp "\\(Starting phase\\)")
        ov)
    (if (and arg (< arg 0))
        (re-search-backward phase-or-error-regexp)
      (end-of-line)
      (re-search-forward phase-or-error-regexp))
    (beginning-of-line)
    (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
      (overlay-put ov 'face 'uvm-log-mode-highlight-phase-face)
      (sit-for 1)
      (delete-overlay ov))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar uvm-log-mode-map nil "'uvm-log-mode' keymap.")
(if (not uvm-log-mode-map)
    (let ((map (make-keymap)))
      (setq uvm-log-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar uvm-log-mode-syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    (modify-syntax-entry ?_ "_" table)

    (modify-syntax-entry ?~ "." table)
    (modify-syntax-entry ?` "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?/ "." table)

    table)
  "Syntax table used in uvm-log-mode buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defun uvm-log-mode ()
  "uvm-log-mode is a major mode for browsing SSE run.log files.\n\n
\\{uvm-log-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'uvm-log-mode)
  (setq mode-name "uvm-log")

  (set-syntax-table uvm-log-mode-syntax-table)

  (use-local-map uvm-log-mode-map)

  (setq next-error-function 'uvm-log-mode-next-phase)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(uvm-log-mode-font-lock-keywords t))
  (turn-on-font-lock)

  (setq truncate-lines t)
  (setq imenu-generic-expression (list '(nil "Starting phase: \\(.+\\)" 1)))

  (run-mode-hooks 'uvm-log-mode-hook))

(provide 'uvm-log-mode)
