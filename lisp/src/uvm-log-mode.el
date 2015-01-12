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
  '((t (:foreground "#875F87")))
  "Font Lock mode face used to highlight timestamps."
  :group 'uvm-log-mode)

(defface uvm-log-mode-msg-id-face
  '((t (:foreground "#5F5FD7")))
  "Font Lock mode face used to highlight messages IDs."
  :group 'uvm-log-mode)

(defface uvm-log-mode-path-face
  '((t (:foreground "#5F8787")))
  "Font Lock mode face used to highlight the component path."
  :group 'uvm-log-mode)

(defface uvm-log-mode-msg-face
  '((t (:foreground "#000087")))
  "Font Lock mode face used to highlight messages."
  :group 'uvm-log-mode)

(defface uvm-log-mode-phase-face
  '((t (:foreground "#875F5F")))
  "Font Lock mode face used to highlight phase change messages."
  :group 'uvm-log-mode)

(defface uvm-log-mode-error-face
  '((t (:foreground "#FF0000")))
  "Font Lock mode face used to highlight errors."
  :group 'uvm-log-mode)

(defface uvm-log-mode-warning-face
  '((t (:foreground "#878700")))
  "Font Lock mode face used to highlight warnings."
  :group 'uvm-log-mode)

(defface uvm-log-mode-debug-face
  '((t (:foreground "#000000" :background "#FF8700")))
  "Font Lock mode face used to highlight debug markers."
  :group 'uvm-log-mode)

(defface uvm-log-mode-debug-msg-face
  '((t (:foreground "#D75F00")))
  "Font Lock mode face used to highlight debug messages."
  :group 'uvm-log-mode)

(defface uvm-log-mode-sb-add-face
  '((t (:foreground "#D700FF")))
  "Font Lock mode face used to highlight send statements."
  :group 'sse-log-mode)

(defface uvm-log-mode-sb-match-face
  '((t (:foreground "#00D700")))
  "Font Lock mode face used to highlight match statements."
  :group 'uvm-log-mode)

(defface uvm-log-mode-highlight-phase-face
  '((t (:background "#AFD7D7")))
  "Font Lock mode face used to highlight tags."
  :group 'uvm-log-mode)

(defvar uvm-log-mode-font-lock-keywords
  '(
    ("\\*\\{80\\}"
     (0 'uvm-log-mode-phase-face))
    ("\\(\\*\\* \\(Starting\\|Ending\\) phase:\\)\\s-+\\([a-zA-Z0-9_]+\\)"
     (1 'uvm-log-mode-phase-face)
     (3 'font-lock-function-name-face))
    ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\(DEBUG-[a-zA-Z0-9_:-]+\\) \\([^ ]+\\) \\(.*\\)"
     (1 'uvm-log-mode-timestamp-face)
     (2 'uvm-log-mode-debug-face)
     (3 'uvm-log-mode-path-face)
     (4 'uvm-log-mode-debug-msg-face))
    ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\([a-zA-Z0-9_:-]+\\) \\([^ ]+\\) \\(stream\\[[^]]+\\]:\\) \\(Added upstream item #[0-9]+\\|Early downstream transaction added:\\) \\(.*\\)"
     (1 'uvm-log-mode-timestamp-face)
     (2 'uvm-log-mode-msg-id-face)
     (3 'uvm-log-mode-path-face)
     (4 'uvm-log-mode-msg-face)
     (5 'uvm-log-mode-sb-add-face)
     (6 'uvm-log-mode-msg-face))
    ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\([a-zA-Z0-9_:-]+\\) \\([^ ]+\\) \\(stream\\[[^]]+\\]:\\) \\(Matched upstream item #[0-9]+\\) \\(.*\\)"
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
    ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\(MISCMP\\) \\([^ ]+\\) \\(.*\\)"
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
  (let ((phase-or-error-regexp "\\(Starting phase\\)") ov pos)
    (if (and arg (< arg 0))
        (unless (re-search-backward phase-or-error-regexp nil t)
          (error "No previous phase change"))
      (setq pos (point))
      (end-of-line)
      (unless (re-search-forward phase-or-error-regexp nil t)
        (goto-char pos)
        (error "No more phase changes")))
    (beginning-of-line)
    (recenter)
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
