;;; sim-log-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(defgroup sim-log-mode nil
  "*SIM-LOG log mode."
  :group 'compilation)

(defcustom sim-log-mode-hook nil
  "*List of functions to call on entry to sim-log-mode mode."
  :group 'sim-log-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

(defface sim-log-mode-timestamp-face
  '((t :inherit font-lock-type-face))
  "Font Lock mode face used to highlight timestamps."
  :group 'sim-log-mode)

(defface sim-log-mode-msg-id-face
  '((t :inherit font-lock-string-face))
  "Font Lock mode face used to highlight messages IDs."
  :group 'sim-log-mode)

(defface sim-log-mode-path-face
  '((t :inherit font-lock-variable-name-face))
  "Font Lock mode face used to highlight the component path."
  :group 'sim-log-mode)

(defface sim-log-mode-msg-face
  '((t :inherit default))
  "Font Lock mode face used to highlight messages."
  :group 'sim-log-mode)

(defface sim-log-mode-phase-face
  '((t :inherit font-lock-keyword-face))
  "Font Lock mode face used to highlight phase change messages."
  :group 'sim-log-mode)

(defface sim-log-mode-debug-face
  '((t (:foreground "black" :background "#FF8700")))
  "Font Lock mode face used to highlight debug markers."
  :group 'sim-log-mode)

(defface sim-log-mode-debug-msg-face
  '((t :foreground "darkorange2"))
  "Font Lock mode face used to highlight debug messages."
  :group 'sim-log-mode)

(defface sim-log-mode-sb-add-face
  '((t :inherit link))
  "Font Lock mode face used to highlight send statements."
  :group 'sim-log-mode)

(defface sim-log-mode-sb-match-face
  '((t :inherit link-visited))
  "Font Lock mode face used to highlight match statements."
  :group 'sim-log-mode)

(defface sim-log-mode-highlight-phase-face
  '((t :inherit highlight))
  "Font Lock mode face used to highlight tags."
  :group 'sim-log-mode)

(defvar sim-log-mode-font-lock-keywords
  '(
    ;; ("\\*\\{80\\}"
    ;;  (0 'sim-log-mode-phase-face))
    ;; ("\\(\\*\\* \\(Starting\\|Ending\\) phase:\\)\\s-+\\([a-zA-Z0-9_]+\\)"
    ;;  (1 'sim-log-mode-phase-face)
    ;;  (3 'font-lock-function-name-face))
    ;; ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\(DEBUG-[a-zA-Z0-9_:-]+\\) \\([^ ]+\\) \\(.*\\)"
    ;;  (1 'sim-log-mode-timestamp-face)
    ;;  (2 'sim-log-mode-debug-face)
    ;;  (3 'sim-log-mode-path-face)
    ;;  (4 'sim-log-mode-debug-msg-face))
    ;; ("\\_<\\(WARNING\\|Warning\\|warning\\)\\_>"
    ;;  (0 'warning))
    ;; ("\\_<\\(ERROR\\|Error\\|error\\|FATAL\\|Fatal\\|fatal\\)\\_>"
    ;;  (0 'error))
    ;; ("=>"
    ;;  (0 'font-lock-preprocessor-face))
    ;; ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\([^ ]+\\) \\[\\(.+?\\)\\]: \\(stream\\[.+?\\]:\\) \\(Added upstream item #[0-9]+\\|Early downstream transaction added:\\) \\(.*\\)"
    ;;  (1 'sim-log-mode-timestamp-face)
    ;;  (2 'sim-log-mode-msg-id-face)
    ;;  (3 'sim-log-mode-path-face)
    ;;  (4 'sim-log-mode-msg-face)
    ;;  (5 'sim-log-mode-sb-add-face)
    ;;  (6 'sim-log-mode-msg-face))
    ;; ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\([^ ]+\\) \\[\\(.+?\\)\\]: \\(stream\\[.+?\\]:\\) \\(Matched upstream item #[0-9]+\\) \\(.*\\)"
    ;;  (1 'sim-log-mode-timestamp-face)
    ;;  (2 'sim-log-mode-msg-id-face)
    ;;  (3 'sim-log-mode-path-face)
    ;;  (4 'sim-log-mode-msg-face)
    ;;  (5 'sim-log-mode-sb-match-face)
    ;;  (6 'sim-log-mode-msg-face))
    ;; ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) \\(\\*\\*[^]]+\\) \\[\\(.+?\\)\\]: \\(.*\\)"
    ;;  (1 'sim-log-mode-timestamp-face)
    ;;  (2 'warning)
    ;;  (3 'sim-log-mode-path-face)
    ;;  (4 'sim-log-mode-msg-face))
    ;; ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\(MISCMP\\) \\[\\(.+?\\)\\]: \\(.*\\)"
    ;;  (1 'sim-log-mode-timestamp-face)
    ;;  (2 'warning)
    ;;  (3 'sim-log-mode-path-face)
    ;;  (4 'sim-log-mode-msg-face))
    ;; ("^\\s-*\\([0-9.]+\\s-*[fpnum]?s\\) . \\([^ ]+\\) \\[\\(.+?\\)\\]: \\(.*\\)"
    ;;  (1 'sim-log-mode-timestamp-face)
    ;;  (2 'sim-log-mode-msg-id-face)
    ;;  (3 'sim-log-mode-path-face)
    ;;  (4 'sim-log-mode-msg-face))
    ;; ("^UVM_INFO \\([^@]+?\\)@"
    ;;  (1 'font-lock-comment-face))
    ("^UVM_INFO \\([^@]+?\\)@"
     (1 (progn (add-text-properties (match-beginning 0)
                                     (match-end 0)
                                     '(invisible t)))))
    "Font locking for 'sim-log-mode'."))

(add-to-list 'font-lock-extra-managed-props 'invisible)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun sim-log-mode-next-phase (arg reset)
  "Goto next TESTFLOW phase (or error)."
  (interactive)
  (let ((phase-or-error-regexp "\\(Starting phase\\|[*] ERROR \\|[*] FATAL\\)") ov pos)
    (if (and arg (< arg 0))
        (unless (re-search-backward phase-or-error-regexp nil t)
          (error "No previous phase/error"))
      (setq pos (point))
      (end-of-line)
      (unless (re-search-forward phase-or-error-regexp nil t)
        (goto-char pos)
        (error "No more phases/errors")))
    (beginning-of-line)
    (recenter)
    (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
      (overlay-put ov 'face 'sim-log-mode-highlight-phase-face)
      (sit-for 1)
      (delete-overlay ov))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar sim-log-mode-map nil "'sim-log-mode' keymap.")
(if (not sim-log-mode-map)
    (let ((map (make-keymap)))
      (setq sim-log-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar sim-log-mode-syntax-table
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
  "Syntax table used in sim-log-mode buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defun sim-log-mode ()
  "sim-log-mode is a major mode for browsing UVM run.log files.\n\n
\\{sim-log-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'sim-log-mode)
  (setq mode-name "sim-log")

  (set-syntax-table sim-log-mode-syntax-table)

  (use-local-map sim-log-mode-map)

  (setq next-error-function 'sim-log-mode-next-phase)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(sim-log-mode-font-lock-keywords t))
  (turn-on-font-lock)

  (setq truncate-lines t)
  (setq imenu-generic-expression (list '(nil "Starting phase: \\(.+?\\)," 1)))

  (run-mode-hooks 'sim-log-mode-hook))

(provide 'sim-log-mode)
