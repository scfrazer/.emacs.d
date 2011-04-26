;;; sse-log-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(defgroup sse-log-mode nil
  "*SSE-LOG log mode."
  :group 'compilation)

(defcustom sse-log-mode-hook nil
  "*List of functions to call on entry to sse-log-mode mode."
  :group 'sse-log-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

(defface sse-log-mode-error-face
  '((((class color) (background dark)) (:foreground "red" :bold t))
    (((class color) (background light)) (:foreground "red" :bold t)))
  "Font Lock mode face used to highlight errors."
  :group 'sse-log-mode)

(defface sse-log-mode-warning-face
  '((((class color) (background dark)) (:foreground "yellow2" :bold t))
    (((class color) (background light)) (:foreground "yellow4" :bold t)))
  "Font Lock mode face used to highlight warnings."
  :group 'sse-log-mode)

(defface sse-log-mode-debug-face
  '((((class color) (background dark)) (:foreground "red" :background "yellow" :box t :bold t))
    (((class color) (background light)) (:foreground "red" :background "yellow" :box t :bold t)))
  "Font Lock mode face used to highlight debug markers."
  :group 'sse-log-mode)

(defface sse-log-mode-debug-msg-face
  '((((class color) (background dark)) (:foreground "DarkOliveGreen3"))
    (((class color) (background light)) (:bold t)))
  "Font Lock mode face used to highlight debug messages."
  :group 'sse-log-mode)

(defface sse-log-mode-timestamp-face
  '((((class color) (background dark)) (:foreground "plum2"))
    (((class color) (background light)) (:foreground "purple3")))
  "Font Lock mode face used to highlight timestamps."
  :group 'sse-log-mode)

(defface sse-log-mode-path-face
  '((((class color) (background dark)) (:foreground "SkyBlue1"))
    (((class color) (background light)) (:foreground "SteelBlue4")))
  "Font Lock mode face used to highlight tags."
  :group 'sse-log-mode)

(defface sse-log-mode-domain-face
  '((((class color) (background dark)) (:foreground "LightGoldenrod2" :bold t))
    (((class color) (background light)) (:bold t)))
  "Font Lock mode face used to highlight domain advance statements."
  :group 'sse-log-mode)

(defface sse-log-mode-send-face
  '((((class color) (background dark)) (:foreground "white" :background "green3" :bold t))
    (((class color) (background light)) (:foreground "white" :background "green3" :bold t)))
  "Font Lock mode face used to highlight send statements."
  :group 'sse-log-mode)

(defface sse-log-mode-match-face
  '((((class color) (background dark)) (:foreground "PaleGreen2" :bold t))
    (((class color) (background light)) (:foreground "green3" :bold t)))
  "Font Lock mode face used to highlight match statements."
  :group 'sse-log-mode)

(defface sse-log-mode-pass-face
  '((((class color) (background dark)) (:foreground "PaleGreen2" :bold t))
    (((class color) (background light)) (:foreground "green3" :bold t)))
  "Font Lock mode face used to highlight pass statements."
  :group 'sse-log-mode)

(defface sse-log-mode-msg-level-face
  '((((class color) (background dark)) (:foreground "cyan3"))
    (((class color) (background light)) (:foreground "cyan3")))
  "Font Lock mode face used to highlight messages."
  :group 'sse-log-mode)

(defface sse-log-mode-msg-face
  '((((class color) (background dark)) (:foreground "PaleTurquoise2"))
    (((class color) (background light)) (:foreground "tan4")))
  "Font Lock mode face used to highlight messages."
  :group 'sse-log-mode)

(defface sse-log-mode-highlight-phase-face
  '((((class color) (background dark)) (:foreground "white" :background "slateblue3" :bold t))
    (((class color) (background light)) (:foreground "white" :background "slateblue3" :bold t)))
  "Font Lock mode face used to highlight tags."
  :group 'sse-log-mode)

(defvar sse-log-mode-font-lock-keywords
  '(
;     ("^\\s +\\*\\*\\* Dut error.*$"
;      (0 'sse-log-mode-error-face))
;     ("^\\s +\\*\\*\\* Warning.*$"
;      (0 'sse-log-mode-warning-face))
    ("^\\([0-9.]+[fpnum]?s\\):\\s-+\\(DEBUG-[^ ]+\\)\\s-+\\([^ ]+\\)\\(.*\\)"
     (1 'sse-log-mode-timestamp-face)
     (2 'sse-log-mode-debug-face)
     (3 'sse-log-mode-debug-msg-face)
     (4 'sse-log-mode-msg-face))
    ("\\*\\{80\\}"
     (0 'sse-log-mode-domain-face))
    ("\\(\\*\\* STARTING PHASE:\\)\\(.+\\)"
     (1 'sse-log-mode-domain-face)
     (2 'sse-log-mode-msg-face))
    ("^\\([0-9.]+[fpnum]?s\\):\\s-+\\(FATAL\\|ERROR\\):\\s-+\\([^ ]+\\)\\s-+\\(.*\\)"
     (1 'sse-log-mode-timestamp-face)
     (2 'sse-log-mode-error-face)
     (3 'sse-log-mode-path-face)
     (4 'sse-log-mode-msg-face))
    ("^\\([0-9.]+[fpnum]?s\\):\\s-+\\(WARNING\\):\\s-+\\([^ ]+\\)\\s-+\\(.*\\)"
     (1 'sse-log-mode-timestamp-face)
     (2 'sse-log-mode-warning-face)
     (3 'sse-log-mode-path-face)
     (4 'sse-log-mode-msg-face))
    ("^\\([0-9.]+[fpnum]?s\\):\\s-+\\([A-Z]\\):\\s-+\\([^ ]+\\)\\s-+\\(.*\\)"
     (1 'sse-log-mode-timestamp-face)
     (2 'sse-log-mode-msg-level-face)
     (3 'sse-log-mode-path-face)
     (4 'sse-log-mode-msg-face))
;     ("Sending.*"
;      (0 'sse-log-mode-send-face))
;     ("Matched.*$"
;      (0 'sse-log-mode-match-face))
;     ("!+ Test has Failed! !+"
;      (0 'sse-log-mode-error-face t))
;     ("\\*+ Test has Passed! \\*+"
;      (0 'sse-log-mode-pass-face t))
;     ("^\\s *\\([0-9.]+ [fpnum]s\\)\\s *\\[\\(.+?\\)\\] (\\(.+?\\)): \\(.*\\)$"
;      (1 'sse-log-mode-timestamp-face)
;      (2 'sse-log-mode-msg-level-face)
;      (3 'sse-log-mode-path-face)
;      (4 'sse-log-mode-msg-face))
;     ("(Err:.+?)"
;      (0 'sse-log-mode-error-face t))
    )
  "Font locking for 'sse-log-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun sse-log-mode-next-phase (arg reset)
  "Goto next TESTFLOW phase (or error)."
  (interactive)
  (let ((phase-or-error-regexp "\\(STARTING PHASE\\)")
        ov)
    (if (and arg (< arg 0))
        (re-search-backward phase-or-error-regexp)
      (end-of-line)
      (re-search-forward phase-or-error-regexp))
    (beginning-of-line)
    (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
      (overlay-put ov 'face 'sse-log-mode-highlight-phase-face)
      (sit-for 1)
      (delete-overlay ov))))

;; Show/hide phases

(defvar sse-log-mode-testflow-regexp "testflow advancing to:")
(defvar sse-log-mode-testflow-sep-regexp "(GLOBAL_DOMAIN): \*\*\*\*\*\*\*\*\*\*")
(defvar sse-log-mode-config-regexp "printing \\(regenerated \\)?config tree")
(defvar sse-log-mode-config-sep-regexp "(ASTRO): ==============================================")

(defun sse-log-mode-toggle-phase ()
  "Toggle hide/show current phase."
  (interactive)
  (beginning-of-line)
  (while (and (not (eobp))
              (looking-at (concat "^.+?\\(" sse-log-mode-testflow-regexp "\\|"
                                  sse-log-mode-testflow-sep-regexp "\\|"
                                  sse-log-mode-config-regexp "\\|"
                                  sse-log-mode-config-sep-regexp "\\)")))
    (forward-line 1))
  (unless (sse-log-mode-show-phase)
    (sse-log-mode-hide-phase)))

(defun sse-log-mode-show-phase ()
  "Show current phase."
  (let ((ovls (overlays-at (point))))
    (catch 'break
      (dolist (ovl ovls)
        (when (plist-get (overlay-properties ovl) 'sse-log-mode-hidden-phase)
          (sse-log-mode-remove-overlay ovl)
          (throw 'break t)))
      nil)))

(defun sse-log-mode-remove-overlay (ovl)
  "Remove overlay."
  (delete-overlay ovl))

(defun sse-log-mode-hide-phase ()
  "Hide current phase."
  (let ((marker-regexp (concat "^.+?\\(" sse-log-mode-testflow-regexp "\\|" sse-log-mode-config-sep-regexp "\\)"))
        start end)
    (save-excursion
      (when (re-search-backward marker-regexp nil 'go)
        (if (string= (match-string-no-properties 1) sse-log-mode-testflow-regexp)
            (progn
              (beginning-of-line)
              (forward-line 2))
          (beginning-of-line)
          (forward-line 1)
          (when (looking-at (concat "^.+?" sse-log-mode-config-regexp))
            (forward-line 1))))
      (setq start (point)))
    (save-excursion
      (re-search-forward marker-regexp nil 'go)
      (beginning-of-line)
      (when (string= (match-string-no-properties 1) sse-log-mode-testflow-regexp)
        (forward-line -1))
      (setq end (point)))
    (let ((phase-overlay (make-overlay start end)))
      (overlay-put phase-overlay 'before-string "\n")
      (overlay-put phase-overlay 'sse-log-mode-hidden-phase t)
      (overlay-put phase-overlay 'isearch-open-invisible 'sse-log-mode-remove-overlay)
      (overlay-put phase-overlay 'invisible t))))

(defun sse-log-mode-toggle-all ()
  "Toggle showing all phases."
  (interactive)
  (unless (sse-log-mode-show-all)
    (sse-log-mode-hide-all)))

(defun sse-log-mode-show-all ()
  "Show all phases."
  (let ((ovls (sse-log-mode-get-all-overlays)))
    (when ovls
      (dolist (ovl ovls)
        (sse-log-mode-remove-overlay ovl))
      t)))

(defun sse-log-mode-hide-all ()
  "Hide all phases."
  (sse-log-mode-hide-up-to-phase "NOENDPHASE"))

(defun sse-log-mode-toggle-main-test ()
  "Toggle showing all before main-test phase."
  (interactive)
  (unless (sse-log-mode-show-all)
    (sse-log-mode-hide-up-to-main-test)))

(defun sse-log-mode-hide-up-to-main-test ()
  "Hide phases up to main-test."
  (sse-log-mode-hide-up-to-phase "GLOBAL/CSCO_MAIN_TEST"))

(defun sse-log-mode-hide-up-to-phase (phase)
  "Hide up to a phase."
  (goto-char (point-min))
  (sse-log-mode-hide-phase)
  (catch 'done
    (while (re-search-forward
            (concat "^.+?\\(" sse-log-mode-testflow-regexp "\\|" sse-log-mode-config-sep-regexp "\\)") nil 'go)
      (when (looking-at (concat ".+?" phase))
        (throw 'done t))
      (sse-log-mode-hide-phase)))
  (goto-char (point-min)))

(defun sse-log-mode-get-all-overlays ()
  "Get all sse-log-mode overlays."
  (let ((ovls (overlays-in (point-min) (point-max)))
        sse-log-ovls)
    (dolist (ovl ovls)
      (when (plist-get (overlay-properties ovl) 'sse-log-mode-hidden-phase)
        (push ovl sse-log-ovls)))
    sse-log-ovls))

;; Calculate time between lines

(defvar sse-log-marked-time nil
  "Marked time in ns.")

(defvar sse-log-marked-time-regexp nil
  "Marked time regexp.")

(defvar sse-log-clock-period-in-ns 1.6666666666666666
  "*Clock period in ns.")

(defun sse-log-mode-mark-time ()
  "Mark a time stamp."
  (interactive)
  (when sse-log-marked-time-regexp
    (unhighlight-regexp sse-log-marked-time-regexp))
  (let (beg)
    (save-excursion
      (skip-chars-backward "0123456789.")
      (setq beg (point))
      (skip-chars-forward "0123456789.")
      (setq sse-log-marked-time (string-to-number (buffer-substring beg (point))))
      (setq sse-log-marked-time-regexp (regexp-quote (buffer-substring beg (point))))))
  (highlight-regexp sse-log-marked-time-regexp))

(defun sse-log-mode-delta-time ()
  "Show the delta from the marked time."
  (interactive)
  (if (not sse-log-marked-time)
      (error "No time mark set.")
    (let (beg time)
      (save-excursion
        (skip-chars-backward "0123456789.")
        (setq beg (point))
        (skip-chars-forward "0123456789.")
        (setq time (string-to-number (buffer-substring beg (point))))
        (message (format "%f ns (%f clocks)"
                         (- time sse-log-marked-time)
                         (/ (- time sse-log-marked-time) sse-log-clock-period-in-ns)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar sse-log-mode-map nil "'sse-log-mode' keymap.")
(if (not sse-log-mode-map)
    (let ((map (make-keymap)))
;       (define-key map (kbd "C-x m") 'sse-log-mode-mark-time)
;       (define-key map (kbd "C-x d") 'sse-log-mode-delta-time)
;       (define-key map (kbd "<f10>") 'sse-log-mode-toggle-phase)
;       (define-key map (kbd "<S-f10>") 'sse-log-mode-toggle-all)
;       (define-key map (kbd "<C-f10>") 'sse-log-mode-toggle-main-test)
      (setq sse-log-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defun sse-log-mode ()
  "sse-log-mode is a major mode for browsing SSE run.log files.\n\n
\\{sse-log-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'sse-log-mode)
  (setq mode-name "sse-log")

  (set-syntax-table text-mode-syntax-table)

  (use-local-map sse-log-mode-map)

  (setq next-error-function 'sse-log-mode-next-phase)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(sse-log-mode-font-lock-keywords t))
  (turn-on-font-lock)

  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t
        buffer-invisibility-spec '((t . t)))

  (run-mode-hooks 'sse-log-mode-hook))

(provide 'sse-log-mode)
