;;; elog-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(defgroup elog-mode nil
  "*ELOG log mode."
  :group 'compilation)

(defcustom elog-mode-hook nil
  "*List of functions to call on entry to elog-mode mode."
  :group 'elog-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

(defface elog-mode-error-face
    '((((class color) (background dark)) (:foreground "red" :bold t))
      (((class color) (background light)) (:foreground "red" :bold t)))
    "Font Lock mode face used to highlight errors."
    :group 'elog-mode)

(defface elog-mode-warning-face
    '((((class color) (background dark)) (:foreground "yellow2" :bold t))
      (((class color) (background light)) (:foreground "yellow4" :bold t)))
    "Font Lock mode face used to highlight warnings."
    :group 'elog-mode)

(defface elog-mode-debug-face
    '((((class color) (background dark)) (:foreground "red" :background "yellow" :bold t))
      (((class color) (background light)) (:foreground "red" :background "yellow" :bold t)))
    "Font Lock mode face used to highlight debug markers."
    :group 'elog-mode)

(defface elog-mode-debug-msg-face
    '((((class color) (background dark)) (:foreground "CadetBlue1"))
      (((class color) (background light)) (:bold t)))
    "Font Lock mode face used to highlight debug messages."
    :group 'elog-mode)

(defface elog-mode-timestamp-face
    '((((class color) (background dark)) (:foreground "plum2"))
      (((class color) (background light)) (:foreground "purple3")))
    "Font Lock mode face used to highlight timestamps."
    :group 'elog-mode)

(defface elog-mode-path-face
    '((((class color) (background dark)) (:foreground "SkyBlue1"))
      (((class color) (background light)) (:foreground "SteelBlue4")))
    "Font Lock mode face used to highlight tags."
    :group 'elog-mode)

(defface elog-mode-domain-face
    '((((class color) (background dark)) (:foreground "LightGoldenrod2" :bold t))
      (((class color) (background light)) (:bold t)))
    "Font Lock mode face used to highlight domain advance statements."
    :group 'elog-mode)

(defface elog-mode-send-face
    '((((class color) (background dark)) (:foreground "white" :background "green3" :bold t))
      (((class color) (background light)) (:foreground "white" :background "green3" :bold t)))
    "Font Lock mode face used to highlight send statements."
    :group 'elog-mode)

(defface elog-mode-match-face
    '((((class color) (background dark)) (:foreground "PaleGreen2" :bold t))
      (((class color) (background light)) (:foreground "green3" :bold t)))
    "Font Lock mode face used to highlight match statements."
    :group 'elog-mode)

(defface elog-mode-pass-face
    '((((class color) (background dark)) (:foreground "PaleGreen2" :bold t))
      (((class color) (background light)) (:foreground "green3" :bold t)))
    "Font Lock mode face used to highlight pass statements."
    :group 'elog-mode)

(defface elog-mode-msg-level-face
  '((((class color) (background dark)) (:foreground "cyan3"))
    (((class color) (background light)) (:foreground "cyan3")))
  "Font Lock mode face used to highlight messages."
  :group 'elog-mode)

(defface elog-mode-msg-face
    '((((class color) (background dark)) (:foreground "PaleTurquoise2"))
      (((class color) (background light)) (:foreground "tan4")))
    "Font Lock mode face used to highlight messages."
    :group 'elog-mode)

(defface elog-mode-highlight-phase-face
  '((((class color) (background dark)) (:foreground "white" :background "slateblue3" :bold t))
    (((class color) (background light)) (:foreground "white" :background "slateblue3" :bold t)))
  "Font Lock mode face used to highlight tags."
  :group 'elog-mode)

(defvar elog-mode-font-lock-keywords
  '(
    ("^\\s +\\*\\*\\* Dut error.*$"
     (0 'elog-mode-error-face))
    ("!!!! ERROR ERROR ERROR !!!!"
     (0 'elog-mode-error-face))
    ("^\\s +\\*\\*\\* Warning.*$"
     (0 'elog-mode-warning-face))
    ("^\\s *\\(DEBUG.*?:\\)\\(.*\\)$"
     (1 'elog-mode-debug-face)
     (2 'elog-mode-debug-msg-face))
    ("^\\(\\[.*?\\]\\) \\(.*?: \\)"
     (1 'elog-mode-timestamp-face)
     (2 'elog-mode-path-face))
    ("\\(\\(testflow advancing to:.+\\|\\*\\{10\\}\\).*\\)"
     (0 'elog-mode-domain-face))
    ("Sending.*"
     (0 'elog-mode-send-face))
    ("Matched.*$"
     (0 'elog-mode-match-face))
    ("!+ Test has Failed! !+"
     (0 'elog-mode-error-face t))
    ("\\*+ Test has Passed! \\*+"
     (0 'elog-mode-pass-face t))
    ("^\\s *\\([0-9.]+ [fpnum]s\\)\\s *\\[\\(.+?\\)\\] (\\(.+?\\)): \\(.*\\)$"
     (1 'elog-mode-timestamp-face)
     (2 'elog-mode-msg-level-face)
     (3 'elog-mode-path-face)
     (4 'elog-mode-msg-face))
    ("(Err:.+?)"
     (0 'elog-mode-error-face t))
    )
  "Font locking for 'elog-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun elog-mode-next-phase (arg reset)
  "Goto next TESTFLOW phase (or error)."
  (interactive)
  (let ((phase-or-error-regexp "\\(testflow advancing to:\\|!!!! ERROR ERROR ERROR !!!!\\|\\*\\*\\* Dut error\\)")
        ov)
    (if (and arg (< arg 0))
        (re-search-backward phase-or-error-regexp)
      (end-of-line)
      (re-search-forward phase-or-error-regexp))
    (beginning-of-line)
    (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
      (overlay-put ov 'face 'elog-mode-highlight-phase-face)
      (sit-for 1)
      (delete-overlay ov))))

;; Show/hide phases

(defvar elog-mode-testflow-regexp "testflow advancing to:")
(defvar elog-mode-testflow-sep-regexp "(GLOBAL_DOMAIN): \*\*\*\*\*\*\*\*\*\*")
(defvar elog-mode-config-regexp "printing \\(regenerated \\)?config tree")
(defvar elog-mode-config-sep-regexp "(ASTRO): ==============================================")

(defun elog-mode-toggle-phase ()
  "Toggle hide/show current phase."
  (interactive)
  (beginning-of-line)
  (while (and (not (eobp))
              (looking-at (concat "^.+?\\(" elog-mode-testflow-regexp "\\|"
                                  elog-mode-testflow-sep-regexp "\\|"
                                  elog-mode-config-regexp "\\|"
                                  elog-mode-config-sep-regexp "\\)")))
    (forward-line 1))
  (unless (elog-mode-show-phase)
    (elog-mode-hide-phase)))

(defun elog-mode-show-phase ()
  "Show current phase."
  (let ((ovls (overlays-at (point))))
    (catch 'break
      (dolist (ovl ovls)
        (when (plist-get (overlay-properties ovl) 'elog-mode-hidden-phase)
          (elog-mode-remove-overlay ovl)
          (throw 'break t)))
      nil)))

(defun elog-mode-remove-overlay (ovl)
  "Remove overlay."
  (delete-overlay ovl))

(defun elog-mode-hide-phase ()
  "Hide current phase."
  (let ((marker-regexp (concat "^.+?\\(" elog-mode-testflow-regexp "\\|" elog-mode-config-sep-regexp "\\)"))
         start end)
    (save-excursion
      (when (re-search-backward marker-regexp nil 'go)
        (if (string= (match-string-no-properties 1) elog-mode-testflow-regexp)
            (progn
              (beginning-of-line)
              (forward-line 2))
          (beginning-of-line)
          (forward-line 1)
          (when (looking-at (concat "^.+?" elog-mode-config-regexp))
            (forward-line 1))))
      (setq start (point)))
    (save-excursion
      (re-search-forward marker-regexp nil 'go)
      (beginning-of-line)
      (when (string= (match-string-no-properties 1) elog-mode-testflow-regexp)
        (forward-line -1))
      (setq end (point)))
    (let ((phase-overlay (make-overlay start end)))
      (overlay-put phase-overlay 'before-string "\n")
      (overlay-put phase-overlay 'elog-mode-hidden-phase t)
      (overlay-put phase-overlay 'isearch-open-invisible 'elog-mode-remove-overlay)
      (overlay-put phase-overlay 'invisible t))))

(defun elog-mode-toggle-all ()
  "Toggle showing all phases."
  (interactive)
  (unless (elog-mode-show-all)
    (elog-mode-hide-all)))

(defun elog-mode-show-all ()
  "Show all phases."
  (let ((ovls (elog-mode-get-all-overlays)))
    (when ovls
      (dolist (ovl ovls)
        (elog-mode-remove-overlay ovl))
      t)))

(defun elog-mode-hide-all ()
  "Hide all phases."
  (elog-mode-hide-up-to-phase "NOENDPHASE"))

(defun elog-mode-toggle-main-test ()
  "Toggle showing all before main-test phase."
  (interactive)
  (unless (elog-mode-show-all)
    (elog-mode-hide-up-to-main-test)))

(defun elog-mode-hide-up-to-main-test ()
  "Hide phases up to main-test."
  (elog-mode-hide-up-to-phase "GLOBAL/CSCO_MAIN_TEST"))

(defun elog-mode-hide-up-to-phase (phase)
  "Hide up to a phase."
  (goto-char (point-min))
  (elog-mode-hide-phase)
  (catch 'done
    (while (re-search-forward
            (concat "^.+?\\(" elog-mode-testflow-regexp "\\|" elog-mode-config-sep-regexp "\\)") nil 'go)
      (when (looking-at (concat ".+?" phase))
        (throw 'done t))
      (elog-mode-hide-phase)))
  (goto-char (point-min)))

(defun elog-mode-get-all-overlays ()
  "Get all elog-mode overlays."
  (let ((ovls (overlays-in (point-min) (point-max)))
        elog-ovls)
    (dolist (ovl ovls)
      (when (plist-get (overlay-properties ovl) 'elog-mode-hidden-phase)
        (push ovl elog-ovls)))
    elog-ovls))

;; Calculate time between lines

(defvar elog-marked-time nil
  "Marked time in ns.")

(defvar elog-marked-time-regexp nil
  "Marked time regexp.")

(defvar elog-clock-period-in-ns 1.6666666666666666
  "*Clock period in ns.")

(defun elog-mode-mark-time ()
  "Mark a time stamp."
  (interactive)
  (when elog-marked-time-regexp
    (unhighlight-regexp elog-marked-time-regexp))
  (let (beg)
    (save-excursion
      (skip-chars-backward "0123456789.")
      (setq beg (point))
      (skip-chars-forward "0123456789.")
      (setq elog-marked-time (string-to-number (buffer-substring beg (point))))
      (setq elog-marked-time-regexp (regexp-quote (buffer-substring beg (point))))))
  (highlight-regexp elog-marked-time-regexp))

(defun elog-mode-delta-time ()
  "Show the delta from the marked time."
  (interactive)
  (if (not elog-marked-time)
      (error "No time mark set.")
    (let (beg time)
      (save-excursion
        (skip-chars-backward "0123456789.")
        (setq beg (point))
        (skip-chars-forward "0123456789.")
        (setq time (string-to-number (buffer-substring beg (point))))
        (message (format "%f ns (%f clocks)"
                         (- time elog-marked-time)
                         (/ (- time elog-marked-time) elog-clock-period-in-ns)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar elog-mode-map nil "'elog-mode' keymap.")
(if (not elog-mode-map)
    (let ((map (make-keymap)))
      (define-key map (kbd "C-x m") 'elog-mode-mark-time)
      (define-key map (kbd "C-x d") 'elog-mode-delta-time)
      (define-key map (kbd "<f10>") 'elog-mode-toggle-phase)
      (define-key map (kbd "<S-f10>") 'elog-mode-toggle-all)
      (define-key map (kbd "<C-f10>") 'elog-mode-toggle-main-test)
      (setq elog-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defun elog-mode ()
  "elog-mode is a major mode for browsing Specman elog files.\n\n
\\{elog-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'elog-mode)
  (setq mode-name "elog")

  (set-syntax-table text-mode-syntax-table)

  (use-local-map elog-mode-map)

  (setq next-error-function 'elog-mode-next-phase)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(elog-mode-font-lock-keywords t))
  (turn-on-font-lock)

  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t
        buffer-invisibility-spec '((t . t)))

  (run-mode-hooks 'elog-mode-hook))

(setq auto-mode-alist (cons '("\\.elog$" . elog-mode) auto-mode-alist))

(provide 'elog-mode)
