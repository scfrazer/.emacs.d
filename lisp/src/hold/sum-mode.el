;;; sum-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom stuff

(defgroup sum-mode nil
  "*SUM log mode."
  :group 'compilation)

(defcustom sum-mode-hook nil
  "*List of functions to call on entry to sum-mode mode."
  :group 'sum-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; faces

(unless (boundp 'font-lock-builtin-face)
  (defvar font-lock-builtin-face 'font-lock-builtin-face
    "Face name to use for builtins.")
  (defface font-lock-builtin-face
    '((((type tty) (class color)) (:foreground "blue" :weight light))
      (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "Orchid"))
      (((class color) (background dark)) (:foreground "LightSteelBlue"))
      (t (:bold t)))
    "Font Lock mode face used to highlight builtins."
    :group 'font-lock-highlighting-faces))

(defvar sum-mode-font-lock-keywords
  '(
    ("^Cyc [0-9]+" . font-lock-type-face)
    (": \\(INFO.*\\)[[]" .  (1 font-lock-keyword-face))
    (": \\(WARNING.*\\)[[]" .  (1 font-lock-builtin-face))
    (": \\(ERROR.*\\)[[]" .  (1 font-lock-warning-face))
    ("Source object: \\(.*\\) File: \\(.*\\) Line: \\([0-9]+\\)"
     (1 font-lock-reference-face)
     (2 font-lock-string-face)
     (3 font-lock-variable-name-face))
    (": \\(.+\\) \\(.+\\):\\([0-9]+\\)$"
     (1 font-lock-reference-face)
     (2 font-lock-string-face)
     (3 font-lock-variable-name-face))
    ("^Cyc [0-9]+: \\([^ ]+\\) " . (1 font-lock-reference-face))
    )
  "Font locking for 'sum-mode mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defun sum-mode-goto-next-issue ()
  "Goto next WARNING or ERROR."
  (interactive)
  (save-match-data
    (beginning-of-line)
    (if (looking-at "^.*: \\(ERROR\\|WARNING\\) " )
        (re-search-forward "^.*: \\(ERROR\\|WARNING\\) " nil nil 2)
      (re-search-forward "^.*: \\(ERROR\\|WARNING\\) "))
    (beginning-of-line)))

(defun sum-mode-goto-prev-issue ()
  "Goto previous WARNING or ERROR."
  (interactive)
  (save-match-data
    (re-search-backward "^.*: \\(ERROR\\|WARNING\\) ")
    (beginning-of-line)))

(defun sum-mode-visit-issue ()
  "Visit source of INFO, WARNING, or ERROR in file."
  (interactive)
  (let (filename line-num)
    (save-excursion
      (save-match-data
        (beginning-of-line)
        (when (looking-at "Cyc [0-9]+: " )
          (if (looking-at ".*\\(INFO\\|WARNING\\|ERROR\\)")
              (re-search-forward "Source object:.*File: \\(.*\\) Line: \\([0-9]+\\)")
            (re-search-forward ": .+ \\(.+\\):\\([0-9]+\\)$"))
          (setq filename (match-string 1))
          (setq line-num (string-to-int (match-string 2))))))
    (when (file-exists-p filename)
      (find-file filename)
      (goto-line line-num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymap

(defvar sum-mode-map nil "'sum-mode' keymap.")
(if (not sum-mode-map)
    (let ((map (make-keymap)))
      (define-key map [(control ?`)] 'sum-mode-goto-next-issue)
      (define-key map [(control ?~)] 'sum-mode-goto-prev-issue)
      (define-key map [(control return)] 'sum-mode-visit-issue)
      (setq sum-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode startup

(defun sum-mode ()
  "sum-mode is a major mode for browsing SUM logs.\n\n
\\{sum-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'sum-mode)
  (setq mode-name "SUM")

  (set-syntax-table text-mode-syntax-table)

  (use-local-map sum-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(sum-mode-font-lock-keywords t))

  (run-hooks 'sum-mode-hook))

(setq auto-mode-alist (cons '("\\.SUM$" . sum-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.trace$" . sum-mode) auto-mode-alist))

(provide 'sum-mode)
