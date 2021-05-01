;;; my-sum-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom stuff

(defgroup my-sum-mode nil
  "*SUM log mode."
  :group 'compilation)

(defcustom my-sum-mode-hook nil
  "*List of functions to call on entry to my-sum-mode mode."
  :group 'my-sum-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; faces

(defvar my-sum-mode-font-lock-keywords
  '(
    ("^Cyc [0-9]+" . font-lock-type-face)
    (": \\(INFO.*\\)[[] " .  (1 font-lock-keyword-face))
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
  "Font locking for 'my-sum-mode mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defun my-sum-mode-goto-next-issue ()
  "Goto next WARNING or ERROR."
  (interactive)
  (save-match-data
    (if (looking-at ".*: \\(ERROR\\|WARNING\\) " )
        (re-search-forward ".*: \\(ERROR\\|WARNING\\) " nil nil 2)
      (re-search-forward ".*: \\(ERROR\\|WARNING\\) "))
    (beginning-of-line)))

(defun my-sum-mode-goto-prev-issue ()
  "Goto previous WARNING or ERROR."
  (interactive)
  (save-match-data
    (re-search-backward ".*: \\(ERROR\\|WARNING\\) ")
    (beginning-of-line)))

(defun my-sum-mode-visit-issue ()
  "Visit source of INFO, WARNING, or ERROR in file."
  (interactive)
  (save-match-data
    (beginning-of-line)
    (when (looking-at "Cyc [0-9]+: " )
      (if (looking-at ".*\\(INFO\\|WARNING\\|ERROR\\)")
          (re-search-forward "Source object:.*File: \\(.*\\) Line: \\([0-9]+\\)")
        (re-search-forward ": .+ \\(.+\\):\\([0-9]+\\)$"))
      (let ((filename (match-string 1))
            (line-num (string-to-int (match-string 2))))
        (find-file filename)
        (goto-line line-num)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymap

(defvar my-sum-mode-map nil "'my-sum-mode' keymap.")
(if (not my-sum-mode-map)
    (let ((map (make-keymap)))
      (define-key map [(control ?`)] 'my-sum-mode-goto-next-issue)
      (define-key map [(control ?~)] 'my-sum-mode-goto-prev-issue)
      (define-key map [(control return)] 'my-sum-mode-visit-issue)
      (setq my-sum-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode startup

(defun my-sum-mode ()
  "my-sum-mode is a major mode for browsing SUM logs.\n\n
\\{my-sum-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'my-sum-mode)
  (setq mode-name "SUM")

  (set-syntax-table text-mode-syntax-table)

  (use-local-map my-sum-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(my-sum-mode-font-lock-keywords t))

  (run-hooks 'my-sum-mode-hook))

(setq auto-mode-alist (cons '("\\.SUM$" . my-sum-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.trace$" . my-sum-mode) auto-mode-alist))

(provide 'my-sum-mode)
