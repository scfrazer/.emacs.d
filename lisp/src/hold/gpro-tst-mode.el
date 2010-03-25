;;; gpro-tst-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom stuff

(defgroup gpro-tst-mode nil
  "*gpro-tst file mode."
  :group 'compilation)

(defcustom gpro-tst-mode-hook nil
  "*List of functions to call on entry to gpro-tst-mode mode."
  :group 'gpro-tst-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax table

(defvar gpro-tst-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?* "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `gpro-tst-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font-lock

(defvar gpro-tst-mode-font-lock-keywords
  '(
    ("\*.*$" . font-lock-comment-face)
    ("^\\s-*\\(TEST\\|EPILOGUE\\|END_OF_TEST\\)"
     (1 font-lock-function-name-face))
    ("^\\s-*\\(D\\|R\\|I\\|TAG\\)\\s-+\\([0-9A-Za-z_]+\\)\\s-+\\([0-9A-Fa-f]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-type-face))
    ("^\\s-*\\(INITIALIZATIONS\\|RESULTS\\):\\s-+\\(.+\\)$"
     (1 font-lock-function-name-face)
     (2 font-lock-string-face))
    ("^\\s-*\\(LEVEL\\|END_OF_LEVEL\\)\\s-+[0-9]\\s-+\\(CHIP\\|CORE\\|THREAD\\)\\s-+[0-9]"
     (1 font-lock-function-name-face)
     (2 font-lock-function-name-face))
    ("^\\s-*\\(PHASE\\)\\s-+[0-9]\\s-+\\(INSTRUCTIONS\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-function-name-face))
    ("^\\s-*\\(TRACE\\s-+\\(READS\\|WRITES\\)\\)"
     (1 font-lock-function-name-face))
    )
  "Keyword highlighting specification for `gpro-tst-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymap

(defvar gpro-tst-mode-map nil "'gpro-tst-mode' keymap.")
(if (not gpro-tst-mode-map)
    (let ((map (make-keymap)))
      (setq gpro-tst-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode startup

(defun gpro-tst-mode ()
  "gpro-tst-mode is a major mode for browsing gpro-tst files.\n\n
\\{gpro-tst-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'gpro-tst-mode)
  (setq mode-name "gpro-tst")

  (set-syntax-table gpro-tst-mode-syntax-table)
  (set (make-local-variable 'comment-start) "* ")

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gpro-tst-mode-font-lock-keywords t))

  (use-local-map gpro-tst-mode-map)

  (run-hooks 'gpro-tst-mode-hook))

(setq auto-mode-alist (cons '("\\.tst$" . gpro-tst-mode) auto-mode-alist))

(provide 'gpro-tst-mode)
