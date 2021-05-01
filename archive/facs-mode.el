;;; facs-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom stuff

(defgroup facs-mode nil
  "*facs file mode."
  :group 'programming)

(defcustom facs-indent-offset 4
  "*Indentation offset for `facs-mode'"
  :group 'facs-mode
  :type 'integer)

(defcustom facs-mode-hook nil
  "*List of functions to call on entry to facs-mode mode."
  :group 'facs-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar facs-mode-syntax-table
  (let ((st (make-syntax-table)))
    (if (featurep 'xemacs)
        (progn
          (modify-syntax-entry ?/ ". 14" st)
          (modify-syntax-entry ?* ". 23" st))
      (modify-syntax-entry ?/ ". 14n" st)
      (modify-syntax-entry ?* ". 23n" st))
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for `facs-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font-lock

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

; Preprocessor
; (regexp-opt '("#attribute" "#group" "#ignore" "#prefix" "#include") t)

(defvar facs-mode-font-lock-keywords
  '(
    ("//.*$"
     . font-lock-comment-face)
    ("\\(#\\(?:attribute\\|group\\|i\\(?:\\(?:gnor\\|nclud\\)e\\)\\|prefix\\)\\)"
     . font-lock-builtin-face)
    ("\\?[0-9]?\\([a-zA-Z]\\|<[a-zA-Z]+>\\)"
     . font-lock-variable-name-face)
    )
  "Keyword highlighting specification for `facs-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defun facs-indent-line ()
  "Indent current line for `facs-mode'."
  (interactive)
  (let ((indent-col))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (progn
            (backward-up-list 1)
            (if (looking-at "{")
                (progn
                  (back-to-indentation)
                  (setq indent-col (+ (current-column) facs-indent-offset)))
              (re-search-forward "\\s-" (point-at-eol) 'go)
              (setq indent-col (current-column))))
        (error (setq indent-col 0))))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "}") (>= indent-col facs-indent-offset))
        (setq indent-col (- indent-col facs-indent-offset))))
    (indent-line-to indent-col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymap

(defvar facs-mode-map nil "'facs-mode' keymap.")
(if (not facs-mode-map)
    (let ((map (make-keymap)))
      (setq facs-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode startup

(defun facs-mode ()
  "facs-mode is a major mode for browsing facs files.\n\n
\\{facs-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table facs-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'facs-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       '(facs-mode-font-lock-keywords))
  (set (make-local-variable 'comment-start) "// ")
  (use-local-map facs-mode-map)
  (setq major-mode 'facs-mode)
  (setq mode-name "facs")
  (run-hooks 'facs-mode-hook))

(setq auto-mode-alist (cons '("\\.facs$" . facs-mode) auto-mode-alist))

(provide 'facs-mode)
