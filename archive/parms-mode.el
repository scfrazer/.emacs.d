;;; parms-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom stuff

(defgroup parms-mode nil
  "*parms file mode."
  :group 'programming)

(defcustom parms-indent-offset 4
  "*Indentation offset for `parms-mode'"
  :group 'parms-mode
  :type 'integer)

(defcustom parms-mode-hook nil
  "*List of functions to call on entry to parms-mode mode."
  :group 'parms-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar parms-mode-syntax-table
  (let ((st (make-syntax-table)))
    (if (featurep 'xemacs)
        (progn
          (modify-syntax-entry ?/ ". 14" st)
          (modify-syntax-entry ?* ". 23" st))
      (modify-syntax-entry ?/ ". 14n" st)
      (modify-syntax-entry ?* ". 23n" st))
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for `parms-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font-lock

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
; (regexp-opt '("#include" "#prefix") t)

(defvar parms-mode-font-lock-keywords
  '(
    ("//.*$"
     . font-lock-comment-face)
    ("\\(#\\(?:include\\|prefix\\)\\)"
     . font-lock-builtin-face)
    )
  "Keyword highlighting specification for `parms-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defun parms-indent-line ()
  "Indent current line for `parms-mode'."
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
                  (setq indent-col (+ (current-column) parms-indent-offset)))
              (re-search-forward "\\s-" (point-at-eol) 'go)
              (setq indent-col (current-column))))
        (error (setq indent-col 0))))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "}") (>= indent-col parms-indent-offset))
        (setq indent-col (- indent-col parms-indent-offset))))
    (indent-line-to indent-col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymap

(defvar parms-mode-map nil "'parms-mode' keymap.")
(if (not parms-mode-map)
    (let ((map (make-keymap)))
      (setq parms-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode startup

(defun parms-mode ()
  "parms-mode is a major mode for browsing parms files.\n\n
\\{parms-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table parms-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'parms-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       '(parms-mode-font-lock-keywords))
  (set (make-local-variable 'comment-start) "// ")
  (use-local-map parms-mode-map)
  (setq major-mode 'parms-mode)
  (setq mode-name "parms")
  (run-hooks 'parms-mode-hook))

(setq auto-mode-alist (cons '("\\.parms$" . parms-mode) auto-mode-alist))

(provide 'parms-mode)
