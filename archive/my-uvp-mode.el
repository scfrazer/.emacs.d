;;; my-uvp-mode.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom stuff

(defgroup my-uvp-mode nil
  "*uvp file mode."
  :group 'compilation)

(defcustom my-uvp-mode-hook nil
  "*List of functions to call on entry to my-uvp-mode mode."
  :group 'my-uvp-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(defun my-uvp-insert-rand-hex-digits (num-digits)
  "Insert random hex digits."
  (let ((idx 0) num)
    (while (< idx num-digits)
      (setq num (random 16))
      (if (< num 10)
          (insert-string (int-to-string num))
        (setq num (- num 10))
        (insert-char (int-to-char (+ num 97))))
      (setq idx (1+ idx)))))

(defun my-uvp-insert-rand-addr ()
  "Insert random 42-bit address."
  (interactive)
  (insert-string "0x")
  (let ((num (random 2)))
    (insert-string (int-to-string num)))
  (my-uvp-insert-rand-hex-digits 10))

(defun my-uvp-insert-rand-data ()
  "Insert random 64-bit data."
  (interactive)
  (insert-string "0x")
  (my-uvp-insert-rand-hex-digits 16))

(defun my-uvp-insert-rand-txid ()
  "Insert random 6-bit txid."
  (interactive)
  (insert-string (format "0x%02x" (random 64))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font-lock

(defvar my-uvp-mode-font-lock-keywords
  '(
    ("//.*$" . font-lock-comment-face)
    ("\\([\"].*[\"]\\)"
     (1 font-lock-string-face))
    ("\\(call\\|min_start_time\\|delay\\|after\\|starts\\|finishes\\)"
     (1 font-lock-keyword-face))
    ("^\\s-*\\([a-zA-Z0-9._]+\\)\\s-*:\\s-*\\([a-zA-Z0-9_]+\\)"
     (1 font-lock-type-face)
     (2 font-lock-function-name-face))
    ("^\\s-*\\([a-zA-Z0-9._]+\\)\\s-*="
     (1 font-lock-variable-name-face))
    )
  "Keyword highlighting specification for `uvp-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymap

(defvar my-uvp-mode-map nil "'my-uvp-mode' keymap.")
(if (not my-uvp-mode-map)
    (let ((map (make-keymap)))
      (define-key map [(control ?$)] 'my-uvp-insert-rand-addr)
      (define-key map [(control ?#)] 'my-uvp-insert-rand-data)
      (define-key map [(control ?%)] 'my-uvp-insert-rand-txid)
      (setq my-uvp-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alignment

(defcustom my-uvp-align-rules-list
  `(
    (my-uvp-comparison
     (regexp . "^\\s-*[a-zA-Z0-9._]+\\(\\s-*\\)=\\(\\s-*\\)\\S-+\\(\\s-*\\)\\(//.*\\)?$")
     (group . (1 2 3))
     (repeat . t))
    )
  "uvp alignment rules."
  :type align-rules-list-type
  :group 'align)

(defcustom my-uvp-align-exclude-rules-list
  `(
    (exc-dq-string
     (regexp . "\"\\([^\"\n]+\\)\"")
     (repeat . t)
     (modes . align-dq-string-modes))

    (exc-open-comment
     (regexp . ,(function (lambda (end reverse)
        (funcall (if reverse 're-search-backward 're-search-forward)
                 (concat "[^ \t\n\\\\]" (regexp-quote comment-start)
                         "\\(.+\\)$") end t))))
     (modes . align-open-comment-modes))
    )
  "uvp alignment exclusion rules."
  :type align-exclude-rules-list-type
  :group 'align)

(put 'my-uvp-align-rules-list 'risky-local-variable t)
(put 'my-uvp-align-exclude-rules-list 'risky-local-variable t)

(add-to-list 'align-dq-string-modes 'uvp-mode)
(add-to-list 'align-open-comment-modes 'uvp-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode startup

(defun my-uvp-mode ()
  "my-uvp-mode is a major mode for browsing uvp files.\n\n
\\{my-uvp-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'my-uvp-mode)
  (setq mode-name "uvp")

  (set (make-local-variable 'comment-start) "// ")

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(my-uvp-mode-font-lock-keywords t))

  (use-local-map my-uvp-mode-map)

  (setq align-mode-rules-list my-uvp-align-rules-list)
  (setq align-exclude-rules-list my-uvp-align-exclude-rules-list)

  (run-hooks 'my-uvp-mode-hook))

(setq auto-mode-alist (cons '("\\.uvp$" . my-uvp-mode) auto-mode-alist))

(provide 'my-uvp-mode)
