;;; my-sv-mode.el

(require 'sv-mode)
(require 'quick-edit)

(eval-after-load "subr"
  `(progn
     (defadvice find-tag-default (after my-sv-mode-find-tag-default activate)
       "Remove backtick in sv-mode."
       (when (equal major-mode 'sv-mode)
         (setq ad-return-value (when ad-return-value (replace-regexp-in-string "`" "" ad-return-value)))))))

(defun my-sv-mode-expand-reg ()
  "Expand register definition."
  (interactive)
  (back-to-indentation)
  (let* ((orig (buffer-substring-no-properties (point) (point-at-eol)))
         (pieces (split-string orig "[.]" t)))
    (kill-region (point-at-bol) (point-at-eol))
    (sv-mode-indent-line)
    (when (string-match ".*/test/.*" (buffer-file-name))
      (insert "csco_rmap_pkg::"))
    (dolist (piece pieces)
      (insert piece "_t::"))
    (delete-char -2)
    (insert " ")
    (save-excursion
      (insert (car (last pieces)) " = m_" (car pieces) "_rmap.")
      (setq pieces (cdr pieces))
      (dolist (piece pieces)
        (insert "m_" piece "."))
      (delete-char -1)
      (insert ";"))))

(defun my-sv-mode-bit-vector ()
  "Expand bit vector."
  (interactive)
  (let (num-bits)
    (skip-chars-backward "0-9")
    (when (looking-at "[0-9]+")
      (setq num-bits (match-string-no-properties 0))
      (delete-char (length num-bits))
      (setq num-bits (string-to-number num-bits))
      (unless (looking-back "[a-zA-Z_]\\s-*" (point-at-bol))
        (insert "bit "))
      (insert "[" (number-to-string (1- num-bits)) ":0] "))))

(defun my-sv-mode-uvm-create ()
  "Expand UVM create var."
  (interactive)
  (let ((pos (point)))
    (when (re-search-backward "[^a-zA-Z0-9_]\\([a-zA-Z0-9_]+\\)\\s-*=" (point-at-bol) t)
      (let ((var (match-string 1)))
        (goto-char pos)
        (when (re-search-backward (concat "^\\s-*[^a-zA-Z0-9_:]\\([a-zA-Z0-9_:]+\\)\\s-+" var) nil t)
          (let ((type (match-string 1)))
            (goto-char pos)
            (insert type "::type_id::create(\"" var "\", this);")))))))

(defun my-sv-mode-uvm-new (&optional component)
  "Fill in 'new' function for uvm_(object|component)."
  (interactive "*P")
  (let* ((pos (point))
         (class-type (sv-mode-get-class-type))
         (name (nth 0 class-type)))
    (insert "function new(string name = \"" name "\"")
    (when component
      (insert ", uvm_component parent"))
    (insert ");\nsuper.new(name")
    (when component
      (insert ", parent"))
    (insert ");\nendfunction : new\n")
    (indent-region pos (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-sv-xclip-program (executable-find "xclip")
  "Name of XClip program tool.")

(defun my-sv-breakpoint (&optional arg)
  "Create a VCS breakpoint string and copy to the clipboard.
With prefix argument, add a condition."
  (interactive "P")
  (when (and my-sv-xclip-program (getenv "DISPLAY"))
    (let* ((condition (and arg (read-string "Breakpoint condition? ")))
           (breakpoint (concat "stop -file {" (buffer-file-name) "} -line {" (number-to-string (line-number-at-pos)) "}"
                               (if arg (concat " -condition {" condition "}") "")))
           (process-connection-type nil)
           (proc (start-process "xclip" nil "xclip" "-selection" "primary")))
      (process-send-string proc breakpoint)
      (process-send-eof proc)
      (message breakpoint))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-sv-mode-hook ()
  (font-lock-add-keywords nil '(("\\_<\\(bool\\|uint\\)\\_>" (0 'font-lock-type-face))) 'add-to-end)
  (define-key sv-mode-map (kbd "C-c C-e") 'my-sv-mode-expand-reg)
  (define-key sv-mode-map (kbd "<f10>") 'my-sv-breakpoint)
  (setq ff-other-file-alist '(("\\.sv$" (".svh"))
                              ("\\.svh$" (".sv"))
                              ("\\.s$" (".v"))
                              ("\\.v$" (".s" ".vh")))))

(add-hook 'sv-mode-hook 'my-sv-mode-hook)

(add-to-list 'sv-mode-macros-without-semi "`csco_[a-z_]+")

(defadvice sv-mode-create-skeleton-from-prototype (before ct-co-sv-mode-create-proto activate)
  (ff-get-other-file)
  (let ((filename (buffer-file-name)))
    (when (and (string-match "^/vob/" filename)
               (not (file-writable-p filename)))
      (let ((clearcase-checkout-arguments (list "-unreserved" "-nmaster"))
            (clearcase-suppress-checkout-comments t))
        (clearcase-commented-checkout filename)))
    (switch-to-buffer (other-buffer))))

(defadvice qe-forward-block (around sv-forward-block activate)
  (if (equal major-mode 'sv-mode)
      (sv-mode-end-of-block)
    ad-do-it))

(defadvice qe-backward-block (around sv-backward-block activate)
  (if (equal major-mode 'sv-mode)
      (sv-mode-beginning-of-block)
    ad-do-it))

(defun my-sv-mode-uvm-info (verbosity)
  (interactive "sVerbosity? ")
  (sv-mode-guess-uvm-tag)
  (insert "`uvm_info(\"" sv-mode-uvm-tag ":\", \"TODO\", " verbosity ");")
  (sv-mode-indent-line)
  (search-backward ":")
  (forward-char 1))
(setq sv-mode-uvm-info-function 'my-sv-mode-uvm-info)

(defun my-sv-mode-uvm-err (type)
  (interactive "sType? ")
  (sv-mode-guess-uvm-tag)
  (insert "`uvm_" type "(\"" sv-mode-uvm-tag ":\", \"TODO\");")
  (sv-mode-indent-line)
  (search-backward ":")
  (forward-char 1))
(setq sv-mode-uvm-err-function 'my-sv-mode-uvm-err)

(defadvice expand-abbrev (around my-sv-expand-abbrev-advice activate)
  (if (and (equal major-mode 'sv-mode)
           (looking-back "[0-9]"))
      (my-sv-mode-bit-vector)
    ad-do-it))

(define-abbrev sv-mode-abbrev-table
  "sfor"
  ""
  (lambda ()
    (insert "$sformatf(\"\", )")
    (backward-char 4)))

(define-abbrev sv-mode-abbrev-table
  "for"
  "for"
  (lambda()
    (let ((var (read-from-minibuffer "Loop variable? " "idx"))
          (limit (read-from-minibuffer "Limit? ")))
      (insert "(int " var " = 0; " var " < " limit "; " var "++) begin\n")
      (sv-mode-indent-line)
      (save-excursion
        (insert "\nend")
        (sv-mode-indent-line)))))

(define-abbrev sv-mode-abbrev-table
  "forenum"
  "for"
  (lambda()
    (let ((type (read-from-minibuffer "Enum type? "))
          (var (read-from-minibuffer "Enum loop variable name? ")))
      (insert "(int idx=0, " type " " var "=" var ".first(); idx < " var ".num(); idx++, " var "=" var ".next()) begin\n")
      (sv-mode-indent-line)
      (save-excursion
        (insert "\nend")
        (sv-mode-indent-line)))))

(define-abbrev sv-mode-abbrev-table
  "create"
  ""
  'my-sv-mode-uvm-create)

(define-abbrev sv-mode-abbrev-table
  "uno"
  ""
  (lambda () (my-sv-mode-uvm-new nil)))

(define-abbrev sv-mode-abbrev-table
  "unc"
  ""
  (lambda () (my-sv-mode-uvm-new t)))

(provide 'my-sv-mode)
