;;; my-sv-mode.el

(require 'sv-mode)
(require 'narrow-nested)
(require 'my-mode-line)
(require 'quick-edit)

(defadvice find-tag-default (after my-sv-mode-find-tag-default activate)
  "Remove backtick in sv-mode."
  (when (equal major-mode 'sv-mode)
    (setq ad-return-value (when ad-return-value (replace-regexp-in-string "`" "" ad-return-value)))))

(defun ffap-sv-mode (name)
  (let ((ffap-sv-mode-path
         (when (getenv "SV_PATH")
           (split-string (getenv "SV_PATH") ":"))))
    (ffap-locate-file name t ffap-sv-mode-path)))

(setq ffap-alist (append (list '(sv-mode . ffap-sv-mode)) ffap-alist))

(defun my-sv-mode-expand-reg ()
  (interactive)
  (back-to-indentation)
  (let* ((orig (buffer-substring-no-properties (point) (point-at-eol)))
         (pieces (split-string orig "[.]" t)))
    (kill-region (point-at-bol) (point-at-eol))
    (sv-mode-indent-line)
    (dolist (piece pieces)
      (insert piece "_t::"))
    (delete-char -2)
    (insert " " (car (last pieces)) " = m_" (car pieces) "_rmap.")
    (setq pieces (cdr pieces))
    (dolist (piece pieces)
      (insert "m_" piece "."))
    (delete-char -1)
    (insert ";")))

(defun my-sv-mode-bit-vector ()
  (interactive)
  (let (num-bits)
    (skip-chars-backward "0-9")
    (when (looking-at "[0-9]+")
      (setq num-bits (match-string-no-properties 0))
      (delete-char (length num-bits))
      (setq num-bits (string-to-number num-bits))
      (when (looking-back "^\\s-*" (point-at-bol))
        (insert "bit "))
      (insert "[" (number-to-string (1- num-bits)) ":0] "))))

(defun my-sv-mode-hook ()
  (setq tab-width 3)
  (font-lock-add-keywords nil '(("\\_<\\(bool\\|uint\\)\\_>" (0 'font-lock-type-face))) 'add-to-end)
  (define-key sv-mode-map (kbd "C-c C-e") 'my-sv-mode-expand-reg)
  (define-key sv-mode-map (kbd "C-c C-v") 'my-sv-mode-bit-vector)
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

(defadvice sv-mode-narrow-to-scope (before narrow-nested-sv-scope-before activate)
  (narrow-nested-save-restriction))

(defadvice sv-mode-narrow-to-scope (after narrow-nested-sv-scope-after activate)
  (when (not (buffer-modified-p))
    (my-mode-line-count-lines)))

(defadvice qe-forward-block (around sv-forward-block activate)
  (if (equal major-mode 'sv-mode)
      (sv-mode-end-of-block)
    ad-do-it))

(defadvice qe-backward-block (around sv-backward-block activate)
  (if (equal major-mode 'sv-mode)
      (sv-mode-beginning-of-block)
    ad-do-it))

(define-abbrev sv-mode-abbrev-table
  "tdo"
  ""
  (lambda()
    (insert "//! \\todo ")
    (sv-mode-indent-line)))

(define-abbrev sv-mode-abbrev-table
  "sfor"
  ""
  (lambda ()
    (insert "$sformatf(\"\", )")
    (backward-char 4)))

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
          (var (read-from-minibuffer "Loop variable? ")))
      (insert "(int idx=0, " type " " var "=" var ".first; idx < " var ".num; idx++, " var "=" var ".next) begin\n")
      (sv-mode-indent-line)
      (save-excursion
        (insert "\nend")
        (sv-mode-indent-line)))))

(provide 'my-sv-mode)
