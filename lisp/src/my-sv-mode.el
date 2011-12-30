;;; my-sv-mode.el

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

(defun my-sv-mode-hook ()
  (setq ff-other-file-alist '(("\\.sv$" (".svh"))
                              ("\\.svh$" (".sv"))
                              ("\\.s$" (".v"))
                              ("\\.v$" (".s" ".vh")))))

(add-hook 'sv-mode-hook 'my-sv-mode-hook)

(setq ffap-alist (append (list '(sv-mode . ffap-sv-mode)) ffap-alist))

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

(defvar my-sv-mode-uvm-tag "CSCO_SB"
  "*Tag for uvm messages.")

(defun my-sv-mode-uvm-info (verbosity)
  (insert "`uvm_info(\"" my-sv-mode-uvm-tag "\", $sformatf(\"TODO\", TODO), " verbosity ");")
  (sv-mode-indent-line)
  (beginning-of-line)
  (search-forward "TODO")
  (kill-sexp -1))

(define-abbrev sv-mode-abbrev-table
  "inl"
  ""
  (lambda() (my-sv-mode-uvm-info "UVM_LOW")))

(define-abbrev sv-mode-abbrev-table
  "inm"
  ""
  (lambda() (my-sv-mode-uvm-info "UVM_MEDIUM")))

(define-abbrev sv-mode-abbrev-table
  "inh"
  ""
  (lambda() (my-sv-mode-uvm-info "UVM_HIGH")))

(defun my-sv-mode-uvm-err (type)
  (insert "`uvm_" type "(\"" my-sv-mode-uvm-tag "\", $sformatf(\"TODO\", TODO));")
  (sv-mode-indent-line)
  (beginning-of-line)
  (search-forward "TODO")
  (kill-sexp -1))

(define-abbrev sv-mode-abbrev-table
  "err"
  ""
  (lambda() (my-sv-mode-uvm-err "error")))

(define-abbrev sv-mode-abbrev-table
  "fat"
  ""
  (lambda() (my-sv-mode-uvm-err "fatal")))

(provide 'my-sv-mode)
