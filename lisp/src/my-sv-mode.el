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

(add-to-list 'sv-mode-macros-without-semi "`csco_[a-z_]+")

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

(defvar my-sv-mode-uvm-tag nil
  "*Tag for uvm messages.")

(make-variable-buffer-local 'my-sv-mode-uvm-tag)

(defun my-sv-mode-guess-tag ()
  "Try to guess what the uvm message tag should be."
  ;; Try in file first
  (unless my-sv-mode-uvm-tag
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward "`uvm_\\(info\\|error\\|fatal\\).+?\"\\([a-zA-Z0-9_]+\\)" nil t)
          (setq my-sv-mode-uvm-tag (match-string-no-properties 2))))))
  ;; Now look for package
  (unless my-sv-mode-uvm-tag
    (let ((filenames (file-expand-wildcards "*_pkg.sv")))
      (when filenames
        (string-match "\\(.+?\\)_pkg.sv" (car filenames))
        (setq my-sv-mode-uvm-tag (upcase (match-string 1 (car filenames)))))))
  ;; Give up
  (unless my-sv-mode-uvm-tag
    (setq my-sv-mode-uvm-tag "TODO")))

(defun my-sv-mode-uvm-info (verbosity)
  (my-sv-mode-guess-tag)
  (insert "`uvm_info(\"" my-sv-mode-uvm-tag ":TODO\", $sformatf(\"TODO\", TODO), " verbosity ");")
  (sv-mode-indent-line)
  (beginning-of-line)
  (search-forward "TODO")
  (kill-sexp -1))

(define-abbrev sv-mode-abbrev-table
  "infl"
  ""
  (lambda() (my-sv-mode-uvm-info "UVM_LOW")))

(define-abbrev sv-mode-abbrev-table
  "infm"
  ""
  (lambda() (my-sv-mode-uvm-info "UVM_MEDIUM")))

(define-abbrev sv-mode-abbrev-table
  "infh"
  ""
  (lambda() (my-sv-mode-uvm-info "UVM_HIGH")))

(defun my-sv-mode-uvm-err (type)
  (my-sv-mode-guess-tag)
  (insert "`uvm_" type "(\"" my-sv-mode-uvm-tag ":TODO\", $sformatf(\"TODO\", TODO));")
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
