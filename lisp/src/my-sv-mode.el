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

(setq ffap-alist (append (list '(sv-mode . ffap-sv-mode)) ffap-alist))

(defun my-sv-mode-hook ()
  (font-lock-add-keywords nil '(("\\_<\\(bool\\|uint\\)\\_>" (0 'font-lock-type-face))) 'add-to-end)
  (setq ff-other-file-alist '(("\\.sv$" (".svh"))
                              ("\\.svh$" (".sv"))
                              ("\\.s$" (".v"))
                              ("\\.v$" (".s" ".vh")))))

(add-hook 'sv-mode-hook 'my-sv-mode-hook)

(add-to-list 'sv-mode-macros-without-semi "`csco_[a-z_]+")

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

(provide 'my-sv-mode)
