;;; my-sv-mode.el

(require 'narrow-nested)
(require 'my-mode-line)

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

(provide 'my-sv-mode)
