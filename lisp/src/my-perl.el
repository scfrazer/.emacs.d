;;; my-perl.el

(require 'my-flymake)

(defalias 'perl-mode 'cperl-mode)

(setq-default cperl-break-one-line-blocks-when-indent nil
              cperl-close-paren-offset -4
              cperl-continued-brace-offset -4
              cperl-continued-statement-offset 4
              cperl-fix-hanging-brace-when-indent nil
              cperl-highlight-variables-indiscriminately t
              cperl-indent-left-aligned-comments t
              cperl-indent-level 4
              cperl-indent-parens-as-block t
              cperl-invalid-face nil
              cperl-label-offset -4
              cperl-merge-trailing-else t)

(defun my-cperl-mode-hook ()
  (define-key cperl-mode-map (kbd "{") nil)
  (define-key cperl-mode-map (kbd "[") nil)
  (define-key cperl-mode-map (kbd "(") nil)
  (define-key cperl-mode-map (kbd "<") nil)
  (define-key cperl-mode-map (kbd "}") nil)
  (define-key cperl-mode-map (kbd "]") nil)
  (define-key cperl-mode-map (kbd ")") nil)
  (define-key cperl-mode-map (kbd ";") nil)
  (define-key cperl-mode-map (kbd ":") nil)
  (flymake-mode 1))
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

(defun my-perl-tidy (&optional arg)
  "Run perltidy on marked region, or entire buffer."
  (interactive "*P")
  (let ((pos (point))
        (profile "~/.perltidyrc")
        beg end)
  (when arg
    (setq profile (read-file-name "perltidy profile: " "~/" nil t ".perltidyrc")))
  (if (region-active-p)
      (setq beg (region-beginning)
            end (region-end))
    (setq beg (point-min)
          end (point-max)))
  (shell-command-on-region beg end (concat "perltidy -q -pro=" (expand-file-name profile)) nil t)
  (goto-char pos)))

(provide 'my-perl)
