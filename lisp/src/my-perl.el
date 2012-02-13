;;; my-perl.el

(require 'my-flymake)
(require 'flymake-perlcritic)

(defalias 'perl-mode 'cperl-mode)

(setq-default cperl-break-one-line-blocks-when-indent nil
              cperl-continued-brace-offset -4
              cperl-continued-statement-offset 4
              cperl-fix-hanging-brace-when-indent nil
              cperl-highlight-variables-indiscriminately t
              cperl-indent-left-aligned-comments t
              cperl-indent-level 4
              cperl-invalid-face nil
              cperl-label-offset -4
              cperl-merge-trailing-else t
              flymake-perlcritic-severity 1)

(defun my-cperl-mode-hook ()
  (flymake-mode 1))
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

(add-to-list 'flymake-allowed-file-name-masks '(cperl-mode flymake-perlcritic-init flymake-perlcritic-cleanup))

(provide 'my-perl)
