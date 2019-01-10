;;; my-rg.el

(require 'rg)

(setq-default rg-custom-type-aliases nil
              rg-hide-command nil)

(rg-define-toggle "-uu" "I")
(rg-define-toggle "--word-regexp" "w")

(defun my-rg-toggle-group ()
  "Toggle grouping and rerun."
  (interactive)
  (setq rg-group-result (not rg-group-result))
  (rg-rerun))

(rg-define-search my-rg-current-ask
  :query ask
  :format literal
  :files current
  :dir current)

(rg-define-search my-rg-current-region
  :query (buffer-substring-no-properties (region-beginning) (region-end))
  :format literal
  :files current
  :dir current)

(defun my-rg-current (&optional arg)
  "Ripgrep in current directory.
With ARG do literal with current region."
  (interactive "P")
  (if arg
      (call-interactively 'my-rg-current-region)
    (call-interactively 'my-rg-current-ask)))

(rg-define-search my-rg-project-ask
  :query ask
  :format literal
  :files current
  :dir project)

(rg-define-search my-rg-project-region
  :query (buffer-substring-no-properties (region-beginning) (region-end))
  :format literal
  :files current
  :dir project)

(defun my-rg-project (&optional arg)
  "Ripgrep in current project.
With ARG do literal with current region."
  (interactive "P")
  (if arg
      (call-interactively 'my-rg-project-region)
    (call-interactively 'my-rg-project-ask)))

(define-key rg-mode-map "C-x C-q" 'wgrep-change-to-wgrep-mode)
(define-key rg-mode-map "G" 'my-rg-toggle-group)
(define-key rg-mode-map "L" 'rg-list-searches)
(define-key rg-mode-map "l" 'rg-rerun-change-literal)

(provide 'my-rg)
