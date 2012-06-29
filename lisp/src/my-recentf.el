;;; my-recentf.el

(require 'recentf)

(setq recentf-exclude (quote ("TAGS" ".*/info/dir" "\\.~.+~" ".*/[0-9]+$")))
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(setq recentf-menu-filter nil)
(setq recentf-auto-cleanup "11:59pm")

(defun my-recentf-clear-list ()
  (interactive)
  (setq recentf-list nil))

(defun my-recentf-dialog-mode-hook ()
  (hl-line-mode 1))
(add-hook 'recentf-dailog-mode-hook 'my-recentf-dialog-mode-hook)

(recentf-mode t)

(provide 'my-recentf)
