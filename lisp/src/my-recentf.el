;;; my-recentf.el

(require 'recentf)
(require 'midnight)
(require 'subr-x)

(add-hook 'midnight-hook 'recentf-cleanup)
(remove-hook 'midnight-hook 'clean-buffer-list)

(when recentf-save-file
  (if (getenv "WORKSPACE")
      (let ((ws-name (file-name-nondirectory (getenv "WORKSPACE"))))
        (setq recentf-save-file (convert-standard-filename (concat "~/.recentf/" ws-name))))
    (setq recentf-save-file (convert-standard-filename (concat "~/.recentf/recentf")))))

(setq recentf-auto-cleanup "11:59pm"
      recentf-exclude (quote ("TAGS" ".*/info/dir" "\\.~.+~" ".*/[0-9]+$"))
      recentf-max-menu-items 100
      recentf-max-saved-items 100
      recentf-menu-filter nil)

(when recentf-save-file
  (recentf-mode t))

(provide 'my-recentf)
