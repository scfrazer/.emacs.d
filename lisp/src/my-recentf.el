;;; my-recentf.el

(require 'recentf)
(require 'clearcase)

(setq recentf-save-file
      (convert-standard-filename
       (let ((view (and clearcase-servers-online clearcase-setview-viewtag)))
         (if view
             (concat "~/.recentf-" view)
           "~/.recentf"))))

(setq recentf-exclude (quote ("TAGS" ".*/info/dir" "\\.~.+~" ".*/[0-9]+$")))
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(setq recentf-menu-filter nil)
(setq recentf-auto-cleanup "11:59pm")

(defun my-recentf-clear-list ()
  (interactive)
  (setq recentf-list nil))

(recentf-mode t)

(provide 'my-recentf)
