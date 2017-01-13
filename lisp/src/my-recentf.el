;;; my-recentf.el

(require 'recentf)
(require 'midnight)

(add-hook 'midnight-hook 'recentf-cleanup)
(remove-hook 'midnight-hook 'clean-buffer-list)

(require 'my-clearcase)
(defvar clearcase-setview-viewtag)

(when recentf-save-file
  (setq recentf-save-file
        (convert-standard-filename
         (let ((view (and use-clearcase clearcase-setview-viewtag)))
           (if view
               (concat "~/.recentf-" view)
             "~/.recentf")))))

(setq recentf-auto-cleanup "11:59pm"
      recentf-exclude (quote ("TAGS" ".*/info/dir" "\\.~.+~" ".*/[0-9]+$"))
      recentf-max-menu-items 100
      recentf-max-saved-items 100
      recentf-menu-filter nil)

(when recentf-save-file
  (recentf-mode t))

(provide 'my-recentf)
