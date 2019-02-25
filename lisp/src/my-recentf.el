;;; my-recentf.el

(require 'recentf)
(require 'midnight)
(require 'subr-x)

(add-hook 'midnight-hook 'recentf-cleanup)
(remove-hook 'midnight-hook 'clean-buffer-list)

;; (require 'my-clearcase)
;; (defvar clearcase-setview-viewtag)

(when (and recentf-save-file (executable-find "p4"))
  (setq recentf-save-file
        (convert-standard-filename
         (let ((ws (string-trim-right (shell-command-to-string "p4 -F %clientName% -ztag info"))))
           (if (not (string= ws "*unknown*"))
               (concat "~/.recentf-" ws)
             "~/.recentf")))))

(setq recentf-auto-cleanup "11:59pm"
      recentf-exclude (quote ("TAGS" ".*/info/dir" "\\.~.+~" ".*/[0-9]+$"))
      recentf-max-menu-items 100
      recentf-max-saved-items 100
      recentf-menu-filter nil)

(when recentf-save-file
  (recentf-mode t))

(provide 'my-recentf)
