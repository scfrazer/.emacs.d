;;; init-bgl.el

(setq user-emacs-directory "/auto/sse-dump-blr/scfrazer/.emacs.d/")
(setq recentf-save-file nil)

(load (concat user-emacs-directory "init.el"))

(setq recentf-save-file
      (convert-standard-filename
       (let ((view (and use-clearcase clearcase-setview-viewtag)))
         (if view
             (concat "/auto/sse-dump-blr/scfrazer/.recentf-" view)
           "/auto/sse-dump-blr/scfrazer/.recentf"))))
(recentf-mode t)

(setq my-cc-mode-ctags-executable "/auto/sse-dump-blr/scfrazer/local/bin/ctags")
