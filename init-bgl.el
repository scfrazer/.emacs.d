;;; init-bgl.el

(setq user-emacs-directory "/auto/sse-dump-blr/scfrazer/.emacs.d/")
(load (concat user-emacs-directory "init.el"))

(setq my-cc-mode-ctags-executable "/auto/sse-dump-blr/scfrazer/local/bin/ctags")

(setq recentf-save-file
      (convert-standard-filename
       (let ((view (and use-clearcase clearcase-setview-viewtag)))
         (if view
             (concat "/auto/sse-dump-blr/scfrazer/.recentf-" view)
           "/auto/sse-dump-blr/scfrazer/.recentf"))))
