;;; init-sjc.el

(setq user-emacs-directory "/auto/cppfs2e/scfrazer/.emacs.d/")
(load (concat user-emacs-directory "init.el"))

(setq my-cc-mode-ctags-executable "/auto/cppfs2e/scfrazer/local/bin/ctags")

(setq recentf-save-file
      (convert-standard-filename
       (let ((view (and use-clearcase clearcase-setview-viewtag)))
         (if view
             (concat "/auto/cppfs2e/scfrazer/.recentf-" view)
           "/auto/cppfs2e/scfrazer/.recentf"))))
