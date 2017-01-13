;;; init-sjc.el

(setq user-emacs-directory "/auto/cppfs3a/scfrazer/.emacs.d/")
(setq recentf-save-file nil)

(load (concat user-emacs-directory "init.el"))

(setq recentf-save-file
      (convert-standard-filename
       (let ((view (and use-clearcase clearcase-setview-viewtag)))
         (if view
             (concat "/auto/cppfs3a/scfrazer/.recentf-" view)
           "/auto/cppfs3a/scfrazer/.recentf"))))
(recentf-mode t)

(setq my-cc-mode-ctags-executable "/auto/cppfs3a/scfrazer/local/bin/ctags")
