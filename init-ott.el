;;; init-sjc.el

(setq user-emacs-directory "/auto/kan-dump3/scfrazer/.emacs.d/")
(load (concat user-emacs-directory "init.el"))

(setq my-cc-mode-ctags-executable "/auto/kan-dump3/scfrazer/local/bin/ctags")

(setq recentf-save-file
      (convert-standard-filename
       (let ((view (and use-clearcase clearcase-setview-viewtag)))
         (if view
             (concat "/auto/kan-dump3/scfrazer/.recentf-" view)
           "/auto/kan-dump3/scfrazer/.recentf"))))
