;;; init-sjc.el

(setq user-emacs-directory "/auto/asic-home-sjc/scfrazer/.emacs.d/")
(setq recentf-save-file nil)

(load (concat user-emacs-directory "init.el"))

(setq recentf-save-file
      (convert-standard-filename
       (let ((view (and use-clearcase clearcase-setview-viewtag)))
         (if view
             (concat "/auto/asic-home-sjc/scfrazer/.recentf-" view)
           "/auto/asic-home-sjc/scfrazer/.recentf"))))
(recentf-mode t)

(setq my-cc-mode-ctags-executable "/auto/asic-home-sjc/scfrazer/local/bin/ctags")
