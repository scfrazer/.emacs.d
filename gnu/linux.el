;; X vs. terminal
(when window-system
  (normal-erase-is-backspace-mode 1))

;; Work stuff
;; (when (string= (getenv "SHELL") "/bin/tcsh")
;;
;;   ;; ClearCase VOBs can have problems listing space
;;
;;   (defvar ls-vob "~/.emacs.d/bin/ls-vob")
;;   (when (file-exists-p ls-vob)
;;     (setq dired-free-space-program nil)
;;     (setq insert-directory-program ls-vob)))

;; Theme
(my-theme-light)
