;; Fix backspace/delete

(normal-erase-is-backspace-mode 1)

;; ClearCase VOBs can have problems listing space

(defvar ls-vob "~/.emacs.d/bin/ls-vob")
(when (file-exists-p ls-vob)
  (setq dired-free-space-program nil)
  (setq insert-directory-program ls-vob))

;; Use a modern grep!

(setq igrep-program "/bin/egrep")

;; Better font menu

;;(global-set-key [(shift down-mouse-1)] 'alt-mouse-set-font)
