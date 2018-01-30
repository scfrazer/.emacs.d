;; init.el

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode 0))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(setq inhibit-startup-echo-area-message "scfrazer")

(setq-default tramp-default-method "ssh"
              tramp-mode nil)
(defun tramp ()
  "Toggle tramp on/off."
  (interactive)
  (setq tramp-mode (not tramp-mode))
  (message (concat "Tramp mode " (if tramp-mode "on" "off"))))

(let ((gc-cons-threshold 402653184)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  (require 'package)
  (package-initialize)
  (require 'my-init))
