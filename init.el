;; init.el

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode 0))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(setq-default load-prefer-newer nil)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/cc-mode") t)

(setq inhibit-startup-echo-area-message "scfrazer")

(setq-default tramp-default-method "ssh"
              tramp-mode nil)
(defun tramp ()
  "Toggle tramp on/off."
  (interactive)
  (setq tramp-mode (not tramp-mode))
  (message (concat "Tramp mode " (if tramp-mode "on" "off"))))

(defun my-package-menu--refresh (&optional packages keywords)
  (setq tabulated-list-format
        `[("Package" 38 package-menu--name-predicate)
          ("Version" 13 nil)
          ("Status"  10 package-menu--status-predicate)
          ,@(if (cdr package-archives)
                '(("Archive" 10 package-menu--archive-predicate)))
          ("Description" 0 nil)]))
(advice-add #'package-menu--refresh :before #'my-package-menu--refresh)
(setq-default package-quickstart t)

(let ((gc-cons-threshold 402653184)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  (require 'package)
  (setq-default package-check-signature nil)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (require 'my-init))
