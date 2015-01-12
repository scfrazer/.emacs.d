;; init.el

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode 0))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/org"))

(require 'package)
(package-initialize)

(require 'use-package)
(setq use-package-verbose t)

(require 'my-init)
