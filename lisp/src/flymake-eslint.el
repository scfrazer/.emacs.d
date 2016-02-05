;;; flymake-eslint.el --- A flymake handler for javascript files
;;; linted with [eslint](eslint.org)
;;
;;; Author: Travis Jefferson
;;; URL: https://github.com/tjefferson08/flymake-eslint
;;; Version: DEV
;;; Package-Requires: ((flymake-easy "0.1"))
;;;
;;; Commentary:
;;  Usage:
;;   (require 'flymake-eslint)
;;   (add-hook 'js-mode-hook 'flymake-eslint-load)
;;
;; Uses flymake-easy, from https://github.com/purcell/flymake-easy
;;; Code:

(require 'flymake-easy)

(defconst flymake-eslint-err-line-patterns
  '(("^[ ]*\\([0-9]+\\):\\([0-9]+\\)[ ]*\\(.*\\)$" nil 1 2 3))) ;; default eslint reporter format

(defvar flymake-eslint-executable "eslint"
  "The eslint executable to use for syntax checking.")

;; TODO add some options for eslint CLI
(defun flymake-eslint-command (filename)
  "Construct a command that flymake can use to run eslint on a file."
  (list flymake-eslint-executable filename))

;;;###autoload
(defun flymake-eslint-load ()
  "Configure flymake mode to check the current buffer's Javascript syntax."
  (interactive)
  (flymake-easy-load 'flymake-eslint-command
                     flymake-eslint-err-line-patterns

                     ;; this did not work with the 'tempdir, apparently eslint
                     ;; got lost and couldn't find its .eslintrc files
                     'inplace
                     "js"))

(provide 'flymake-eslint)
;;; flymake-eslint.el ends here
