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

 ;; Compact eslint reporter format
(defconst flymake-eslint-err-line-patterns
  '(("^\\([^:]+\\): line \\([0-9]+\\), col \\([0-9]+\\), \\(.+\\)$" 1 2 3 4)))

(defvar flymake-eslint-executable "eslint"
  "The eslint executable to use for syntax checking.")

(defvar flymake-eslint-option-list (list "--no-color")
  "List of eslint options.")

(defun flymake-eslint-command (filename)
  "Construct a command that flymake can use to run eslint on a file."
  (append (list flymake-eslint-executable "--format" "compact") flymake-eslint-option-list (list filename)))

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
