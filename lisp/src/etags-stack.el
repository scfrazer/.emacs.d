;;; etags-stack.el --- Navigate the tags stack

;; Copyright (C) 2008-2010  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 14 Aug 2008
;; Version: 1.1
;; Keywords: tags

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Navigate the tags stack.  A small window opens showing the current
;; stack of tag lines you have jumped to.  Move to a line and press RET
;; to pop locations off until you get to the selected one, or press 'q'
;; to quit.

;;; Code:

(require 'custom)
(require 'etags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom stuff

;;;###autoload
(defgroup etags-stack nil
  "*etags stack."
  :group 'etags)

;;;###autoload
(defcustom etags-stack-mode-hook nil
  "*List of functions to call on entry to etags-stack-mode mode."
  :group 'etags-stack
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar etags-stack-buffer-name "*etags-stack*"
  "Buffer name.")

(defvar etags-stack-mode-font-lock-keywords
  '(
    ("<<current buffer>>"
     (0 'font-lock-comment-face))
    ("^\\(.+?\\):\\(.+?\\):"
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face))
    )
  "Font-lock-keywords.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

;;;###autoload
(defun etags-stack-show ()
  "Show tags stack."
  (interactive)
  (get-buffer-create etags-stack-buffer-name)
  (set-buffer etags-stack-buffer-name)
  (setq buffer-read-only nil)
  (erase-buffer)
  (mapc 'etags-stack-insert-from-marker (reverse (ring-elements find-tag-marker-ring)))
  (insert "<<current buffer>>")
  (goto-char (point-at-bol))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (select-window (split-window-vertically))
  (switch-to-buffer etags-stack-buffer-name)
  (fit-window-to-buffer)
  (etags-stack-mode))

(defun etags-stack-insert-from-marker (marker)
  "Insert tag contents from marker."
  (let ((buf (marker-buffer marker))
        (pos (marker-position marker))
        line-num line-text)
    (when buf
      (save-excursion
        (set-buffer buf)
        (goto-char pos)
        (setq line-num (int-to-string (count-lines (point-min) pos)))
        (setq line-text (buffer-substring (point-at-bol) (point-at-eol))))
      (insert (buffer-name buf) ":" line-num ":" line-text "\n"))))

(defun etags-stack-go ()
  "Go to tag on current line."
  (interactive)
  (let ((line-num (count-lines (point-min) (point-at-bol)))
        stack-offset pop-arg)
    (goto-char (point-min))
    (re-search-forward "^<<current buffer>>")
    (setq stack-offset (- line-num (count-lines (point-min) (point-at-bol))))
    (etags-stack-quit)
    (setq pop-arg (< 0 stack-offset))
    (setq stack-offset (abs stack-offset))
    (while (> stack-offset 0)
      (pop-tag-mark)
      (setq stack-offset (1- stack-offset)))))

(defun etags-stack-quit ()
  "Quit etags-stack buffer."
  (interactive)
  (kill-buffer nil)
  (delete-window))

(defun etags-stack-clear()
  "Clear tag stack."
  (interactive)
  (setq find-tag-marker-ring (make-ring find-tag-marker-ring-length)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymap

(defvar etags-stack-mode-map nil "'etags-stack-mode' keymap.")
(if (not etags-stack-mode-map)
    (let ((map (make-keymap)))
      (define-key map [(return)] 'etags-stack-go)
      (define-key map "q" 'etags-stack-quit)
      (setq etags-stack-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode startup

(defun etags-stack-mode ()
  "etags-stack-mode is a mode for navigating the tags stack.\n\n
\\{etags-stack-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'etags-stack-mode)
  (setq mode-name "etags-stack")
  (set-syntax-table text-mode-syntax-table)
  (use-local-map etags-stack-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(etags-stack-mode-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'etags-stack-mode-hook))

(provide 'etags-stack)
;;; etags-stack.el ends here
