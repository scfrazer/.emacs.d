;;; jump-to-prev-pos.el --- Jump to previous position

;; Copyright (C) 2007  Scott Frazer

;; Author: Scott Frazer <scfrazer@lx28-vnc-010.cisco.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; Automatically keep a stack of previous positions to jump back to in case
;; you fat-finger a motion key.

;;; Code:

(defgroup jtpp nil
  "Jump to previous position."
  :group 'convenience)

(defcustom jtpp-ignore-commands (list 'jump-to-prev-pos
                                      'self-insert-command
                                      'forward-char
                                      'backward-char
                                      'next-line
                                      'previous-line)
  "Commands for jtpp to ignore when saving positions."
  :type '(repeat symbol)
  :group 'jtpp)

(defvar jtpp-prev-pos-stack nil
  "Stack of previous positions.")
(make-variable-buffer-local 'jtpp-prev-pos-stack)

(defvar jtpp-next-pos-stack nil
  "Stack of next positions.")
(make-variable-buffer-local 'jtpp-next-pos-stack)

(defvar jtpp-stack-depth 16
  "*Stack depth for jump-to-prev-pos.")

(defun jtpp-remember-position ()
  "Remember current position."
  (unless (or (equal (point) (car jtpp-prev-pos-stack))
              (member this-command jtpp-ignore-commands))
    (setq jtpp-prev-pos-stack (cons (point) jtpp-prev-pos-stack))
    (when (> (length jtpp-prev-pos-stack) jtpp-stack-depth)
      (nbutlast jtpp-prev-pos-stack))
    (when (> (length jtpp-next-pos-stack) jtpp-stack-depth)
      (nbutlast jtpp-next-pos-stack))))

(add-hook 'pre-command-hook 'jtpp-remember-position)

(defun jump-to-prev-pos (&optional arg)
  "Jump to previous saved position.  With ARG, jump to next saved
position."
  (interactive "P")
  (if arg
      (when jtpp-next-pos-stack
        (setq jtpp-prev-pos-stack (cons (point) jtpp-prev-pos-stack))
        (while (and jtpp-next-pos-stack
                    (equal (point) (car jtpp-next-pos-stack)))
          (setq jtpp-next-pos-stack (cdr jtpp-next-pos-stack)))
        (when jtpp-next-pos-stack
          (goto-char (car jtpp-next-pos-stack))
          (setq jtpp-next-pos-stack (cdr jtpp-next-pos-stack))))
    (when jtpp-prev-pos-stack
      (setq jtpp-next-pos-stack (cons (point) jtpp-next-pos-stack))
      (while (and jtpp-prev-pos-stack
                  (equal (point) (car jtpp-prev-pos-stack)))
        (setq jtpp-prev-pos-stack (cdr jtpp-prev-pos-stack)))
      (when jtpp-prev-pos-stack
        (goto-char (car jtpp-prev-pos-stack))
        (setq jtpp-prev-pos-stack (cdr jtpp-prev-pos-stack))))))

(provide 'jump-to-prev-pos)
;;; jump-to-prev-pos.el ends here
