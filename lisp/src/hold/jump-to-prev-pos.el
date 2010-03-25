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

(defvar jtpp-prev-pos-stack nil
  "Stack of previous positions.")
(make-variable-buffer-local 'jtpp-prev-pos-stack)

(defvar jtpp-next-pos-stack nil
  "Stack of next positions.")
(make-variable-buffer-local 'jtpp-next-pos-stack)

(defvar jtpp-stack-depth 16
  "*Stack depth for jump-to-prev-pos.")

(defvar jtpp-ignore-commands (list 'self-insert-command
                                   'newline
                                   'newline-and-indent
                                   'jump-to-prev-pos
                                   'jump-to-next-pos
                                   'next-line
                                   'previous-line
                                   'forward-char
                                   'backward-char
                                   'pager-row-up
                                   'pager-row-down)
  "*Default list of commands to ignore when recording point")

(defun jtpp-ignore-movement ()
  (equal (point) (car jtpp-prev-pos-stack)))

(defun jtpp-remember-position ()
  (unless (or (jtpp-ignore-movement)
              (member this-command jtpp-ignore-commands))
    (setq jtpp-next-pos-stack nil)
    (setq jtpp-prev-pos-stack (cons (point) jtpp-prev-pos-stack))
    (when (> (length jtpp-prev-pos-stack) jtpp-stack-depth)
      (nbutlast jtpp-prev-pos-stack))))
(add-hook 'pre-command-hook 'jtpp-remember-position)

(defun jump-to-prev-pos ()
  "Jump to previous position."
  (interactive)
  (when jtpp-prev-pos-stack
    (goto-char (car jtpp-prev-pos-stack))
    (setq jtpp-prev-pos-stack (cdr jtpp-prev-pos-stack))
    (setq jtpp-next-pos-stack (cons (point) jtpp-next-pos-stack))))

(defun jump-to-next-pos ()
  "Jump to next position."
  (interactive)
  (when jtpp-next-pos-stack
    (goto-char (car jtpp-next-pos-stack))
    (setq jtpp-next-pos-stack (cdr jtpp-next-pos-stack))
    (setq jtpp-prev-pos-stack (cons (point) jtpp-prev-pos-stack))))

(provide 'jump-to-prev-pos)
;;; jump-to-prev-pos.el ends here
