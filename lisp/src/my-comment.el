;;; my-comment.el --- Comment lines my way

;; Copyright (C) 2007  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Keywords: comment

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

;; Comment lines my way.

;;; Code:

;; Strip whitespace

(defun my-comment-stripped-comment-start ()
  "comment-start stripped of whitespace."
  (replace-regexp-in-string "\\s-+$" "" comment-start))

(defun my-comment-stripped-comment-end ()
  "comment-end stripped of whitespace."
  (replace-regexp-in-string "^\\s-+" ""  comment-end))

;; Comment current line

(defun my-comment-current-line (&optional arg)
  "Comment current line."
  (save-excursion
    (beginning-of-line)
    (unless arg (setq arg 1))
    (while (> arg 0)
      (insert (my-comment-stripped-comment-start))
      (setq arg (1- arg)))
    (unless (looking-at "\\s-*$")
      (insert " "))
    (when (> (length (my-comment-stripped-comment-end)) 0)
      (end-of-line)
      (insert " " (my-comment-stripped-comment-end)))))

;; Uncomment current line

(defun my-uncomment-current-line ()
  "Uncomment current line."
  (save-excursion
    (save-match-data
      (back-to-indentation)
      (while (looking-at (my-comment-stripped-comment-start))
        (replace-match ""))
      (when (> (length (my-comment-stripped-comment-end)) 0)
        (end-of-line)
        (while (looking-back (concat (my-comment-stripped-comment-end) "\\s-*"))
          (replace-match "")
          (end-of-line))
        (when (looking-back "\\s-+")
          (replace-match "")))
      (beginning-of-line)
      (unless (looking-at "^\\s-*$")
        (indent-according-to-mode)))))

;; Toggle comment on current line

(defun my-toggle-comment-current-line (&optional arg)
  "Toggle comment on current line."
  (save-excursion
    (back-to-indentation)
    (if (looking-at (my-comment-stripped-comment-start))
        (progn
          (end-of-line)
          (if (> (length (my-comment-stripped-comment-end)) 0)
              (if (looking-back (concat (my-comment-stripped-comment-end) "\\s-*"))
                  (my-uncomment-current-line)
                (my-comment-current-line arg))
            (my-uncomment-current-line)))
      (my-comment-current-line arg))))

;; Act on region

(defun my-comment-apply-on-region-lines (func &optional arg)
  "Apply function on region lines."
  (if (not (region-active-p))
      (funcall func arg)
    (save-excursion
      (let ((last-line (line-number-at-pos (region-end))))
        (when (bolp)
          (setq last-line (1- last-line)))
        (goto-char (region-beginning))
        (while (and (<= (line-number-at-pos (point)) last-line)
                    (not (eobp)))
          (funcall func arg)
          (forward-line))))))

;; Comment region (or line)

(defun my-comment-region (&optional arg)
  "Comment region if it is active, line if it is not."
  (interactive "*p")
  (my-comment-apply-on-region-lines 'my-comment-current-line arg))

;; Uncomment region (or line)

(defun my-uncomment-region ()
  "Uncomment region if it is active, line if it is not."
  (interactive "*")
  (my-comment-apply-on-region-lines 'my-uncomment-current-line))

;; Toggle comments in region (or line)

(defun my-comment-region-toggle (&optional arg)
  "Toogle comments in region if it is active, line if it is not."
  (interactive "*p")
  (my-comment-apply-on-region-lines 'my-toggle-comment-current-line arg))

;; Copy region and comment original

(defun my-comment-region-after-copy (&optional arg)
  "Copy region and comment original."
  (interactive "*p")
  (if (region-active-p)
      (progn
        (copy-region-as-kill (region-beginning) (region-end))
        (my-comment-region arg)
        (yank)
        (goto-char (region-beginning)))
    (copy-region-as-kill (point-at-bol) (point-at-eol))
    (my-comment-current-line arg)
    (end-of-line)
    (newline)
    (yank)
    (beginning-of-line)))

(provide 'my-comment)
;;; my-comment.el ends here
