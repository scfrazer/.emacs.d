;;; grep-buffers.el --- grep through buffers (a la 'moccur')

;; Copyright (C) 2007-2010  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 01 Jan 2007
;; Version: 2.2
;; Keywords: grep

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

;; This code lets you grep through all loaded buffers that have a file
;; associated with them.  It's similar to 'moccur' and it's many variants, but
;; uses the standard compilation-mode interface, i.e. next-error,
;; previous-error, etc. all work.

;;; Code:

(defvar grep-buffers-buffer-name "*grep-buffers*"
  "grep-buffers buffer name.")

(defvar grep-buffers-name-regexps-to-ignore (list "TAGS$")
  "*Name regexps to ignore")

(defvar grep-buffers-regexp-history nil
  "Regexp history for grep-buffers.")

(defvar grep-buffers-match-index -1
  "Current match index.")

;; Grep buffers

;;;###autoload
(defun grep-buffers ()
  "Grep buffers that have file names associated with them."
  (interactive)
  (let ((buffers (sort (buffer-list)
                       (function (lambda (elt1 elt2)
                                   (string< (downcase (buffer-name elt1))
                                            (downcase (buffer-name elt2)))))))
        (regexp (grep-buffers-symbol-at-point)))
    (setq regexp (read-string (format "grep buffers for [%s]: " regexp)
                              nil 'grep-buffers-regexp-history regexp))
    (add-to-list 'grep-buffers-regexp-history regexp)
    (get-buffer-create grep-buffers-buffer-name)
    (save-excursion
      (set-buffer grep-buffers-buffer-name)
      (erase-buffer)
      (display-buffer grep-buffers-buffer-name)
      (insert (format "grep buffers for '%s' ...\n\n" regexp))
      (mapc (lambda (x)
              (when (grep-buffers-do-this-one x)
                (set-buffer x)
                (save-excursion
                  (save-match-data
                    (goto-char (point-min))
                    (while (re-search-forward regexp nil t)
                      (let ((line (count-lines 1 (point)))
                            (substr (buffer-substring (point-at-bol)
                                                      (point-at-eol))))
                        (save-excursion
                          (set-buffer grep-buffers-buffer-name)
                          (insert (format "%s:%d:%s\n" x line substr))))
                      (goto-char (point-at-eol)))))))
            buffers)
      (set-buffer grep-buffers-buffer-name)
      (goto-char (point-max))
      (insert "\ngrep finished\n")
      (grep-mode)
      (grep-buffers-parse-matches nil nil)
      (setq grep-buffers-match-index -1)
      (setq overlay-arrow-position nil)
      (setq next-error-function 'grep-buffers-next-match)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only nil)
      (force-mode-line-update))))

(defun grep-buffers-symbol-at-point ()
  (save-excursion
    (buffer-substring (progn (skip-syntax-backward "w_") (point))
                      (progn (skip-syntax-forward "w_") (point)))))

(defun grep-buffers-do-this-one (buf)
  (let ((name (buffer-file-name buf))
        (case-fold-search nil)
        match)
  (when name
    (mapc
     (lambda (x) (setq match (or match (string-match x name))))
     grep-buffers-name-regexps-to-ignore)
    (not match))))

;; Parse matches

(defun grep-buffers-parse-matches (limit-search find-at-least)
  "Parse the grep buffer for matches.
See variable `compilation-parse-errors-function' for interface."
  (save-excursion
    (set-buffer grep-buffers-buffer-name)
    (goto-char (point-min))
    (setq compilation-error-list nil)
    (while (re-search-forward "^\\(.+?\\):\\([0-9]+?\\):\\(.+?\\)$" nil t)
      (let ((buffer-of-match (match-string 1))
            (line-of-match (string-to-number (match-string 2))))
        (setq compilation-error-list
              (nconc compilation-error-list
                     (list (cons
                            (save-excursion
                              (beginning-of-line)
                              (point-marker))
                            (save-excursion
                              (set-buffer buffer-of-match)
                              (goto-char (point-min))
                              (forward-line (1- line-of-match))
                              (beginning-of-line)
                              (point-marker))))))))))

(provide 'grep-buffers)
;;; grep-buffers.el ends here
