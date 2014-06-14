;;; yank-target.el --- Yank or kill text to another location

;; Copyright (C) 2010  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 01 Jan 2010
;; Version: 1.0
;; Keywords: convenience

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
;;
;; Yank or kill text to another location in the current or a different
;; buffer.  This is handy for moving chunks of text around to a specific
;; location, e.g. when refactoring code.
;;
;; If transient-mark mode is enabled and a region is highlighted, the
;; region will be acted upon, otherwise the current line is yanked or
;; killed.  If transient-mark mode is disabled, point and mark set the
;; region to use.
;;
;; Add this to your .emacs to set "C-c y" as the prefix key:
;;
;; (global-set-key (kbd "C-c y") 'yank-target-map)
;;
;; These bindings are in the prefix map:
;;
;; SPC -- Set the target to yank/kill to
;; y   -- Yank to target and stay here
;; Y   -- Yank to target and go there
;; k   -- Kill to target and stay here
;; K   -- Kill to target and go there
;; t   -- Go to target location
;; s   -- Go to source location, i.e. where you just were

;;; Code:

(defvar yank-target-marker (make-marker))
(defvar yank-target-source (make-marker))

;;;###autoload
(defun yank-target-set ()
  "Set yank target point."
  (interactive)
  (setq yank-target-marker (point-marker))
  (set-marker-insertion-type yank-target-marker t)
  (message "Yank target set"))

;;;###autoload
(defun yank-target-yank (&optional kill)
  "Yank to target."
  (interactive)
  (if (or (not (marker-buffer yank-target-marker))
          (not (marker-position yank-target-marker)))
      (error "No yank target")
    (let (start end text)
      (if transient-mark-mode
          (if mark-active
              (setq start (region-beginning)
                    end (region-end))
            (setq start (point-at-bol)
                  end (point-at-bol 2)))
        (setq start (region-beginning)
              end (region-end)))
      (setq text (buffer-substring start end))
      (when kill
        (delete-region start end))
      (save-excursion
        (with-current-buffer (marker-buffer yank-target-marker)
          (goto-char (marker-position yank-target-marker))
          (setq start (point))
          (insert text)))
      (setq yank-target-source (point-marker))
      (if transient-mark-mode
          (if (region-active-p)
              (progn (deactivate-mark)
                     (message "Sent region to target"))
            (message "Sent line to target"))
        (message "Sent region to target")))))

;;;###autoload
(defun yank-target-kill ()
  "Kill to target."
  (interactive)
  (yank-target-yank 'kill))

;;;###autoload
(defun yank-target-go-target ()
  "Go to yank target."
  (interactive)
  (if (or (not (marker-buffer yank-target-marker))
          (not (marker-position yank-target-marker)))
      (error "Yank target buffer killed or position no longer exists")
    (setq yank-target-source (point-marker))
    (switch-to-buffer (marker-buffer yank-target-marker))
    (goto-char (marker-position yank-target-marker))))

;;;###autoload
(defun yank-target-yank-and-go ()
  "Yank to target and go there."
  (interactive)
  (yank-target-yank)
  (yank-target-go-target))

;;;###autoload
(defun yank-target-kill-and-go ()
  "Kill to target and go there."
  (interactive)
  (yank-target-kill)
  (yank-target-go-target))

;;;###autoload
(defun yank-target-go-source ()
  "Go to yank target source."
  (interactive)
  (if (or (not (marker-buffer yank-target-source))
          (not (marker-position yank-target-source)))
      (error "Yank source buffer killed or position no longer exists")
    (switch-to-buffer (marker-buffer yank-target-source))
    (goto-char (marker-position yank-target-source))))

(define-prefix-command 'yank-target-map)
(define-key yank-target-map (kbd "SPC") 'yank-target-set)
(define-key yank-target-map (kbd "y") 'yank-target-yank)
(define-key yank-target-map (kbd "Y") 'yank-target-yank-and-go)
(define-key yank-target-map (kbd "k") 'yank-target-kill)
(define-key yank-target-map (kbd "K") 'yank-target-kill-and-go)
(define-key yank-target-map (kbd "t") 'yank-target-go-target)
(define-key yank-target-map (kbd "s") 'yank-target-go-source)

(provide 'yank-target)
;;; yank-target.el ends here
