;;; vbm.el --- Visible bookmarks
;;; Copyright (C) 2009, Scott Frazer <frazer.scott@gmail.com>

;; vbm.el is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc., 59
;; Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; This is version 1.1 as of 23 Oct 2009

;;; Commentary:
;; TODO This code ...
;; (global-set-key (kbd "<left-fringe> <mouse-1>") 'vbm-mouse-toggle)

(defvar vbm-face-background
  '("gray35" "DodgerBlue4" "DarkOliveGreen" "DarkRed" "magenta4")
  "*Background colors for visible bookmarks.")

(defvar vbm-curr-face-num 0
  "Visible bookmarks current face number.")

(defvar vbm-nav-hit-end nil
  "Visible bookmark navigation hit end of buffer.")

(defvar vbm-nav-hit-start nil
  "Visible bookmark navigation hit start of buffer.")

(defconst vbm-fringe-str "*vbm*"
  "Dummy string to make fringe marker")

(defface vbm-fringe-face
  '((t (:background "gray35" :foreground "white")))
  "Face to highlight vbm fringe markers"
  :group 'faces)

(define-fringe-bitmap 'vbm-marker [#x20 #x30 #x38 #x3C #x3C #x38 #x30 #x20])
(put-text-property 0 (length vbm-fringe-str) 'display (list 'left-fringe 'vbm-marker 'vbm-fringe-face) vbm-fringe-str)

;; Functions

(defun vbm-make-overlay (start end face-num)
  "Make a vbm overlay."
  (let ((ovl (make-overlay start end nil t t)))
    (overlay-put ovl 'before-string vbm-fringe-str)
    (overlay-put ovl 'evaporate t)
    (vbm-set-overlay-face-num ovl face-num)))

(defun vbm-get-overlay-face-num (ovl)
  "Get the vbm face number of an overlay."
  (let ((face (face-name (plist-get (overlay-properties ovl) 'face))))
    (string-match ".+\\([0-9]+\\)$" face)
    (string-to-number (substring face (match-beginning 1) (match-end 1)))))

(defun vbm-set-overlay-face-num (ovl face-num)
  "Set the vbm overlay face."
  (overlay-put ovl 'face (intern (format "vbm-face%d" face-num))))

(defun vbm-overlay-at (pnt)
  "Return vbm visible overlay at PNT if there is one."
  (let ((overlays (overlays-at pnt))
        (ovl nil))
    (dolist (overlay overlays)
      (when (vbm-overlay overlay)
        (setq ovl overlay)))
    ovl))

(defun vbm-overlay (ovl)
  "Is this overlay a vbm visible overlay?"
  (let ((result nil)
        (face (plist-get (overlay-properties ovl) 'face)))
    (when (and face (facep face) (string-match "^vbm" (face-name face)))
      (setq result t))
    result))

(defun vbm-get-overlay-list (start end)
  "Get list of all visible vbm overlays."
  (let ((overlays (overlays-in start end))
        (ovl-list nil))
    (dolist (overlay overlays)
      (when (vbm-overlay overlay)
        (push overlay ovl-list)))
    ovl-list))

(defun vbm-get-both-overlay-lists ()
  "Get lists of visible and invisible vbm overlays."
  (let ((overlays (overlays-in (point-min) (point-max)))
        (visible-ovl-list nil)
        (invisible-ovl-list nil))
    (dolist (overlay overlays)
      (when (vbm-overlay overlay)
        (push overlay visible-ovl-list))
      (when (memq 'vbm-invisible (overlay-properties overlay))
        (push overlay invisible-ovl-list)))
    (list visible-ovl-list invisible-ovl-list)))

(defun vbm-add (&optional face-num)
  "Add visible bookmark."
  (interactive)
  (let ((bol (point-at-bol))
        (eol (point-at-eol))
        (ovl (vbm-overlay-at (point))))
    (when (< bol eol)
      (when ovl
        (delete-overlay ovl))
      (if face-num
          (setq vbm-curr-face-num face-num)
        (setq face-num vbm-curr-face-num))
      (vbm-make-overlay bol eol face-num))))

(defun vbm-remove ()
  "Remove visible bookmark."
  (interactive)
  (let ((ovl (vbm-overlay-at (point-at-bol))))
    (when ovl
      (delete-overlay ovl))))

(defun vbm-toggle ()
  "Toggle visible bookmark."
  (interactive)
  (if (vbm-overlay-at (point-at-bol))
      (vbm-remove)
    (vbm-add)))

(defun vbm-mouse-toggle (ev)
  "Toggle visible bookmark with mouse."
  (interactive "e")
  (save-excursion
    (mouse-set-point ev)
    (vbm-toggle)))

(defun vbm-inc ()
  "Switch to next visible bookmark face."
  (interactive)
  (let ((ovl (vbm-overlay-at (point)))
        (face-num nil))
    (when ovl
      (setq face-num (1+ (vbm-get-overlay-face-num ovl)))
      (when (>= face-num (length vbm-face-background))
        (setq face-num 0))
      (setq vbm-curr-face-num face-num)
      (vbm-set-overlay-face-num ovl face-num))))

(defun vbm-next ()
  "Jump to the next visible bookmark."
  (interactive)
  (let* ((end (point-max))
         (start (min (1+ (point-at-eol)) end))
         (ovl-list (vbm-get-overlay-list start end)))
    (if ovl-list
        (let ((closest-point end))
          (setq vbm-nav-hit-end nil)
          (dolist (ovl ovl-list)
            (setq closest-point (min closest-point (overlay-start ovl))))
          (goto-char closest-point)
          (recenter))
      (if vbm-nav-hit-end
          (progn
            (setq vbm-nav-hit-end nil)
            (setq ovl-list (vbm-get-overlay-list (point-min) end))
            (if (not ovl-list)
                (message "No visible bookmarks in buffer.")
              (goto-char (point-min))
              (vbm-next)))
        (setq vbm-nav-hit-end 't)
        (message "Reached end of buffer.")))))

(defun vbm-prev ()
  "Jump to the previous visible bookmark."
  (interactive)
  (let* ((start (point-min))
         (end (if (>= (1- (point-at-bol)) (point-min))
                        (1- (point-at-bol))
                      (point-max)))
         (ovl-list (vbm-get-overlay-list start end)))
    (if ovl-list
        (let ((closest-point start))
          (dolist (ovl ovl-list)
            (setq closest-point (max closest-point (overlay-start ovl))))
          (goto-char closest-point)
          (recenter))
      (if vbm-nav-hit-start
          (progn
            (setq vbm-nav-hit-start nil)
            (setq ovl-list (vbm-get-overlay-list start (point-max)))
            (if (not ovl-list)
                (message "No visible bookmarks in buffer.")
              (goto-char (point-max))
              (vbm-prev)))
        (setq vbm-nav-hit-start 't)
        (message "Reached start of buffer.")))))

(defun vbm-only-toggle ()
  "Toggle showing only visible bookmarked lines."
  (interactive)
  (let* ((ovl-lists (vbm-get-both-overlay-lists))
         (visible-ovl-list (car ovl-lists))
         (invisible-ovl-list (cadr ovl-lists)))
    (if invisible-ovl-list
        (dolist (ovl invisible-ovl-list)
          (delete-overlay ovl))
      (if (not visible-ovl-list)
          (message "No visible bookmarks in buffer.")
        (setq visible-ovl-list (sort visible-ovl-list '(lambda (x y) (< (overlay-start x) (overlay-start y)))))
        (let ((start (point-min)) end)
          (dolist (ovl visible-ovl-list)
            (setq end (overlay-start ovl))
            (vbm-make-invisible-overlay start end)
            (setq start (overlay-end ovl))
            (unless (= start (point-max))
              (setq start (1+ start))))
          (setq end (point-max))
          (vbm-make-invisible-overlay start end))))))

(defun vbm-make-invisible-overlay (start end)
  "Make an overlay with invisible/intangible properties."
  (let (new-ovl)
    (when (> end start)
      (setq new-ovl (make-overlay start end nil t nil))
      (overlay-put new-ovl 'invisible t)
      (overlay-put new-ovl 'intangible t)
      (overlay-put new-ovl 'vbm-invisible t))))

;; Build vbm faces

(let ((face-num 0))
  (dolist (bgnd vbm-face-background)
    (let ((face (make-face (intern (format "vbm-face%d" face-num))
                           (format "Bookmark face %d" face-num))))
      (set-face-background face bgnd)
      (setq face-num (1+ face-num)))))

(provide 'vbm)

;;; vbm.el ends here
