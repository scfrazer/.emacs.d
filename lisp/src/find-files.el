;;; find-files.el -- handle globing or regexp patterns for find-file
;;
;; Copyright (C) 2000 by Robert Fenk
;;
;; Author:	Robert.Fenk@gmx.de
;; Status:	Tested with XEmacs 21.1.8 and Emacs 20.3
;; Keywords:	find-file, convenience
;; X-URL:       http://www.forwiss.tu-muenchen.de/~fenk/public/prog/
;;
;; $Id: find-files.el,v 1.1 2004/05/12 12:26:18 sfrazer Exp $
;;
;; History:
;;    Revision	    Comment
;;	1.1	First public release
;;	1.2	Several bugfixes in the creation of the "glob pattern"
;;	1.3	Support for GNU Emacs / fix handling for no match
;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; To use these functions you should put this file into your load-path
;; and add the following line to your .emacs file:
;;    (require 'find-files)
;; and you may rebind "C-x C-f" by adding 
;;   (global-set-key [(control x) (control f)] 'find-files-glob)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar find-files-ignore-no-match t
  "*Set this to nil it you want the find-files-* functions to complain
about no matching files.")

;; Emacs 20.3 seems to miss the function replace-in-string?
(if (not (functionp 'replace-in-string))
    ;; actually this is dired-replace-in-string slightly modified 
    (defun replace-in-string (string regexp newtext &optional literal)
      ;; Replace REGEXP with NEWTEXT everywhere in STRING and return result.
      ;; NEWTEXT is taken literally---no \\DIGIT escapes will be recognized.
      (let ((result "") (start 0) mb me)
	(while (string-match regexp string start)
	  (setq mb (match-beginning 0)
		me (match-end 0)
		result (concat result (substring string start mb) newtext)
		start me))
	(concat result (substring string start)))))

(defun find-files-glob-internal (filepattern codesys find-file-function)
  (let ((directory (file-name-directory filepattern))
	(fp (file-name-nondirectory filepattern))
	filename strlist
	(files nil))
    ;; create a regexp which acts like a glob pattern 
    (setq fp (replace-in-string fp "\\." "\\." t))
    (setq fp (replace-in-string fp "\\?" "." t))
    (if (string-match "^\\*" fp) (setq fp (concat "[^.]" fp)))
    (setq fp (replace-in-string fp "\\*" ".*" t))
    (setq fp (replace-in-string fp "\\+" "\\+" t))
    (if (string-match "{\\([^}]+\\)}" fp)
	(progn
	  (setq strlist (substring fp (match-beginning 1) (match-end 1)))
	  (setq strlist (replace-in-string strlist "," "\\|" t))
	  (string-match "{\\([^}]+\\)}" fp)
	  (setq fp (replace-match (concat "\\(" strlist "\\)")
				  t t fp))))
    (setq fp (concat "^" fp "$"))
    
    ;; now get all matching files 
    (setq files (directory-files directory nil fp nil))
    (if (not files)
	(if find-files-ignore-no-match
	    (funcall find-file-function filepattern codesys)
	  (error "No matching files for `%s'!" filepattern)))
    (while files
      (setq filename (car files))
      (if directory (setq filename (concat directory filename)))
      (if (not (file-directory-p filename))
	  (progn (message "Reading %s" filename)
		 (if (string-match "XEmacs" emacs-version)
		     (funcall find-file-function filename codesys)
		   (funcall find-file-function filename))))
      (setq files (cdr files)))))

(defun find-files-glob (filepattern &optional codesys)
  "Edit files matching glob pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-glob-internal filepattern codesys 'find-file))

(defun find-files-glob-other-frame (filepattern &optional codesys)
  "Edit files matching glob pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-glob-internal filepattern codesys 'find-file-other-frame))

(defun find-files-glob-other-window (filepattern &optional codesys)
  "Edit files matching glob pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-glob-internal filepattern codesys 'find-file-other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-files-regexp-internal (filepattern codesys find-file-function)
  (let ((directory (file-name-directory filepattern))
	(fp (file-name-nondirectory filepattern))
	filename
	(files nil))
    (setq files (directory-files directory nil fp nil))
    (if (not files)
	(if find-files-ignore-no-match
	    (funcall find-file-function filepattern codesys)
	  (error "No matching files for `%s'!" filepattern)))
    (while files
      (setq filename (car files))
      (if directory (setq filename (concat directory filename)))
      (if (not (file-directory-p filename))
	  (progn (message "Reading %s" filename)
		 (if (string-match "XEmacs" emacs-version)
		     (funcall find-file-function filename codesys)
		   (funcall find-file-function filename))))
      (setq files (cdr files)))))

(defun find-files-regexp (filepattern &optional codesys)
  "Edit files matching regexp pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-regexp-internal filepattern codesys 'find-file))

(defun find-files-regexp-other-frame (filepattern &optional codesys)
  "Edit files matching regexp pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-regexp-internal filepattern codesys 'find-file-other-frame))

(defun find-files-regexp-other-window (filepattern &optional codesys)
  "Edit files matching regexp pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-regexp-internal filepattern codesys 'find-file-other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'find-files)
;;; find-files.el ends here
