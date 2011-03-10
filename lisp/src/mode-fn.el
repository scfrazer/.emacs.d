;;; mode-fn.el --- Execute mode-specific function using a common function name

;; Copyright (C) 2011  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 09 Mar 2011
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
;; This package lets you execute a mode-specific function using a common
;; function name.  For example, if you do this:
;;
;; (require 'mode-fn)
;; (mode-fn-map 'html 'org-mode 'org-export-as-html)
;; (mode-fn-map 'html 'default 'htmlize-buffer)
;;
;; then "M-x html", if you are in an org-mode buffer it will call the export
;; function, otherwise it will call the htmlize function.  The 'default entry
;; is not necessary if you don't want to have one.  You can use
;; `mode-fn-remove' to remove mapped entries.  If all mapped entries are
;; removed, the common function name will also be removed.

;;; Code:

(defvar mode-fn-alist nil
  "*alist that maps a function name to alist of mode-specific functions.
If the function name exists, but there is no mode-specific entry
for the current mode, the 'default entry is called if it
exists.")

;;;###autoload
(defun mode-fn-map (fn-name mode mode-fn)
  "For function FN-NAME, add or replace the mapping for MODE to
point to MODE-FN."
  (let ((fn-alist (assoc fn-name mode-fn-alist)))
    (if fn-alist
        (let* ((alist (cdr fn-alist))
               (mapping (assoc mode alist)))
          (if mapping
              (setcdr mapping mode-fn)
            (setcdr fn-alist (add-to-list 'alist (cons mode mode-fn)))))
      (add-to-list 'mode-fn-alist (cons fn-name (list (cons mode mode-fn))))
      (defalias fn-name 'mode-fn-execute))))

(defun mode-fn-remove (fn-name mode)
  "Remove MODE mapping from FN-NAME."
  (let ((fn-alist (assoc fn-name mode-fn-alist)))
    (unless fn-alist
      (error "Function name not mapped"))
    (let* ((alist (cdr fn-alist)) new-alist)
      (if (not (assoc mode alist))
          (error "No function mapped for this mode")
        (setq new-alist (delq (assoc mode alist) alist))
        (if new-alist
            (setcdr fn-alist new-alist)
          (setq mode-fn-alist (delq fn-alist mode-fn-alist))
          (fmakunbound fn-name))))))

(defun mode-fn-execute ()
  "Executes a mode-specific function.  See `mode-fn-alist' for the current mappings."
  (interactive)
  (let* ((fn-alist (cdr (assoc this-command mode-fn-alist)))
         (cmd (or (assoc major-mode fn-alist)
                  (assoc 'default fn-alist))))
    (if (null cmd)
        (error "No mode-specific or default function defined")
      (call-interactively (cdr cmd)))))

(provide 'mode-fn)
;;; mode-fn.el ends here
