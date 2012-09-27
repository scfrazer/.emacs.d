;;; num3-mode.el --- highlight groups of digits in long numbers  -*- lexical-binding: t -*-

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Felix Lee <felix8a@gmail.com>, Michal Nazarewicz <mina86@mina86.com>
;; Maintainer: Michal Nazarewicz <mina86@mina86.com>
;; Keywoards: faces, minor-mode

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Num3 is a minor mode that makes long numbers more readable by
;; highlighting groups of 3 (customisable) decimal digits or 4 hex
;; digits when font-lock is on.  Highlighting alternates between two
;; faces that can be customised.

;;; Usage:

;;     M-x num3-mode           toggle for current buffer.
;;     M-x global-num3-mode    toggle for all buffers.
;;
;; Or add the following to your ~/.emacs file:
;;     (load "path/to/num3")
;;     (global-num3-mode)

;;; Code:

(defgroup num3 nil
  "Num3 is a minor mode that makes long numbers more readable by
highlighting groups of decimal digits or 4 hex digits when
font-lock is on."
  :group 'text)

(defcustom num3-group-size 3
  "Number of digits to group in decimal numbers.")

(defcustom num3-threshold 5
  "Number must be at least that long to start highlighting.")

(defface num3-face-odd
  '((t))
  "Face to add for odd groups of digits."
  :group 'num3)

(defface num3-face-even
  '((t :background "gray35" :foreground "gray90" :underline t))
  "Face to add for even groups of digits.
The default face uses redundant signaling, because this is in
addition to any other font-lock highlighting."
  :group 'num3)

;;; Implementation:

(require 'font-lock)

;;;###autoload
(define-minor-mode num3-mode
  "Toggle num3 minor mode in the current buffer.
Num3 minor mode makes long numbers more readable by highlighting
groups of digits when font-lock mode is on.

If a number is longer than `num3-threshold', the mode will split
it into a group of `num3-group-size' (if number is decimal) or
4 (if number is hexadecimal) digits.  Hexadecimal number is
detected as one starting with 0x, 0X or 'h.

With decimal numbers, fractions are recognised as well and
grouped from the beginning rathar then from end.  For instance,
with group size of 3, a number \"12345.12345\" will be split into
groups as follows: \"12|345.123|45\".  Fractions without integer
part are also recognised, eg. \".12345\".

The groups are highlighted alternately using `num3-face-odd' and
`num3-face-even' faces.  `num3-face-odd' face (which is empty by
default) is the one used for the group closest to the decimal point,
ie. groups are counted starting with one outwards from the (place
where) decimal point (would be) is."
  nil "" nil
  (if num3-mode
      (unless (assoc '-num3-matcher font-lock-keywords)
        (font-lock-add-keywords nil '(-num3-matcher) 'append))
    (font-lock-remove-keywords nil '(-num3-matcher)))
  (when font-lock-mode
    (font-lock-fontify-buffer)))

;;;###autoload
(define-globalized-minor-mode global-num3-mode num3-mode turn-on-num3-mode)

;;;###autoload
(defun turn-on-num3-mode ()
  "Turn `num3-mode' on if it's not enabled."
  (unless num3-mode (num3-mode t)))

(defconst -num3-number-re
  (concat "\\(?:0[xX]\\|'h\\)\\([0-9a-fA-F]+\\)" ; 1 hexadecimal
          "\\|\\(?:0[bB]\\|'b\\)\\([01]+\\)"     ; 2 binary
          "\\|\\_<\\([0-9]+\\)\\_>"              ; 3 decimal
          "\\|\\.\\([0-9]+\\)"                   ; 4 fraction
          ))

(defun -num3-matcher (lim)
  "Function used as a font-lock-keyword handler used in `num3-mode'.
Performs fontification of numbers from point to LIM."
  (save-excursion
    (while (re-search-forward -num3-number-re lim t)
      (-num3-int  (match-beginning 1) (match-end 1) 4)
      (-num3-int  (match-beginning 2) (match-end 2) 4)
      (-num3-int  (match-beginning 3) (match-end 3) num3-group-size)
      (-num3-frac (match-beginning 4) (match-end 4) num3-group-size)))
  nil)

(defun -num3-int (lo hi n)
  "Highlight groups of digits in a long number.
LO and HI arguments specify the range where the number is
located.  If the length of that region exceeds `num3-threshold',
the function will split it into groups of N digits and fontify
tham alternately using `num3-face-odd' and `num3-face-even'
faces.  Grouping is done from the end, eg. (12)(345)."
  (when (and lo (> (- hi lo) num3-threshold))
    (let (even)
      (while (< lo hi)
        (-num3-put even (max lo (- hi n)) hi)
        (setq hi (- hi n) even (not even))))))

(defun -num3-frac (lo hi n)
  "Highlight groups of digits in a long number.
LO and HI arguments specify the range where the number is
located.  If the length of that region exceeds `num3-threshold',
the function will split it into groups of N digits and fontify
tham alternately using `num3-face-odd' and `num3-face-even'
faces.  Grouping is done from the beginning, eg. (123)(45)."
  (when (and lo (> (- hi lo) num3-threshold))
    (let (even)
      (while (< lo hi)
        (-num3-put even lo (min hi (+ lo n)))
        (setq lo (+ lo n) even (not even))))))

(defun -num3-put (even lo hi)
  "Add font lock text property to highlight a single group of digit.
Use `num3-face-odd' if EVEN is nil and `num3-face-even' if EVEN is
non-nil.  The region the face is set to is from LO to HI."
  (font-lock-append-text-property lo hi 'face
                                  (if even 'num3-face-even 'num3-face-odd)))

(provide 'num3)

;;; num3.el ends here
