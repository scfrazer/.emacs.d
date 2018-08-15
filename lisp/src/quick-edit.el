;;; quick-edit.el --- Quick motion and copy/kill commands
;;
;; Copyright (C) 2011  Scott Frazer
;;
;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 24 Oct 2011
;; Version: 1.0
;; Keywords: convenience
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; TODO A set of quick motion and copy/kill commands

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable variables

(defvar qe-camelcase-sections t
  "*Section commands also work on camelCase")

(make-local-variable 'qe-camelcase-sections)

(defvar qe-block-indented-modes (list 'emacs-lisp-mode
                                      'lisp-mode
                                      'python-mode)
  "*Major modes where blocks commands should work by indentation")

(defface qe-copy-region-face
  '((t (:inherit region)))
  "Face to highlight region that will be copied"
  :group 'faces)

(defvar qe-highlight-delay 0.5
  "*How long to highlight.")

;; Avoid byte-compiler warnings
(defvar mc--this-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Motion

(defun qe-forward-word ()
  "Like forward-word, but stops at beginning of words."
  (interactive)
  (when (qe-looking-at-syntax "w_")
    (skip-syntax-forward "w_"))
  (skip-syntax-forward "^w_"))

(defun qe-forward-word-section ()
  "Like forward-word, but only goes over alphanumerics."
  (interactive)
  (qe-forward-section)
  (skip-chars-forward "^a-zA-Z0-9"))

(defun qe-forward-word-end ()
  "Forward to end of word."
  (interactive)
  (unless (qe-looking-at-syntax "w_")
    (skip-syntax-forward "^w_"))
  (skip-syntax-forward "w_"))

(defun qe-forward-paragraph ()
  "Like forward-paragraph, but goes to next non-blank line."
  (interactive)
  (beginning-of-line)
  (when (re-search-forward "^\\s-*$" nil 'go)
    (re-search-forward "[^ \t\f\n]" nil 'go)
    (beginning-of-line)))

(defun qe-forward-block ()
  "Goes forward to end of next curly-bracket or indented block
depending on the major mode (see `qe-block-indented-modes')."
  (interactive)
  (if (memq major-mode qe-block-indented-modes)
      (qe-forward-indented-block)
    (qe-forward-curly-block)))

(defun qe-forward-curly-block ()
  "Goes forward to end of next curly-bracket block."
  (interactive)
  (let (done forward-sexp-function)
    (while (not (or done (eobp)))
      (when (and (re-search-forward "{" nil 'go)
                 (not (memq (get-text-property (1- (point)) 'face)
                            '(font-lock-comment-face font-lock-string-face))))
        (when (equal (char-before) ?{)
          (backward-char)
          (forward-sexp))
        (forward-line)
        (beginning-of-line)
        (setq done t)))))

(defun qe-forward-indented-block ()
  "Goes forward to next line at the same or less indentation."
  (interactive)
  (beginning-of-line)
  (when (looking-at "\\s-*$")
    (re-search-forward "[^ \t\f\n]" nil 'go))
  (back-to-indentation)
  (let ((col (current-column)) done)
    (while (not (or done (eobp)))
      (forward-line 1)
      (back-to-indentation)
      (when (and (<= (current-column) col) (not (looking-at "$")))
        (beginning-of-line)
        (while (and (not (eobp)) (looking-at "^\\s-*$"))
          (forward-line))
        (setq done t)))))

(defun qe-backward-word ()
  "Like backward-word, but stops at beginning of words."
  (interactive)
  (unless (qe-looking-back-syntax "w_")
    (skip-syntax-backward "^w_"))
  (skip-syntax-backward "w_"))

(defun qe-backward-word-section ()
  "Like backward-word, but only goes over alphanumerics."
  (interactive)
  (skip-chars-backward "^a-zA-Z0-9")
  (qe-backward-section))

(defun qe-backward-word-end ()
  "Backward to end of word."
  (interactive)
  (when (qe-looking-back-syntax "w_")
    (skip-syntax-backward "w_"))
  (skip-syntax-backward "^w_"))

(defun qe-backward-paragraph ()
  "Go to first line after previous blank line."
  (interactive)
  (beginning-of-line)
  (re-search-backward "[^ \t\f\n]" nil 'go)
  (when (re-search-backward "^\\s-*$" nil 'go)
    (forward-line 1))
  (beginning-of-line))

(defun qe-backward-block ()
  "Goes backward to beginning of next curly-bracket or indented block
depending on the major mode (see `qe-block-indented-modes')."
  (interactive)
  (if (memq major-mode qe-block-indented-modes)
      (qe-backward-indented-block)
    (qe-backward-curly-block)))

(defun qe-backward-curly-block ()
  "Goes backward to beginning of next curly-bracket block."
  (interactive)
  (let (done forward-sexp-function)
    (while (not (or done (bobp)))
      (when (and (re-search-backward "}" nil 'go)
                 (not (memq (get-text-property (point) 'face)
                            '(font-lock-comment-face font-lock-string-face))))
        (when (equal (char-after) ?})
          (forward-char)
          (backward-sexp))
        (beginning-of-line)
        (setq done t)))))

(defun qe-backward-indented-block ()
  "Goes backward to beginning of line at the same or less indentation."
  (interactive)
  (beginning-of-line)
  (when (looking-at "\\s-*$")
    (re-search-backward "[^ \t\f\n]" nil 'go))
  (back-to-indentation)
  (let ((col (current-column)) done)
    (while (not (or done (bobp)))
      (forward-line -1)
      (back-to-indentation)
      (when (and (<= (current-column) col) (not (looking-at "$")))
        (setq done t)
        (beginning-of-line)))))

(defun qe-yank ()
  "Like yank, but with prefix number yank that many times."
  (interactive "*")
  (when (and delete-selection-mode (region-active-p))
    (delete-region (region-beginning) (region-end)))
  (if (and current-prefix-arg (integerp current-prefix-arg))
      (dotimes (x current-prefix-arg)
        (yank))
    (yank)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart kill

(defun qe-forward-kill (&optional arg)
  "Smart kill forward.
1. If region is active, kill it
2. Else if at the beginning of a word, kill the word and trailing whitespace
3. Else if in the middle of a word, kill the rest of the word
4. Else if looking at whitespace, kill whitespace forward
5. Else if looking at punctuation, kill punctuation forward
6. Else if looking at an open bracket/brace/paren, kill sexp forward
7. Else if looking at a quotation mark, kill quoted text
8. Else kill next char
With C-u prefix arg, delete instead of kill.  With numeric prefix arg, append kill."
  (interactive "*P")
  (let (forward-sexp-function)
    (when (and arg (not (listp arg))) (append-next-kill))
    (if (region-active-p)
        (if (and arg (listp arg))
            (delete-region (region-beginning) (region-end))
          (kill-region (region-beginning) (region-end)))
      (let ((table (copy-syntax-table (syntax-table))))
        (when (equal (char-after) ?\')
          (modify-syntax-entry ?\' "\"" table))
        (with-syntax-table table
          (let ((beg (point))
                (end (progn
                       (cond ((looking-at "\\<\\(\\sw\\|\\s_\\)")
                              (skip-syntax-forward "w_")
                              (skip-syntax-forward " " (point-at-eol)))
                             ((qe-looking-at-syntax "w_")
                              (skip-syntax-forward "w_"))
                             ((qe-looking-at-syntax " ")
                              (skip-syntax-forward " "))
                             ((qe-looking-at-syntax ".")
                              (skip-syntax-forward "."))
                             ((qe-looking-at-syntax "(")
                              (forward-sexp))
                             ((qe-looking-at-syntax "\"")
                              (let ((c (char-after)) region)
                                (forward-char)
                                (setq region (qe-region-inside-quotes c 'forward))
                                (goto-char (cdr region)))
                              (forward-char))
                             (t
                              (forward-char)))
                       (point))))
            (if (and arg (listp arg))
                (delete-region beg end)
              (kill-region beg end))))))))

(defun qe-forward-kill-section (&optional arg)
  "Forward kill pieces of words.
With C-u prefix arg, delete instead of kill.  With numeric prefix arg, append kill."
  (interactive "*P")
  (when (and arg (not (listp arg))) (append-next-kill))
  (let ((beg (point))
        (end (progn
               (qe-forward-section)
               (skip-syntax-forward "_")
               (point))))
    (if (and arg (listp arg))
        (delete-region beg end)
      (kill-region beg end))))

(defun qe-backward-kill (&optional arg)
  "Smart kill backward.
1. If region is active, kill it
2. Else if looking back at whitespace, kill backward whitespace and word
3. Else if in the middle of a word, kill backward word
4. Else if looking at punctuation, kill backward punctuation
5. Else if looking at an close bracket/brace/paren, kill backward sexp
6. Else if looking at a quotation mark, kill backward quoted text
7. Else kill previous char
With C-u prefix arg, delete instead of kill.  With numeric prefix arg, append kill."
  (interactive "*P")
  (let (forward-sexp-function)
    (when (and arg (not (listp arg))) (append-next-kill))
    (if (region-active-p)
        (if (and arg (listp arg))
            (delete-region (region-beginning) (region-end))
          (kill-region (region-beginning) (region-end)))
      (let ((table (copy-syntax-table (syntax-table))))
        (when (equal (char-before) ?\')
          (modify-syntax-entry ?\' "\"" table))
        (with-syntax-table table
          (let ((beg (point))
                (end (progn
                       (cond ((= (char-before) ?\n)
                              (backward-char))
                             ((qe-looking-back-syntax " ")
                              (skip-syntax-backward " " (point-at-bol))
                              (when (qe-looking-back-syntax "w_")
                                (skip-syntax-backward "w_")))
                             ((qe-looking-back-syntax "w_")
                              (skip-syntax-backward "w_"))
                             ((qe-looking-back-syntax ".")
                              (skip-syntax-backward "."))
                             ((qe-looking-back-syntax ")")
                              (backward-sexp))
                             ((qe-looking-back-syntax "\"")
                              (backward-char)
                              (let ((c (char-after)) region)
                                (setq region (qe-region-inside-quotes c 'backward))
                                (goto-char (car region)))
                              (backward-char))
                             (t
                              (backward-char)))
                       (point))))
            (if (and arg (listp arg))
                (delete-region beg end)
              (kill-region beg end))))))))

(defun qe-backward-kill-section (&optional arg)
  "Backward kill pieces of words.
With C-u prefix arg, delete instead of kill.  With numeric prefix arg, append kill."
  (interactive "*P")
  (when (and arg (not (listp arg))) (append-next-kill))
  (let ((beg (point))
        (end (progn
               (skip-syntax-backward "_")
               (qe-backward-section)
               (point))))
    (if (and arg (listp arg))
        (delete-region beg end)
      (kill-region beg end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text unit

(defun qe-unit-kill (key-seq)
  "Kill by text unit."
  (interactive (list
                (read-key-sequence
                 (if current-prefix-arg
                     (if (listp current-prefix-arg)
                         "Delete:"
                       "(Append) Kill:")
                   "Kill:"))))
  (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
    (setq mc--this-command `(lambda () (interactive) (qe-unit-kill-1 ',key-seq))))
  (qe-unit-kill-1 key-seq))

(defun qe-unit-kill-1 (key-seq)
  "Real work for qe-unit-kill."
  (let ((bounds (qe-unit-bounds key-seq)))
    (if current-prefix-arg
        (if (listp current-prefix-arg)
            (delete-region (car bounds) (cdr bounds))
          (append-next-kill)
          (kill-region (car bounds) (cdr bounds)))
      (kill-region (car bounds) (cdr bounds)))))

(defvar qe-highlight-num 0)
(defvar qe-highlight-count 0)
(defvar qe-highlight-overlays nil)

(defun qe-unit-copy (key-seq)
  "Copy by text unit."
  (interactive (list
                (read-key-sequence
                 (if current-prefix-arg "(Append) Copy:" "Copy:"))))
  (setq qe-highlight-num 1)
  (setq qe-highlight-count 0)
  (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode (fboundp 'mc/num-cursors))
    (setq qe-highlight-num (mc/num-cursors))
    (setq mc--this-command `(lambda () (interactive) (qe-unit-copy-1 ',key-seq))))
  (qe-unit-copy-1 key-seq))

(defun qe-unit-copy-1 (key-seq)
  "Real work for qe-unit-copy."
  (let ((bounds (qe-unit-bounds key-seq)))
    (when current-prefix-arg (append-next-kill))
    (kill-ring-save (car bounds) (cdr bounds))
    (if (region-active-p)
        (deactivate-mark)
      (let ((ov (make-overlay (car bounds) (cdr bounds))))
        (overlay-put ov 'face 'qe-copy-region-face)
        (push ov qe-highlight-overlays)
        (setq qe-highlight-count (1+ qe-highlight-count))
        (when (= qe-highlight-count qe-highlight-num)
          (sit-for qe-highlight-delay)
          (while qe-highlight-overlays
            (delete-overlay (pop qe-highlight-overlays))))))))

(defun qe-unit-bounds (key-seq)
  "Get text unit bounds for KEY-SEQ."
  (let ((first-key (aref key-seq 0))
        (dir 'forward))
    ;; If C-w or M-w return current region
    (if (or (equal first-key ?\C-w)
            (and (equal (length key-seq) 2)
                 (equal first-key 27)
                 (equal (aref key-seq 1) ?w)))
        (cons (region-beginning) (region-end))
      (cond ((equal first-key ?r)
             (setq dir 'backward
                   first-key (read-char)))
            ((equal first-key ?i)
             (setq dir 'inside
                   first-key (read-char))))
      ;; Decode key
      (save-excursion
        (cl-case first-key
          (?\C-m (qe-unit-ends-point-to-fcn 'qe-forward-next-blank-line))
          (?\" (qe-region-inside-quotes ?\" dir))
          (?\' (qe-region-inside-quotes ?\' dir))
          (?\` (qe-region-inside-quotes ?\` dir))
          (?\} (qe-region-inside-pair ?\} dir))
          (?\) (qe-region-inside-pair ?\) dir))
          (?\] (qe-region-inside-pair ?\] dir))
          (?\> (qe-region-inside-pair ?\> dir))
          (?A (qe-unit-ends-point-to-fcn 'back-to-indentation))
          (?a (qe-unit-ends-point-to-fcn 'beginning-of-line))
          (?b (qe-unit-ends-point-to-fcn 'qe-forward-block))
          (?c (qe-unit-ends-forward-to-char))
          (?e (qe-unit-ends-point-to-fcn 'end-of-line))
          (?l (qe-unit-ends-line))
          (?m (qe-unit-ends-matching))
          (?p (qe-unit-ends-point-to-fcn 'qe-forward-paragraph))
          (?s (qe-unit-symbol))
          (?t (qe-unit-ends-forward-to-starting-char))
          (?w (qe-unit-ends-forward-word))
          (?x (qe-region-xml-content dir))
          (t
           (cond ((not (equal dir 'forward))
                  (if (member first-key '(?< ?\( ?\[ ?\{ ?> ?\) ?\] ?\}))
                      (qe-region-inside-pair first-key dir)
                    (when (member first-key '(?\" ?\' ?\`))
                      (qe-region-inside-quotes first-key dir))))
                 ((or (< 31 first-key 48)    ;; space through slash
                      (< 57 first-key 65)    ;; colon through at-symbol
                      (< 90 first-key 97)    ;; left-bracket through backtick
                      (< 122 first-key 127)) ;; left-brace through tilde
                  (qe-unit-ends-forward-to-char first-key))
                 (t
                  (error "Unknown key entered for text unit")))))))))

(defun qe-unit-ends-point-to-fcn (fcn)
  "Wrap single function call getting end points."
  (cons (point) (progn (funcall fcn) (point))))

(defun qe-unit-ends-line ()
  "Text unit ends for current line."
  (cons (point-at-bol) (point-at-bol 2)))

(defun qe-forward-next-blank-line ()
  "Text unit ends for forward to next blank line."
  (cons (point) (progn (re-search-forward "^\\s-*$" nil 'go))))

(defun qe-unit-ends-forward-word ()
  "Text unit ends for forward word."
  (cons (point) (progn (skip-syntax-forward "w_") (point))))

(defun qe-unit-ends-matching ()
  "Text unit ends matching parens/quotes."
  (let* ((table (copy-syntax-table (syntax-table)))
         (char (char-before))
         (backward (member char '(?> ?\) ?\] ?\} ?\` ?\' ?\")))
         (beg (point))
         forward-sexp-function)
    (unless backward
      (setq char (char-after)))
    (when (or (eq char ?<) (eq char ?>))
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (when (eq char ?`)
      (modify-syntax-entry ?` "\"" table))
    (when (eq char ?\')
      (modify-syntax-entry ?\' "\"" table))
    (when (eq char ?\")
      (modify-syntax-entry ?\" "\"" table))
    (with-syntax-table table
      (if backward
          (progn
            (forward-sexp -1)
            (cons (point) beg))
        (forward-sexp 1)
        (cons beg (point))))))

(defun qe-unit-ends-forward-to-char (&optional char)
  "Text unit ends for forward to char."
  (let ((case-fold-search nil))
    (unless char
      (message "To char:")
      (setq char (read-char)))
    (cons (point) (progn (forward-char)
                         (search-forward (char-to-string char))
                         (backward-char)
                         (point)))))

(defun qe-unit-ends-forward-to-starting-char ()
  "Text unit ends for forward to a word starting with char."
  (let ((case-fold-search nil))
    (message "To char:")
    (let ((char (read-char)))
      (cons (point) (progn (forward-char)
                           (re-search-forward (concat "\\<" (char-to-string char)) nil t)
                           (backward-char)
                           (point))))))

(defun qe-unit-symbol ()
  "Text unit ends for current symbol."
  (cons (progn (skip-syntax-backward "w_") (point))
        (progn (skip-syntax-forward "w_") (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions

(defun qe-looking-at-syntax (str)
  "Return non-nil if looking at syntax of a char in STR."
  (unless (eobp)
    (let* ((invert (= (string-to-char str) ?^))
           (syntax-chars (append (if invert (substring str 1) str) nil))
           (result (member (char-syntax (char-after (point))) syntax-chars)))
      (if invert (not result) result))))

(defun qe-looking-back-syntax (str)
  "Return non-nil if looking back at syntax of a char in STR."
  (unless (bobp)
    (let* ((invert (= (string-to-char str) ?^))
           (syntax-chars (append (if invert (substring str 1) str) nil))
           (result (member (char-syntax (char-before (point))) syntax-chars)))
      (if invert (not result) result))))

(defun qe-forward-section ()
  "Move forward a word section."
  (let ((case-fold-search nil))
    (if qe-camelcase-sections
        (if (looking-at "[a-z0-9]")
            (skip-chars-forward "a-z0-9")
          (when (looking-at "[A-Z]")
            (forward-char)
            (if (looking-at "[a-z]")
                (skip-chars-forward "a-z0-9")
              (skip-chars-forward "A-Z0-9")
              (when (and (looking-at "[a-z]")
                         (looking-back "[A-Z0-9][A-Z]" (point-at-bol)))
                (backward-char)))))
      (skip-chars-forward "a-zA-Z0-9"))))

(defun qe-backward-section ()
  "Move backward a word section."
  (let ((case-fold-search nil))
    (if qe-camelcase-sections
        (progn
          (skip-chars-backward "0-9")
          (if (looking-back "[A-Z]" (1- (point)))
              (skip-chars-backward "A-Z0-9")
            (when (looking-back "[a-z]" (1- (point)))
              (skip-chars-backward "a-z0-9")
              (when (looking-back "[A-Z]" (1- (point)))
                (backward-char)))))
      (skip-chars-backward "a-zA-Z0-9"))))

(defun qe-region-inside-pair (char dir)
  "Find the region inside paired chars ()[]{}<>"
  (let* ((beg (point))
         (end beg)
         (table (copy-syntax-table (syntax-table)))
         open close forward-sexp-function)
    (when (or (eq char ?\<) (eq char ?\>))
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (cond ((or (eq char ?\() (eq char ?\)))
           (setq open ?\( close ?\)))
          ((or (eq char ?\[) (eq char ?\]))
           (setq open ?\[ close ?\]))
          ((or (eq char ?\{) (eq char ?\}))
           (setq open ?\{ close ?\}))
          ((or (eq char ?\<) (eq char ?\>))
           (setq open ?\< close ?\>)))
    (unless (equal dir 'forward)
      (setq beg (save-excursion
                  (with-syntax-table table
                    (catch 'done
                      (condition-case nil
                          (while t
                            (backward-up-list)
                            (when (= (char-after) open)
                              (throw 'done (1+ (point)))))
                        (error nil))
                      (point-min))))))
    (unless (equal dir 'backward)
      (setq end (save-excursion
                  (with-syntax-table table
                    (catch 'done
                      (condition-case nil
                          (while t
                            (up-list)
                            (when (= (char-before) close)
                              (throw 'done (1- (point)))))
                        (error nil))
                      (point-max))))))
    (cons beg end)))

(defun qe-region-inside-quotes (char dir)
  "Find the region inside quote chars."
  (let* ((beg (point))
         (end beg)
         (regex (char-to-string char))
         done)
    (unless (equal dir 'forward)
      (save-excursion
        (setq done nil)
        (while (not (or done (bobp)))
          (when (re-search-backward regex nil 'go)
            (setq done (not (equal (char-before) ?\\)))))
        (unless (bobp)
          (forward-char))
        (setq beg (point))))
    (unless (equal dir 'backward)
      (save-excursion
        (setq done nil)
        (while (not (or done (eobp)))
          (when (re-search-forward regex nil 'go)
            (backward-char)
            (setq done (not (equal (char-before) ?\\)))
            (forward-char)))
        (unless (eobp)
          (backward-char))
        (setq end (point))))
    (cons beg end)))

(defvar qe-xml-void-tags
  (regexp-opt (list "area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen" "link" "meta" "param" "source" "track" "wbr"))
  "XML void tags (i.e. HTML5 void tags.)")

(defun qe-region-xml-content (dir)
  "Find the region of the the current XML content."
  (let* ((beg (point))
         (end beg)
         (table (copy-syntax-table (syntax-table)))
         depth
         forward-sexp-function)
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (with-syntax-table table
      (unless (equal dir 'forward)
        (save-excursion
          (setq depth 0)
          (while (not (or (= -1 depth) (bobp)))
            (when (re-search-backward ">" nil 'go)
              (if (= (char-before) ?/)
                  (progn (forward-char) (backward-sexp))
                (forward-char)
                (setq beg (point))
                (backward-sexp)
                (if (looking-at "</")
                    (when (not (looking-at (concat "</" qe-xml-void-tags)))
                      (setq depth (1+ depth)))
                  (when (and (looking-at "<[a-zA-Z]")
                             (not (looking-at (concat "<" qe-xml-void-tags))))
                    (setq depth (1- depth)))))))))
      (unless (equal dir 'backward)
        (save-excursion
          (setq depth 0)
          (while (not (or (= -1 depth) (eobp)))
            (when (re-search-forward "<" nil 'go)
              (backward-char)
              (if (looking-at (concat "</?" qe-xml-void-tags))
                  (forward-sexp)
                (setq end (point))
                (if (looking-at "</")
                    (progn
                      (forward-sexp)
                      (setq depth (1- depth)))
                  (if (looking-at "<[a-zA-Z]")
                      (progn
                        (forward-sexp)
                        (backward-char)
                        (unless (= (char-before) ?/)
                          (setq depth (1+ depth))))
                    (forward-sexp))))))))
      (cons beg end))))

(provide 'quick-edit)
;;; quick-edit.el ends here
