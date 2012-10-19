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

(defface qe-kill-region-face
  '((t (:inherit region)))
  "Face to highlight region that will be killed"
  :group 'faces)

(defface qe-copy-region-face
  '((t (:inherit region)))
  "Face to highlight region that will be copied"
  :group 'faces)

(defvar qe-highlight-delay 0.5
  "*How long to highlight.")

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

(defun qe-forward-not-word ()
  "Forward to next not-word or whitespace."
  (interactive)
  (when (qe-looking-at-syntax "^w_ ")
    (forward-char))
  (when (looking-at "\\s-*$")
    (forward-line))
  (while (or (> (skip-syntax-forward "w_ ") 0)
             (and (looking-at "$")
                  (progn (forward-char) t)
                  (not (eobp))))))

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
  (let (done)
    (while (not (or done (eobp)))
      (when (and (re-search-forward "[{}]" nil 'go)
                 (not (memq (get-text-property (1- (point)) 'face)
                            '(font-lock-comment-face font-lock-string-face))))
        (when (equal (char-before) ?{)
          (backward-char)
          (forward-sexp))
        (forward-line)
        (beginning-of-line)
        (while (and (not (eobp)) (looking-at "^\\s-*$"))
          (forward-line))
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

(defun qe-backward-not-word ()
  "Backward to next not-word or whitespace."
  (interactive)
  (skip-syntax-backward "w_ ")
  (backward-char)
  (while (and (looking-at "$") (not (bobp)))
    (skip-syntax-backward "w_ ")
    (backward-char)))

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
  (let (done)
    (while (not (or done (bobp)))
      (when (and (re-search-backward "[{}]" nil 'go)
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
With prefix arg, append kill."
  (interactive "*P")
  (when arg (append-next-kill))
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (point)
                 (progn
                   (cond ((looking-at "\\<\\(\\sw\\|\\s_\\)")
                          (skip-syntax-forward "w_")
                          (skip-syntax-forward " "))
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
                   (point)))))

(defun qe-forward-kill-section (&optional arg)
  "Forward kill pieces of words.
With prefix arg, append kill."
  (interactive "*P")
  (when arg (append-next-kill))
  (kill-region (point)
               (progn
                 (qe-forward-section)
                 (skip-syntax-forward "_")
                 (point))))

(defun qe-backward-kill (&optional arg)
  "Smart kill backward.
1. If region is active, kill it
2. Else if looking back at whitespace, kill backward whitespace and word
3. Else if in the middle of a word, kill backward word
4. Else if looking at punctuation, kill backward punctuation
5. Else if looking at an close bracket/brace/paren, kill backward sexp
6. Else if looking at a quotation mark, kill backward quoted text
7. Else kill previous char
With prefix arg, append kill."
  (interactive "*P")
  (when arg (append-next-kill))
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (point)
                 (progn
                   (cond ((qe-looking-back-syntax " ")
                          (skip-syntax-backward " ")
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
                   (point)))))

(defun qe-backward-kill-section (&optional arg)
  "Backward kill pieces of words.
With prefix arg, append kill."
  (interactive "*P")
  (when arg (append-next-kill))
  (kill-region (point)
               (progn
                 (skip-syntax-backward "_")
                 (qe-backward-section)
                 (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text unit

(defvar qe-unit-prev-key nil)
(defvar qe-unit-prev-to-char nil)

(defun qe-unit-kill (&optional arg)
  "TODO"
  (interactive "*P")
  (let (result)
    (if (region-active-p)
        (progn
          (setq result (cons (region-beginning) (region-end))
                qe-unit-prev-key nil)
          (deactivate-mark))
      (let ((cmd-keys (this-command-keys))
            (seq (read-key-sequence "Kill:"))
            fcn)
        (if (and (equal seq cmd-keys)
                 (equal real-last-command 'qe-unit-kill)
                 (member qe-unit-prev-key '("t" "T" "s" "S")))
            (setq fcn (lookup-key qe-unit-common-map qe-unit-prev-key)
                  result (funcall fcn qe-unit-prev-to-char))
          (if (equal seq cmd-keys)
              (setq fcn 'qe-unit-ends-line)
            (setq fcn (lookup-key qe-unit-common-map seq)))
          (unless fcn
            (error "Unknown key entered for kill text unit"))
          (setq result (funcall fcn)
                qe-unit-prev-key seq))))
    (when (and result
               (consp result)
               (not (= (car result) (cdr result))))
      (when arg (append-next-kill))
      (kill-region (car result) (cdr result)))))

(defun qe-unit-copy (&optional arg)
  "TODO"
  (interactive "P")
  (let (result (do-highlight t))
    (if (region-active-p)
        (progn
          (setq result (cons (region-beginning) (region-end)))
          (deactivate-mark)
          (setq do-highlight nil))
      (let ((cmd-keys (this-command-keys))
            (seq (read-key-sequence "Copy:"))
            fcn)
        (if (equal seq cmd-keys)
            (setq fcn 'qe-unit-ends-line)
          (setq fcn (lookup-key qe-unit-common-map seq)))
        (unless fcn
          (error "Unknown key entered for copy text unit"))
        (save-excursion
          (setq result (funcall fcn)))))
    (when (and result
               (consp result)
               (not (= (car result) (cdr result))))
      (when do-highlight
        (qe-highlight (car result) (cdr result) 'qe-copy-region-face))
      (when arg (append-next-kill))
      (kill-ring-save (car result) (cdr result)))))

(defun qe-unit-move ()
  "TODO"
  (interactive)
  (let ((cmd-keys (this-command-keys))
        (seq (read-key-sequence "Move:"))
        fcn result)
    (if (and (equal seq cmd-keys)
             (equal real-last-command 'qe-unit-move)
             (member qe-unit-prev-key '("t" "T" "s" "S")))
        (setq fcn (lookup-key qe-unit-common-map qe-unit-prev-key)
              result (funcall fcn qe-unit-prev-to-char))
      (setq fcn (lookup-key qe-unit-common-map seq))
      (unless fcn
        (error "Unknown key entered for move text unit"))
      (setq result (funcall fcn)
            qe-unit-prev-key seq))
    (when (and result (consp result))
      (cond ((member seq '("(" "[" "{" "<"))
             (goto-char (car result)))
            ((or (equal seq "T")
                 (and (equal seq cmd-keys)
                      (equal real-last-command 'qe-unit-move)
                      (equal qe-unit-prev-key "T")))
             (goto-char (1- (cdr result))))
            (t
             (goto-char (cdr result)))))))

(defvar qe-unit-common-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") (lambda () (qe-unit-ends-point-to-fcn 'qe-forward-paragraph)))
    (define-key map (kbd "P") (lambda () (qe-unit-ends-point-to-fcn 'qe-backward-paragraph)))
    (define-key map (kbd "b") (lambda () (qe-unit-ends-point-to-fcn 'qe-forward-block)))
    (define-key map (kbd "B") (lambda () (qe-unit-ends-point-to-fcn 'qe-backward-block)))
    (define-key map (kbd "w") 'qe-unit-ends-forward-word)
    (define-key map (kbd "W") 'qe-unit-ends-backward-word)
    (define-key map (kbd "_") (lambda () (qe-unit-ends-point-to-fcn 'qe-forward-section)))
    (define-key map (kbd "-") (lambda () (qe-unit-ends-point-to-fcn 'qe-backward-section)))
    (define-key map (kbd "m") 'qe-unit-ends-forward-matching)
    (define-key map (kbd "M") 'qe-unit-ends-backward-matching)
    (define-key map (kbd "t") 'qe-unit-ends-forward-to-char)
    (define-key map (kbd "T") 'qe-unit-ends-backward-to-char)
    (define-key map (kbd "s") 'qe-unit-ends-forward-starts-char)
    (define-key map (kbd "S") 'qe-unit-ends-backward-starts-char)
    (define-key map (kbd "e") (lambda () (qe-unit-ends-point-to-fcn 'end-of-line)))
    (define-key map (kbd "a") (lambda () (qe-unit-ends-point-to-fcn 'back-to-indentation)))
    (define-key map (kbd "A") (lambda () (qe-unit-ends-point-to-fcn 'beginning-of-line)))
    (define-key map (kbd "RET") (lambda () (qe-unit-ends-point-to-fcn 'forward-paragraph)))
    (define-key map (kbd "SPC") 'qe-unit-ends-mark)
    (define-key map (kbd "TAB") 'qe-unit-ends-forward-whitespace)
    (define-key map (kbd "\"") (lambda () (qe-region-inside-quotes ?\" 'forward)))
    (define-key map (kbd "'") (lambda () (qe-region-inside-quotes ?\' 'forward)))
    (define-key map (kbd ")") (lambda () (qe-region-inside-pair ?\) 'forward)))
    (define-key map (kbd "]") (lambda () (qe-region-inside-pair ?\] 'forward)))
    (define-key map (kbd "}") (lambda () (qe-region-inside-pair ?\} 'forward)))
    (define-key map (kbd ">") (lambda () (qe-region-inside-pair ?\> 'forward)))
    (define-key map (kbd "(") (lambda () (qe-region-inside-pair ?\) 'backward)))
    (define-key map (kbd "[") (lambda () (qe-region-inside-pair ?\] 'backward)))
    (define-key map (kbd "{") (lambda () (qe-region-inside-pair ?\} 'backward)))
    (define-key map (kbd "<") (lambda () (qe-region-inside-pair ?\> 'backward)))
    (define-key map (kbd "i") 'qe-unit-ends-inside)
    map)
  "Common keymap for unit kill/copy/move.  Functions should return a cons
cell with beginning/end of region.  Original point position need not be
preserved.")

(defun qe-unit-ends-point-to-fcn (fcn)
  "Wrap single function call getting end points."
  (cons (point) (progn (funcall fcn) (point))))

(defun qe-unit-ends-line ()
  "Text unit ends for current line."
  (cons (point-at-bol) (point-at-bol 2)))

(defun qe-unit-ends-forward-word ()
  "Text unit ends for forward word."
  (cons (point) (progn (skip-syntax-forward "w_") (point))))

(defun qe-unit-ends-backward-word ()
  "Text unit ends for backward word."
  (cons (point) (progn (skip-syntax-backward "w_") (point))))

(defun qe-unit-ends-forward-matching ()
  "Text unit ends for forward matching parens."
  (let ((beg (point))
        (char (char-after))
        (table (copy-syntax-table (syntax-table))))
    (when (or (eq char ?<) (eq char ?>))
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      (forward-sexp 1)
      (cons beg (point)))))

(defun qe-unit-ends-backward-matching ()
  "Text unit ends for backward matching parens."
  (let ((beg (point))
        (char (char-before))
        (table (copy-syntax-table (syntax-table))))
    (when (or (eq char ?<) (eq char ?>))
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (with-syntax-table table
      (forward-sexp -1)
      (cons beg (point)))))

(defun qe-unit-ends-forward-to-char (&optional char)
  "Text unit ends for forward to some char."
  (let ((case-fold-search nil))
    (unless char
      (message "To char:")
      (setq char (read-char)
            qe-unit-prev-to-char char))
    (cons (point) (progn (forward-char)
                         (search-forward (char-to-string char))
                         (backward-char)
                         (point)))))

(defun qe-unit-ends-backward-to-char (&optional char)
  "Text unit ends for backward over some char."
  (let ((case-fold-search nil))
    (unless char
      (message "Backward to char:")
      (setq char (read-char)
            qe-unit-prev-to-char char))
    (cons (point) (progn (search-backward (char-to-string char))
                         (forward-char)
                         (point)))))

(defun qe-unit-ends-forward-starts-char (&optional char)
  "Text unit ends for forward to word starting with char."
  (let ((case-fold-search nil))
    (unless char
      (message "To word starting with char:")
      (setq char (read-char)
            qe-unit-prev-to-char char))
    (cons (point) (progn (forward-char)
                         (re-search-forward (format "\\_<%c" char))
                         (backward-char)
                         (point)))))

(defun qe-unit-ends-backward-starts-char (&optional char)
  "Text unit ends for backward over word starting with char."
  (let ((case-fold-search nil))
    (unless char
      (message "Backward to word starting with char:")
      (setq char (read-char)
            qe-unit-prev-to-char char))
    (cons (point) (progn (re-search-backward (format "\\_<%c" char))
                         (point)))))

(defun qe-unit-ends-mark ()
  "Text unit ends for mark."
  (cons (point) (progn (goto-char (or (mark) (point))) (point))))

(defun qe-unit-ends-forward-whitespace ()
  "Text unit ends for forward whitespace."
  (cons (point) (progn (skip-syntax-forward " ") (point))))

(defun qe-unit-ends-inside ()
  "Text unit ends for inside quotes/parens."
  (message "Inside:")
  (let ((char (read-char)))
    (if (memq char '(?\( ?\) ?\[ ?\] ?\{ ?\} ?\< ?\>))
        (qe-region-inside-pair char 'inside)
      (if(memq char '(?\" ?\'))
          (qe-region-inside-quotes char 'inside)
        (error "Unknown char entered for kill inside text unit")))))

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
              (skip-chars-forward "A-Z0-9"))))
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
         open close)
    (when (or (eq char ?<) (eq char ?>))
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table))
    (cond ((or (eq char ?() (eq char ?)))
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

(defun qe-highlight (beg end &optional face)
  "Highlight a region temporarily."
  (unless face
    (setq face 'isearch))
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face face)
    (sit-for qe-highlight-delay)
    (delete-overlay ov)))

(provide 'quick-edit)
;;; quick-edit.el ends here
