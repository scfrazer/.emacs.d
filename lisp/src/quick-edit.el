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
;; Smart kill/copy

(defun qe-forward-kill ()
  "Smart kill forward.
1. If region is active, kill it
2. Else if looking at 'tag', kill tag
3. Else if at the beginning of a word, kill the word and trailing whitespace
4. Else if in the middle of a word, kill the rest of the word
5. Else if looking at whitespace, kill whitespace forward
6. Else if looking at punctuation, kill punctuation forward
7. Else if looking at an open bracket/brace/paren, kill sexp forward
8. Else if looking at a quotation mark, kill quoted text
9. Else kill next char"
  (interactive "*")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (point)
                 (progn
                   (cond ((looking-at "</?\\s-*[a-zA-Z].*>")
                          (goto-char (match-end 0)))
                         ((looking-at "\\<\\(\\sw\\|\\s_\\)")
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

(defun qe-forward-kill-section ()
  "Forward kill pieces of words."
  (interactive "*")
  (kill-region (point)
               (progn
                 (qe-forward-section)
                 (skip-syntax-forward "_")
                 (point))))

(defun qe-backward-kill ()
  "Smart kill backward.
1. If region is active, kill it
2. Else if looking at 'tag', kill tag
3. Else if looking back at whitespace, kill backward whitespace and word
4. Else if in the middle of a word, kill backward word
5. Else if looking at punctuation, kill backward punctuation
6. Else if looking at an close bracket/brace/paren, kill backward sexp
7. Else if looking at a quotation mark, kill backward quoted text
8. Else kill previous char"
  (interactive "*")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (point)
                 (progn
                   (cond ((looking-back "</?\\s-*[a-zA-Z].*>" (point-at-bol))
                          (goto-char (match-beginning 0)))
                         ((qe-looking-back-syntax " ")
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

(defun qe-backward-kill-section ()
  "Backward kill pieces of words."
  (interactive "*")
  (kill-region (point)
               (progn
                 (skip-syntax-backward "_")
                 (qe-backward-section)
                 (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text unit kill/copy

(defun qe-kill-unit (&optional arg)
  "Kill over a unit of text.  With a prefix arg, delete instead of kill.
If the region is highlighted, it will be killed.  Otherwise, you are prompted
to enter another character which will cause the following:
p -- Kill forward paragraph (and trailing whitespace)
P -- Kill backward paragraph (and trailing whitespace)
b -- Kill forward block
B -- Kill backward block
m -- Kill forward over matching paren/brace/curly/angle-bracket/quotes
M -- Kill backward over matching paren/brace/curly/angle-bracket/quotes
w -- Kill foward word
W -- Kill backward word
e -- Kill forward to end of line
a -- Kill backward to beginning of indentation
A -- Kill backward to beginning of line
RET -- Kill forward to end of paragraph
SPC -- Kill to mark
\" -- Kill forward to double-quotes
\' -- Kill forward to single-quote
),],},> -- Kill forward to that char
\(,[,{,< -- Kill backward to that char
i -- Start kill inside paren/brace/curly/quotes/angle-bracket.  Another
  character must then be entered and all the text backward and forward
  inside the matching parens/brackets/curlies/quotes/angle-brackets is killed
/ -- Kill to a location using isearch
? -- Kill backward to a location using isearch
Anything else -- Kill line.  For example if this function is bound to C-w,
  a convenient way to kill the current line is C-w C-w"
  (interactive "*P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if arg
        (message "Delete:")
      (message "Kill:"))
    (let ((c (read-char)) dir)
      (cond
       ((eq c ?/) (call-interactively 'qe-kill-to-isearch))
       ((eq c ??) (call-interactively 'qe-backward-kill-to-isearch))

       ((eq c ?p) (kill-region (point) (progn (qe-forward-paragraph) (point))))
       ((eq c ?P) (kill-region (point) (progn (qe-backward-paragraph) (point))))

       ((eq c ?b) (kill-region (point) (progn (qe-forward-block) (point))))
       ((eq c ?B) (kill-region (point) (progn (qe-backward-block) (point))))

       ((eq c ?m)
        (if (looking-at "</?\\s-*[a-zA-Z].*>")
            (replace-match "")
          (kill-sexp 1)))
       ((eq c ?M)
        (if (looking-back "</?\\s-*[a-zA-Z].*>" (point-at-bol))
            (replace-match "")
          (kill-sexp -1)))

       ((eq c ?w) (kill-region (point) (progn (skip-syntax-forward "w_") (point))))
       ((eq c ?W) (kill-region (point) (progn (skip-syntax-backward"w_") (point))))

       ((eq c ?e) (kill-region (point) (point-at-eol)))
       ((eq c ?a) (kill-region (point) (progn (back-to-indentation) (point))))
       ((eq c ?A) (kill-region (point) (point-at-bol)))

       ((eq c ?) (kill-region (point) (progn (forward-paragraph) (point))))

       ((eq c ? ) (kill-region (point) (or (mark) (point))))

       ((not (memq c '(?i ?\( ?\) ?\[ ?\] ?\{ ?\} ?\< ?\> ?\" ?\')))
        (kill-region (point-at-bol) (point-at-bol 2))))

      (when (eq c ?i)
        (if arg
            (message "Kill: i")
          (message "Delete: i"))
        (setq c (read-char))
        (setq dir 'inside))

      (cond ((memq c '(?\( ?\[ ?\{ ?\<))
             (unless dir
               (setq dir 'backward)))
            ((memq c '(?\) ?\] ?\} ?\>))
             (unless dir
               (setq dir 'forward))))
      (when (memq c '(?\( ?\) ?\[ ?\] ?\{ ?\} ?\< ?\>))
        (let ((region (qe-region-inside-pair c dir)))
          (kill-region (car region) (cdr region))))

      (when (memq c '(?\" ?\'))
        (unless dir
          (setq dir 'forward))
        (let ((region (qe-region-inside-quotes c dir)))
          (kill-region (car region) (cdr region))))))

  (when arg
    (when kill-ring
      (setq kill-ring (cdr kill-ring)))
    (when kill-ring-yank-pointer
      (setq kill-ring-yank-pointer kill-ring))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar qe-unit-common-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") (lambda () (qe-unit-ends-point-to-fcn 'qe-forward-paragraph)))
    (define-key map (kbd "P") (lambda () (qe-unit-ends-point-to-fcn 'qe-backward-paragraph)))
    (define-key map (kbd "b") (lambda () (qe-unit-ends-point-to-fcn 'qe-forward-block)))
    (define-key map (kbd "B") (lambda () (qe-unit-ends-point-to-fcn 'qe-backward-block)))
    (define-key map (kbd "w") 'qe-unit-ends-forward-word)
    (define-key map (kbd "W") 'qe-unit-ends-backward-word)
    (define-key map (kbd "m") 'qe-unit-ends-forward-matching)
    (define-key map (kbd "M") 'qe-unit-ends-backward-matching)
    (define-key map (kbd "e") (lambda () (qe-unit-ends-point-to-fcn 'end-of-line)))
    (define-key map (kbd "a") (lambda () (qe-unit-ends-point-to-fcn 'back-to-indentation)))
    (define-key map (kbd "A") (lambda () (qe-unit-ends-point-to-fcn 'beginning-of-line)))
    (define-key map (kbd "RET") (lambda () (qe-unit-ends-point-to-fcn 'forward-paragraph)))
    (define-key map (kbd "SPC") 'qe-unit-ends-mark)
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
  "Common keymap for unit kill/delete/copy.  Functions should return a cons
cell with beginning/end of region.  Original point position need not be
preserved.")

(defvar qe-unit-kill-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-w") 'qe-unit-ends-paragraph)
    (set-keymap-parent map qe-common-unit-map)
    map)
  "Keymap for unit kill/delete.  Parent keymap is `qe-unit-common-map', see
that variable for more information.")

(defun qe-unit-ends-point-to-fcn (fcn)
  (cons (point) (progn (funcall fcn) (point))))

(defun qe-unit-ends-forward-word ()
  (cons (point) (progn (skip-syntax-forward "w_") (point))))

(defun qe-unit-ends-backward-word ()
  (cons (point) (progn (skip-syntax-backward "w_") (point))))

(defun qe-unit-ends-forward-matching ()
  (let ((beg (point))
        (table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (with-syntax-table table
      (forward-sexp 1)
      (cons beg (point)))))

(defun qe-unit-ends-backward-matching ()
  (let ((beg (point))
        (table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (with-syntax-table table
      (forward-sexp -1)
      (cons beg (point)))))

(defun qe-unit-ends-mark ()
  (cons (point) (progn (goto-char (or (mark) (point))) (point))))

(defun qe-unit-ends-inside ()
  (message "Inside:")
  (let ((char (read-char)))
    (if (memq char '(?\( ?\) ?\[ ?\] ?\{ ?\} ?\< ?\>))
        (qe-region-inside-pair char 'inside)
      (if(memq char '(?\" ?\'))
          (qe-region-inside-quotes char dir)
        (error "Unknown char entered for kill inside text unit")))))

;; (let* ((ev (read-event "Key:"))
;;        (fcn (lookup-key qe-kill-unit-map (vector ev))))
;;   (when fcn
;;     (funcall fcn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun qe-copy-unit ()
  "Copy over a unit of text.
If the region is highlighted, it will be copied.  Otherwise, you are prompted
to enter another character which will cause the following:
p -- Copy forward paragraph (and trailing whitespace)
P -- Copy backward paragraph (and trailing whitespace)
b -- Copy forward block
B -- Copy backward block
m -- Copy forward over matching paren/brace/curly/angle-bracket/quotes
M -- Copy backward over matching paren/brace/curly/angle-bracket/quotes
w -- Copy foward word
W -- Copy backward word
e -- Copy forward to end of line
a -- Copy backward to beginning of indentation
A -- Copy backward to beginning of line
RET -- Copy forward to end of paragraph
SPC -- Copy to mark
\" -- Copy forward to double-quotes
\' -- Copy forward to single-quote
),],},> -- Copy forward to that char
\(,[,{,< -- Copy backward to that char
i -- Start copy inside paren/brace/curly/quotes/angle-bracket.  Another
  character must then be entered and all the text backward and forward
  inside the matching parens/brackets/curlies/quotes/angle-brackets is copied
/ -- Copy to a location using isearch
? -- Copy backward to a location using isearch
Anything else -- Copy line.  For example if this function is bound to M-w,
  a convenient way to copy the current line is M-w M-w"
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (message "Copy:")
    (let ((c (read-char)))
      (cond ((eq c ?/) (call-interactively 'qe-copy-to-isearch))
            ((eq c ??) (call-interactively 'qe-backward-copy-to-isearch))
            (t
             (let ((beg (point)) end dir)
               (save-excursion
                 (cond
                  ((eq c ?p) (qe-forward-paragraph))
                  ((eq c ?P) (qe-backward-paragraph))

                  ((eq c ?b) (qe-forward-block))
                  ((eq c ?B) (qe-backward-block))

                  ((eq c ?m)
                   (if (looking-at "</?\\s-*[a-zA-Z].*>")
                       (goto-char (match-end 0))
                     (forward-sexp 1)))
                  ((eq c ?M)
                   (if (looking-back "</?\\s-*[a-zA-Z].*>" (point-at-bol))
                       (goto-char (match-beginning 0))
                     (forward-sexp -1)))

                  ((eq c ?w) (skip-syntax-forward "w_"))
                  ((eq c ?W) (skip-syntax-backward"w_"))

                  ((eq c ?e) (end-of-line))
                  ((eq c ?a) (back-to-indentation))
                  ((eq c ?A) (beginning-of-line))

                  ((eq c ?) (forward-paragraph))

                  ((eq c ? ) (goto-char (or (mark) (point))))

                  ((not (memq c '(?i ?\( ?\) ?\[ ?\] ?\{ ?\} ?\< ?\> ?\" ?\')))
                   (setq beg (point-at-bol)) (forward-line) (beginning-of-line)))

                 (setq end (point)))

               (when (eq c ?i)
                 (message "Copy: i")
                 (setq c (read-char))
                 (setq dir 'inside))

               (cond ((memq c '(?\( ?\[ ?\{ ?\<))
                      (unless dir
                        (setq dir 'backward)))
                     ((memq c '(?\) ?\] ?\} ?\>))
                      (unless dir
                        (setq dir 'forward))))
               (when (memq c '(?\( ?\) ?\[ ?\] ?\{ ?\} ?\< ?\>))
                 (let ((region (qe-region-inside-pair c dir)))
                   (setq beg (car region)
                         end (cdr region))))

               (when (memq c '(?\" ?\'))
                 (unless dir
                   (setq dir 'forward))
                 (let ((region (qe-region-inside-quotes c dir)))
                   (setq beg (car region)
                         end (cdr region))))

               (unless (= beg end)
                 (qe-highlight beg end 'qe-copy-region-face)
                 (kill-ring-save beg end))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill/copy with isearch

(defvar qe-isearch-start nil)
(defvar qe-isearch-end nil)
(defvar qe-isearch-overlay nil)
(defvar qe-isearch-face nil)
(defvar qe-isearch-forward nil)

(defun qe-kill-to-isearch ()
  "Kill from point to somewhere else using isearch."
  (interactive "*")
  (qe-kill-or-copy-isearch t t))

(defun qe-backward-kill-to-isearch ()
  "Kill backward from point to somewhere else using isearch."
  (interactive "*")
  (qe-kill-or-copy-isearch t nil))

(defun qe-copy-to-isearch ()
  "Copy from point to somewhere else using isearch."
  (interactive)
  (qe-kill-or-copy-isearch nil t))

(defun qe-backward-copy-to-isearch ()
  "Copy backward from point to somewhere else using isearch."
  (interactive)
  (qe-kill-or-copy-isearch nil nil))

(defun qe-kill-or-copy-isearch (kill forward)
  "Kill or copy from point to somewhere else using isearch."
  (unwind-protect
      (save-excursion
        (setq qe-isearch-start (point))
        (setq qe-isearch-face (if kill
                                  'qe-kill-region-face
                                'qe-copy-region-face))
        (setq qe-isearch-forward forward)
        (when (if forward (isearch-forward) (isearch-backward))
          (if kill
              (kill-region qe-isearch-start qe-isearch-end)
            (copy-region-as-kill qe-isearch-start qe-isearch-end))))
    (setq qe-isearch-start nil)
    (when qe-isearch-overlay
      (delete-overlay qe-isearch-overlay))))

(defadvice isearch-highlight (after qe-iseach-add-overlay activate)
  (when qe-isearch-start
    (if qe-isearch-forward
        (setq qe-isearch-end (ad-get-arg 0))
      (setq qe-isearch-end (ad-get-arg 1)))
    (if qe-isearch-overlay
        (move-overlay qe-isearch-overlay qe-isearch-start qe-isearch-end (current-buffer))
      (setq qe-isearch-overlay (make-overlay qe-isearch-start qe-isearch-end)))
    (overlay-put qe-isearch-overlay 'face qe-isearch-face)))

(defadvice isearch-dehighlight (after qe-isearch-remove-overlay activate)
  (when qe-isearch-overlay
    (delete-overlay qe-isearch-overlay)))

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
  "Move forward a word section.
This is a utility function, you probably want `qe-forward-word-section'"
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
  "Move backward a word section.
This is a utility function, you probably want `qe-backward-word-section'."
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
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (cond ((or (eq char ?() (eq char ?)))
           (setq open  ?\( close ?\)))
          ((or (eq char ?\[) (eq char ?\]))
           (setq open  ?\[ close ?\]))
          ((or (eq char ?\{) (eq char ?\}))
           (setq open  ?\{ close ?\}))
          ((or (eq char ?\<) (eq char ?\>))
           (setq open  ?\< close ?\>)))
    (unless (equal dir 'forward)
      (setq beg (save-excursion
                  (with-syntax-table table
                    (catch 'done
                      (condition-case nil
                          (while t
                            (backward-up-list)
                            (when (= (char-after) char)
                              (throw 'done (point))))
                        (error nil))
                      (point-min))))))
    (unless (equal dir 'backward)
      (setq end (save-excursion
                  (with-syntax-table table
                    (catch 'done
                      (condition-case nil
                          (while t
                            (up-list)
                            (when (= (char-before) open)
                              (throw 'done (point))))
                        (error nil))
                      (point-max))))))
    (cons beg end)))

(defun qe-region-inside-quotes (char dir)
  "Find the region inside quote chars \"'"
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
