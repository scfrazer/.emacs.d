;;; motion-and-kill-dwim.el --- Motion and kill DWIM commands

;; Copyright (C) 2007  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 01 Aug 2007
;; Version: 1.3
;; Keywords: motion kill dwim

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

;; A set of motion and kill do-what-I-mean commands, plus a minor-mode.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions

(defun makd-mark-active ()
  "Is region active."
  (and transient-mark-mode mark-active))

(defun makd-dotimes (n form)
  "Optionally do a form a number of times."
  (unless n (setq n 1))
  (while (> n 0)
    (eval form)
    (setq n (1- n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control variables

(defvar makd-camelcase-sections t
  "*Sections can also be camelCase")

(make-local-variable 'makd-camelcase-sections)

(defvar makd-highlight-delay 0.5
  "*How long to highlight.")

(defvar makd-block-indented-modes (list 'emacs-lisp-mode
                                        'lisp-mode
                                        'scheme-mode
                                        'xml-mode
                                        'python-mode)
  "*Major modes that forward block should work by indentation")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Motion

(defun makd-forward-word (&optional n)
  "Like forward-word, but stops at beginning of words.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(progn (when (looking-at "\\(\\sw\\|\\s_\\)")
                            (skip-syntax-forward "w_"))
                          (skip-syntax-forward "^w_"))))

(defun makd-forward-word-section (&optional n)
  "Like forward-word, but only goes over alphanumerics.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(progn
                     (makd-forward-section)
                     (skip-chars-forward "^a-zA-Z0-9"))))

(defun makd-forward-word-end (&optional n)
  "Forward to end of word.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(progn (unless (looking-at "\\(\\sw\\|\\s_\\)")
                            (skip-syntax-forward "^w_"))
                          (skip-syntax-forward "w_"))))

(defun makd-forward-to-char (n char)
  "Move forward to CHAR.
With argument, do this that many times"
  (interactive "p\ncForward to char: ")
  (unless (and n (integerp n) (> n 0)) (setq n 1))
  (let ((case-fold-search nil))
    (forward-char)
    (search-forward (char-to-string char) nil nil n))
  (backward-char))

(defun makd-forward-paragraph (&optional n)
  "Like forward-paragraph, but goes to next non-blank line.
With argument, do this that many times"
  (interactive "p")
  (beginning-of-line)
  (makd-dotimes n '(progn (when (re-search-forward "^\\s-*$" nil 'go)
                            (re-search-forward "[^ \t\f\n]" nil 'go)
                            (beginning-of-line)))))

(defun makd-forward-block (&optional n)
  "Goes forward to end of next curly-bracket or indented block
depending on the major mode (see `makd-block-indented-modes').
With argument, do this that many times"
  (interactive "p")
  (if (memq major-mode makd-block-indented-modes)
      (makd-forward-indented-block n)
    (makd-forward-curly-block n)))

(defun makd-forward-curly-block (&optional n)
  "Goes forward to end of next curly-bracket block.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(progn (let (done)
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
                                (setq done t)))))))

(defun makd-forward-indented-block (&optional n)
  "Goes forward to next line at the same or less indentation.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(progn (beginning-of-line)
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
                                (setq done t)))))))

(defun makd-backward-word (&optional n)
  "Like backward-word, but stops at beginning of words.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(progn (unless (looking-back "\\(\\sw\\|\\s_\\)")
                            (skip-syntax-backward "^w_"))
                          (skip-syntax-backward "w_"))))

(defun makd-backward-word-section (&optional n)
  "Like backward-word, but only goes over alphanumerics.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(progn
                     (skip-chars-backward "^a-zA-Z0-9")
                     (makd-backward-section))))

(defun makd-backward-word-end (&optional n)
  "Backward to end of word.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(progn (when (looking-back "\\(\\sw\\|\\s_\\)")
                            (skip-syntax-backward "w_"))
                          (skip-syntax-backward "^w_"))))

(defun makd-backward-to-char (n char)
  "Move backward to CHAR.
With argument, do this that many times"
  (interactive "p\ncBackward to char: ")
  (unless (and n (integerp n) (> n 0)) (setq n 1))
  (let ((case-fold-search nil))
    (search-backward (char-to-string char) nil nil n)))

(defun makd-backward-paragraph (&optional n)
  "Go to first line after previous blank line.
With argument, do this that many times"
  (interactive "p")
  (beginning-of-line)
  (makd-dotimes n '(progn (re-search-backward "[^ \t\f\n]" nil 'go)
                          (when (re-search-backward "^\\s-*$" nil 'go)
                            (forward-line 1))
                          (beginning-of-line))))

(defun makd-backward-block (&optional n)
  "Goes backward to beginning of next curly-bracket or indented block
depending on the major mode (see `makd-block-indented-modes').
With argument, do this that many times"
  (interactive "p")
  (if (memq major-mode makd-block-indented-modes)
      (makd-backward-indented-block n)
    (makd-backward-curly-block n)))

(defun makd-backward-curly-block (&optional n)
  "Goes backward to beginning of next curly-bracket block.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(progn (let (done)
                            (while (not (or done (bobp)))
                              (when (and (re-search-backward "[{}]" nil 'go)
                                         (not (memq (get-text-property (point) 'face)
                                                    '(font-lock-comment-face font-lock-string-face))))
                                (when (equal (char-after) ?})
                                  (forward-char)
                                  (backward-sexp))
                                (beginning-of-line)
                                (setq done t)))))))

(defun makd-backward-indented-block (&optional n)
  "Goes backward to beginning of line at the same or less indentation.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(progn (beginning-of-line)
                          (when (looking-at "\\s-*$")
                            (re-search-backward "[^ \t\f\n]" nil 'go))
                          (back-to-indentation)
                          (let ((col (current-column)) done)
                            (while (not (or done (bobp)))
                              (forward-line -1)
                              (back-to-indentation)
                              (when (and (<= (current-column) col) (not (looking-at "$")))
                                (setq done t)
                                (beginning-of-line)))))))

;;; Utility motion functions

(defun makd-forward-section ()
  "Move forward a word section.
This is a utility function, you probably want `makd-forward-word-section'"
  (let ((case-fold-search nil))
    (if makd-camelcase-sections
        (if (looking-at "[a-z0-9]")
            (skip-chars-forward "a-z0-9")
          (when (looking-at "[A-Z]")
            (forward-char)
            (if (looking-at "[a-z]")
                (skip-chars-forward "a-z0-9")
              (skip-chars-forward "A-Z0-9"))))
      (skip-chars-forward "a-zA-Z0-9"))))

(defun makd-backward-section ()
  "Move backward a word section.
This is a utility function, you probably want `makd-backward-word-section'."
  (let ((case-fold-search nil))
    (if makd-camelcase-sections
        (progn
          (skip-chars-backward "0-9")
          (if (looking-back "[A-Z]")
              (skip-chars-backward "A-Z0-9")
            (when (looking-back "[a-z]")
                (skip-chars-backward "a-z0-9")
                (when (looking-back "[A-Z]")
                  (backward-char)))))
      (skip-chars-backward "a-zA-Z0-9"))))

;; Utility region functions

(defun makd-region-inside-pair (char dir)
  "Find the region inside paired chars ()[]{}<>"
  (let* ((beg (point))
         (end beg)
         regex open close)
    (cond ((or (eq char ?() (eq char ?)))
           (setq regex "[()]"
                 open  ?\(
                 close ?\)))
          ((or (eq char ?\[) (eq char ?\]))
           (setq regex "[][]"
                 open  ?\[
                 close ?\]))
          ((or (eq char ?\{) (eq char ?\}))
           (setq regex "[{}]"
                 open  ?\{
                 close ?\}))
          ((or (eq char ?\<) (eq char ?\>))
           (setq regex "[<>]"
                 open  ?\<
                 close ?\>)))
    (save-excursion
      (cond ((equal dir 'backward)
             (setq beg (makd-backward-paired regex open close)))
            ((equal dir 'forward)
             (setq end (makd-forward-paired regex open close)))
            ((equal dir 'inside)
             (save-excursion (setq beg (makd-backward-paired regex open close)))
             (setq end (makd-forward-paired regex open close)))))
    (cons beg end)))

(defun makd-backward-paired (regex open close)
  (let (done (nesting 0))
    (while (not (or done (bobp)))
      (when (and (re-search-backward regex nil 'go)
                 (not (memq (get-text-property (point) 'face)
                            '(font-lock-comment-face font-lock-string-face))))
        (cond ((eq (char-after) close)
               (setq nesting (1+ nesting)))
              ((eq (char-after) open)
               (if (= nesting 0)
                   (setq done t)
                 (setq nesting (1- nesting))))))))
  (unless (bobp)
    (forward-char))
  (point))

(defun makd-forward-paired (regex open close)
  (let (done (nesting 0))
    (while (not (or done (eobp)))
      (when (and (re-search-forward regex nil 'go)
                 (not (memq (get-text-property (point) 'face)
                            '(font-lock-comment-face font-lock-string-face))))
        (cond ((eq (char-before) open)
               (setq nesting (1+ nesting)))
              ((eq (char-before) close)
               (if (= nesting 0)
                   (setq done t)
                 (setq nesting (1- nesting))))))))
  (unless (eobp)
    (backward-char))
  (point))

(defun makd-region-inside-quotes (char dir)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill

(defun makd-forward-kill (&optional n)
  "Smart kill forward.
1. If region is active, kill it
2. Else if at the beginning of a word, kill the word and trailing whitespace
3. Else if in the middle of a word, kill the rest of the word
4. Else if looking at whitespace, kill whitespace forward
5. Else if looking at punctuation, kill punctuation forward
6. Else if looking at an open bracket/brace/paren, kill sexp forward
7. Else if looking at a quotation mark, kill quoted text
8. Else kill next char
With argument, do this that many times"
  (interactive "p")
  (if (makd-mark-active)
      (kill-region (region-beginning) (region-end))
    (makd-dotimes n '(kill-region (point)
                                  (progn
                                    (cond ((looking-at "\\<\\(\\sw\\|\\s_\\)")
                                           (skip-syntax-forward "w_")
                                           (skip-syntax-forward " "))
                                          ((looking-at "\\(\\sw\\|\\s_\\)")
                                           (skip-syntax-forward "w_"))
                                          ((looking-at "\\s ")
                                           (skip-syntax-forward " "))
                                          ((looking-at "\\s.")
                                           (skip-syntax-forward "."))
                                          ((looking-at "\\s(")
                                           (forward-sexp))
                                          ((looking-at "\\s\"")
                                           (forward-char)
                                           (skip-syntax-forward "^\"")
                                           (forward-char))
                                          (t
                                           (forward-char)))
                                    (point))))))

(defun makd-forward-kill-section (&optional n)
  "Forward kill pieces of words.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(kill-region (point)
                                (progn
                                  (makd-forward-section)
                                  (while (looking-at "\\s_")
                                    (forward-char))
                                  (point)))))

(defun makd-backward-kill (&optional n)
  "Smart kill backward.
1. If region is active, kill it
2. Else if looking back at whitespace, kill backward whitespace and word
3. Else if in the middle of a word, kill backward word
4. Else if looking at punctuation, kill backward punctuation
5. Else if looking at an close bracket/brace/paren, kill backward sexp
6. Else if looking at a quotation mark, kill backward quoted text
7. Else kill previous char
With argument, do this that many times"
  (interactive "p")
  (if (makd-mark-active)
      (kill-region (region-beginning) (region-end))
    (makd-dotimes n '(kill-region (point)
                                  (progn
                                    (cond ((looking-back "\\s ")
                                           (skip-syntax-backward " ")
                                           (when (looking-back "\\(\\sw\\|\\s_\\)")
                                             (skip-syntax-backward "w_")))
;;                                           ((looking-back "\\(\\sw\\|\\s_\\)\\>")
;;                                            (skip-syntax-backward "w_")
;;                                            (unless (looking-back "^\\s +")
;;                                              (skip-syntax-backward " ")))
                                          ((looking-back "\\(\\sw\\|\\s_\\)")
                                           (skip-syntax-backward "w_"))
                                          ((looking-back "\\s.")
                                           (skip-syntax-backward "."))
                                          ((looking-back "\\s)")
                                           (backward-sexp))
                                          ((looking-back "\\s\"")
                                           (backward-char)
                                           (skip-syntax-backward "^\"")
                                           (backward-char))
                                          (t
                                           (backward-char)))
                                    (point))))))

(defun makd-backward-kill-section (&optional n)
  "Backward kill pieces of words.
With argument, do this that many times"
  (interactive "p")
  (makd-dotimes n '(kill-region (point)
                                (progn
                                  (while (looking-back "\\s_")
                                    (backward-char))
                                  (makd-backward-section)
                                  (point)))))

;; Kill to char

(defun makd-kill-to-char (n char)
  "Kill up to CHAR.
With argument, do this that many times"
  (interactive "p\ncKill to char: ")
  (unless (and n (integerp n) (> n 0)) (setq n 1))
  (let ((case-fold-search nil))
    (kill-region (point) (progn
                           (forward-char)
                           (search-forward (char-to-string char) nil nil n)
                           (backward-char)
                           (point)))))

(defun makd-backward-kill-to-char (n char)
  "Kill backwards up to CHAR.
With argument, do this that many times"
  (interactive "p\ncBackward kill to char: ")
  (unless (and n (integerp n) (> n 0)) (setq n 1))
  (let ((case-fold-search nil))
    (kill-region (point) (progn
                           (search-backward (char-to-string char) nil nil n)
                           (forward-char)
                           (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill/copy with isearch

(defvar makd-isearch-start nil)
(defvar makd-isearch-end nil)
(defvar makd-isearch-overlay nil)
(defvar makd-isearch-face nil)

(defface makd-kill-region-face
  '((t (:inherit region)))
  "Face to highlight region that will be killed"
  :group 'faces)

(defface makd-copy-region-face
  '((t (:inherit region)))
  "Face to highlight region that will be copied"
  :group 'faces)

(defun makd-kill-to-isearch ()
  "Kill from point to somewhere else using isearch."
  (interactive)
  (unwind-protect
      (save-excursion
        (setq makd-isearch-start (point))
        (setq makd-isearch-face 'makd-kill-region-face)
        (when (isearch-forward)
          (kill-region makd-isearch-start makd-isearch-end)))
    (setq makd-isearch-start nil)
    (when makd-isearch-overlay
      (delete-overlay makd-isearch-overlay))))

(defun makd-copy-to-isearch ()
  "Copy from point to somewhere else using isearch."
  (interactive)
  (unwind-protect
      (save-excursion
        (setq makd-isearch-start (point))
        (setq makd-isearch-face 'makd-copy-region-face)
        (when (isearch-forward)
          (copy-region-as-kill makd-isearch-start makd-isearch-end)))
    (setq makd-isearch-start nil)
    (when makd-isearch-overlay
      (delete-overlay makd-isearch-overlay))))

(defadvice isearch-highlight (after makd-iseach-add-overlay activate)
  (when makd-isearch-start
    (setq makd-isearch-end (ad-get-arg 0))
    (if makd-isearch-overlay
        (move-overlay makd-isearch-overlay makd-isearch-start makd-isearch-end (current-buffer))
      (setq makd-isearch-overlay (make-overlay makd-isearch-start makd-isearch-end)))
    (overlay-put makd-isearch-overlay 'face makd-isearch-face)))

(defadvice isearch-dehighlight (after makd-isearch-remove-overlay activate)
  (when makd-isearch-overlay
    (delete-overlay makd-isearch-overlay)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy/kill/yank/indent/open

(defun makd-copy-region-as-kill ()
  "When called interactively with no active region, copy current line."
  (interactive)
  (let ((beg (if (makd-mark-active) (region-beginning) (point-at-bol)))
        (end (if (makd-mark-active) (region-end) (point-at-bol 2))))
    (copy-region-as-kill beg end)))

(defun makd-kill-region ()
  "When called interactively with no active region, kill current line."
  (interactive)
  (let ((beg (if (makd-mark-active) (region-beginning) (point-at-bol)))
        (end (if (makd-mark-active) (region-end) (point-at-bol 2))))
    (kill-region beg end)))

(defun makd-yank (&optional arg)
  "Yank and indent."
  (interactive "P")
  (when (makd-mark-active)
    (kill-region (region-beginning) (region-end))
    (rotate-yank-pointer 1))
  (yank)
  (when (and (not arg) (not (member indent-line-function '(indent-relative sh-basic-indent-line))))
    (exchange-point-and-mark)
    (indent-region (point) (mark 't))
    (exchange-point-and-mark)))

(defun makd-indent ()
  "Indent the region if it is active, otherwise normal indent."
  (interactive)
  (if (makd-mark-active)
      (indent-region (region-beginning) (region-end))
    (indent-according-to-mode)))

(defun makd-open-line-above ()
  "Open a line above the current one."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1))

(defun makd-open-line-below ()
  "Open a line below the current one."
  (interactive)
  (end-of-line)
  (newline))

(defun makd-kill-over-motion ()
  "Kill over a motion command"
  (interactive)
  (if (makd-mark-active)
      (kill-region (region-beginning) (region-end))
    (message "Kill:")
    (let ((c (read-char)) dir)
      (cond
       ((eq c ?/) (call-interactively 'makd-kill-to-isearch))

       ((eq c ?s) (makd-forward-kill-section))
       ((eq c ?S) (makd-backward-kill-section))

       ((eq c ?w) (makd-forward-kill))
       ((eq c ?W) (makd-backward-kill))

       ((eq c ?b) (kill-region (point) (progn (makd-forward-block) (point))))
       ((eq c ?B) (kill-region (point) (progn (makd-backward-block) (point))))

       ((eq c ?p) (kill-region (point) (progn (makd-forward-paragraph) (point))))
       ((eq c ?P) (kill-region (point) (progn (makd-backward-paragraph) (point))))

       ((eq c ?.) (kill-region (point) (progn (makd-forward-word-end) (point))))
       ((eq c ?,) (kill-region (point) (progn (makd-backward-word-end) (point))))

       ((eq c ?a) (kill-region (point) (point-at-bol)))
       ((eq c ?e) (kill-line))

       ((eq c ?m) (kill-sexp 1))
       ((eq c ?M) (kill-sexp -1))

       ((eq c ?A) (kill-region (point) (progn (back-to-indentation) (point))))

       ((not (memq c '(?i ?\( ?\) ?\[ ?\] ?\{ ?\} ?\< ?\> ?\" ?\')))
        (kill-region (point-at-bol) (point-at-bol 2))))

      (when (eq c ?i)
        (message "Kill: i")
        (setq c (read-char))
        (setq dir 'inside))

      (cond ((memq c '(?\( ?\[ ?\{ ?\<))
             (unless dir
               (setq dir 'backward)))
            ((memq c '(?\) ?\] ?\} ?\>))
             (unless dir
               (setq dir 'forward))))
      (when (memq c '(?\( ?\) ?\[ ?\] ?\{ ?\} ?\< ?\>))
        (let ((region (makd-region-inside-pair c dir)))
          (kill-region (car region) (cdr region))))

      (when (memq c '(?\" ?\'))
        (unless dir
          (setq dir 'forward))
        (let ((region (makd-region-inside-quotes c dir)))
          (kill-region (car region)(cdr region)))))))

(defun makd-copy-over-motion ()
  "Copy over a motion command"
  (interactive)
  (if (makd-mark-active)
      (kill-ring-save (region-beginning) (region-end))
    (message "Copy:")
    (let ((c (read-char)))
      (if (eq c ?/)
          (call-interactively 'makd-copy-to-isearch)
        (let ((beg (point)) end dir)
          (save-excursion
            (cond
             ((eq c ?s) (makd-forward-section))
             ((eq c ?S) (makd-backward-section))

             ((eq c ?w) (makd-forward-word))
             ((eq c ?W) (makd-backward-word))

             ((eq c ?p) (makd-forward-paragraph))
             ((eq c ?P) (makd-backward-paragraph))

             ((eq c ?b) (makd-forward-block))
             ((eq c ?B) (makd-backward-block))

             ((eq c ?.) (makd-forward-word-end))
             ((eq c ?,) (makd-backward-word-end))

             ((eq c ?a) (beginning-of-line))
             ((eq c ?e) (end-of-line))

             ((eq c ?A) (back-to-indentation))

             ((eq c ?m) (forward-sexp 1))
             ((eq c ?M) (forward-sexp -1))

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
            (let ((region (makd-region-inside-pair c dir)))
              (setq beg (car region)
                    end (cdr region))))

          (when (memq c '(?\" ?\'))
            (unless dir
              (setq dir 'forward))
            (let ((region (makd-region-inside-quotes c dir)))
              (setq beg (car region)
                    end (cdr region))))

          (unless (= beg end)
            (makd-highlight beg end 'makd-copy-region-face)
            (kill-ring-save beg end)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select

(defun makd-select-word-at-point ()
  "Select the word at point."
  (interactive)
  (skip-syntax-forward "w_")
  (push-mark (point))
  (skip-syntax-backward "w_")
  (exchange-point-and-mark))

(defun makd-select-section-at-point ()
  "Select the section at point."
  (interactive)
  (skip-syntax-forward "w")
  (push-mark (point))
  (skip-syntax-backward "w")
  (exchange-point-and-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search/Replace

(defun makd-highlight (beg end &optional face)
  "Highlight a region temporarily."
  (unless face
    (setq face 'isearch))
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'face face)
      (sit-for makd-highlight-delay)
      (delete-overlay ov)))

(defvar makd-search-wrap nil
  "Internal search-wrap variable")

(defun makd-search-forward ()
  "Search forward."
  (interactive)
  (when (makd-mark-active)
    (isearch-update-ring (buffer-substring-no-properties (region-beginning) (region-end)))
    (deactivate-mark))
  (when (car search-ring)
    (cond ((search-forward (car search-ring) nil t)
           (makd-highlight (match-beginning 0) (match-end 0))
           (setq makd-search-wrap nil))
          ((and makd-search-wrap (equal last-command 'makd-search-forward))
           (setq makd-search-wrap nil)
           (if (save-excursion (search-backward (car search-ring) nil t))
               (progn
                 (goto-char (point-min))
                 (makd-search-forward))
             (beep)
             (message "No more matches")))
          (t
           (beep)
           (message "No more matches")
           (setq makd-search-wrap t)))))

(defun makd-search-backward ()
  "Search backward."
  (interactive)
  (when (makd-mark-active)
    (isearch-update-ring (buffer-substring-no-properties (region-beginning) (region-end)))
    (goto-char (region-beginning))
    (deactivate-mark))
  (when (car search-ring)
    (cond ((search-backward (car search-ring) nil t)
           (makd-highlight (match-beginning 0) (match-end 0))
           (setq makd-search-wrap nil))
          ((and makd-search-wrap (equal last-command 'makd-search-backward))
           (setq makd-search-wrap nil)
           (if (save-excursion (search-forward (car search-ring) nil t))
               (progn
                 (goto-char (point-max))
                 (makd-search-backward))
             (beep)
             (message "No more matches")))
          (t
           (beep)
           (message "No more matches")
           (setq makd-search-wrap t)))))

(defun makd-query-replace ()
  "query-replace ... take from-string from region if it is active"
  (interactive "*")
  (if (makd-mark-active)
      (let* ((from (buffer-substring (region-beginning) (region-end)))
             (to (read-from-minibuffer (format "Query replace %s with: " from) nil nil nil 'query-replace-history)))
        (goto-char (region-beginning))
        (setq mark-active nil)
        (query-replace from to))
    (call-interactively 'query-replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scrolling/Paging

(defun makd-scroll-down (n)
  "Scroll down without moving point (if possible)."
  (interactive "p")
  (let ((col (current-column)))
    (unless (pos-visible-in-window-p (point-max))
      (scroll-up n))
    (forward-line n)
    (unless (memq last-command (list 'next-line 'previous-line))
      (setq temporary-goal-column col))
    (move-to-column (truncate temporary-goal-column))
    (setq this-command 'next-line)))

(defun makd-scroll-up (n)
  "Scroll up without moving point (if possible)."
  (interactive "p")
  (let ((col (current-column)))
    (unless (pos-visible-in-window-p (point-min))
      (scroll-down n))
    (forward-line (- 0 n))
    (unless (memq last-command (list 'next-line 'previous-line))
      (setq temporary-goal-column col))
    (move-to-column (truncate temporary-goal-column))
    (setq this-command 'previous-line)))

(defun makd-page-down ()
  "Page down and keep column."
  (interactive)
  (let ((col (current-column))
        (lines (- (window-height) 2)))
    (if (pos-visible-in-window-p (point-max))
        (goto-char (point-max))
      (save-excursion
        (goto-char (window-start))
        (forward-line lines)
        (set-window-start (selected-window) (point)))
      (forward-line lines)
      (when (eobp)
        (recenter -1)))
    (unless (memq last-command (list 'next-line 'previous-line))
      (setq temporary-goal-column col))
    (move-to-column (truncate temporary-goal-column))
    (setq this-command 'next-line)))

(defun makd-page-up ()
  "Page up and keep column."
  (interactive)
  (let ((col (current-column))
        (lines (- 2 (window-height))))
    (if (pos-visible-in-window-p (point-min))
        (goto-char (point-min))
      (save-excursion
        (goto-char (window-start))
        (forward-line lines)
        (set-window-start (selected-window) (point)))
      (forward-line lines))
    (unless (memq last-command (list 'next-line 'previous-line))
      (setq temporary-goal-column col))
    (move-to-column (truncate temporary-goal-column))
    (setq this-command 'previous-line)))

;;; Done

(provide 'motion-and-kill-dwim)
;;; motion-and-kill-dwim.el ends here
