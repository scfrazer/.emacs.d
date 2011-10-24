;;; quick-edit.el
;;
;; Copyright (C) 2011  Scott Frazer
;;
;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 01 Jan 2010
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

;; Control variables

(defvar qe-highlight-delay 0.5
  "*How long to highlight.")

(defvar qe-camelcase-sections t
  "*Sections can also be camelCase")

(make-local-variable 'qe-camelcase-sections)

(defvar qe-block-indented-modes (list 'emacs-lisp-mode
                                      'lisp-mode
                                      'scheme-mode
                                      'xml-mode
                                      'python-mode)
  "*Major modes that forward block should work by indentation")

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

;;; Utility motion functions

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

;; Utility region functions

(defun qe-region-inside-pair (char dir)
  "Find the region inside paired chars ()[]{}<>"
  (let* ((beg (point))
         (end beg)
         open close)
    (cond ((or (eq char ?() (eq char ?)))
           (setq open  ?\( close ?\)))
          ((or (eq char ?\[) (eq char ?\]))
           (setq open  ?\[ close ?\]))
          ((or (eq char ?\{) (eq char ?\}))
           (setq open  ?\{ close ?\}))
          ((or (eq char ?\<) (eq char ?\>))
           (setq open  ?\< close ?\>)))
    (save-excursion
      (cond ((equal dir 'backward)
             (setq beg (qe-backward-paired open close)))
            ((equal dir 'forward)
             (setq end (qe-forward-paired open close)))
            ((equal dir 'inside)
             (save-excursion (setq beg (qe-backward-paired open close)))
             (setq end (qe-forward-paired open close)))))
    (cons beg end)))

(defun qe-backward-paired (open close)
  (let ((nesting 0)
        (regex (concat "[" (char-to-string close) (char-to-string open) "]"))
        done)
    (while (not (or done (bobp)))
      (when (and (re-search-backward regex nil 'go)
                 (not (memq (get-text-property (point) 'face)
                            '(font-lock-comment-face font-lock-string-face))))
        (cond ((eq (char-after) close)
               (setq nesting (1+ nesting)))
              ((eq (char-after) open)
               (if (= nesting 0)
                   (setq done t)
                 (setq nesting (1- nesting)))))))
    (when done
      (forward-char)))
  (point))

(defun qe-forward-paired (open close)
  (let ((nesting 0)
        (regex (concat "[" (char-to-string close) (char-to-string open) "]"))
        done)
    (while (not (or done (eobp)))
      (when (and (re-search-forward regex nil 'go)
                 (not (memq (get-text-property (1- (point)) 'face)
                            '(font-lock-comment-face font-lock-string-face))))
        (cond ((eq (char-before) open)
               (setq nesting (1+ nesting)))
              ((eq (char-before) close)
               (if (= nesting 0)
                   (setq done t)
                 (setq nesting (1- nesting)))))))
    (when done
      (backward-char)))
  (point))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill/copy

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
  (interactive)
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
  (interactive)
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
  (interactive)
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
  (interactive)
  (kill-region (point)
               (progn
                 (skip-syntax-backward "_")
                 (qe-backward-section)
                 (point))))

(defun qe-kill-unit (&optional arg)
  "Kill over a unit of text.  With a prefix arg, delete instead of kill."
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (message "Kill:")
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

(defun qe-copy-unit ()
  "Copy over a unit of text."
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

(defun qe-kill-line (&optional arg)
  "Like kill-line, but use `qe-join-line-with-next' when at
end-of-line (and it's not a empty line).  Kills region if active."
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (or arg (not (eolp)) (bolp))
        (cond ((null arg)
               (kill-line))
              ((= arg 0)
               (kill-region (point) (progn (back-to-indentation) (point))))
              (t
               (kill-line arg)))
      (qe-join-line-with-next))))

(defun qe-join-line-with-next ()
  "Join current line with next."
  (interactive)
  (delete-indentation t)
  (just-one-space 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill/copy with isearch

(defvar qe-isearch-start nil)
(defvar qe-isearch-end nil)
(defvar qe-isearch-overlay nil)
(defvar qe-isearch-face nil)
(defvar qe-isearch-forward nil)

(defface qe-kill-region-face
  '((t (:inherit region)))
  "Face to highlight region that will be killed"
  :group 'faces)

(defface qe-copy-region-face
  '((t (:inherit region)))
  "Face to highlight region that will be copied"
  :group 'faces)

(defun qe-kill-to-isearch ()
  "Kill from point to somewhere else using isearch."
  (interactive)
  (qe-kill-or-copy-isearch t t))

(defun qe-backward-kill-to-isearch ()
  "Kill backward from point to somewhere else using isearch."
  (interactive)
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
;; Yank

(defun qe-yank (&optional arg)
  "Yank and indent."
  (interactive "P")
  (when (region-active-p)
    (kill-region (region-beginning) (region-end))
    (rotate-yank-pointer 1))
  (yank)
  (when (and (not arg) (not (member indent-line-function '(indent-relative sh-basic-indent-line))))
    (exchange-point-and-mark)
    (indent-region (point) (mark 't))
    (exchange-point-and-mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scrolling/Paging

(defun qe-scroll-down (n)
  "Scroll down without moving point (if possible)."
  (interactive "p")
  (let ((col (current-column)))
    (unless (pos-visible-in-window-p (point-max))
      (scroll-up n))
    (forward-line n)
    (unless (memq last-command (list 'next-line 'previous-line))
      (setq temporary-goal-column col))
    (move-to-column (truncate (if (consp temporary-goal-column)
                                  (+ (car temporary-goal-column)
                                     (cdr temporary-goal-column))
                                temporary-goal-column)))
    (setq this-command 'next-line)))

(defun qe-scroll-up (n)
  "Scroll up without moving point (if possible)."
  (interactive "p")
  (let ((col (current-column)))
    (unless (pos-visible-in-window-p (point-min))
      (scroll-down n))
    (forward-line (- 0 n))
    (unless (memq last-command (list 'next-line 'previous-line))
      (setq temporary-goal-column col))
    (move-to-column (truncate (if (consp temporary-goal-column)
                                  (+ (car temporary-goal-column)
                                     (cdr temporary-goal-column))
                                temporary-goal-column)))
    (setq this-command 'previous-line)))

(defun qe-page-down ()
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
    (move-to-column (truncate (if (consp temporary-goal-column)
                                  (+ (car temporary-goal-column)
                                     (cdr temporary-goal-column))
                                temporary-goal-column)))
    (setq this-command 'next-line)))

(defun qe-page-up ()
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
    (move-to-column (truncate (if (consp temporary-goal-column)
                                  (+ (car temporary-goal-column)
                                     (cdr temporary-goal-column))
                                temporary-goal-column)))
    (setq this-command 'previous-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight

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
