;;; my-complete.el  -*- lexical-binding: t; -*-

(require 'selectrum)
(require 'selectrum-prescient)
(require 'orderless)
(require 'marginalia)
(require 'bookmark)
(require 'recentf)

(selectrum-mode 1)
(selectrum-prescient-mode 1)
(marginalia-mode)

(setq completion-styles '(orderless)
      marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)
      orderless-matching-styles '(orderless-prefixes orderless-literal)
      prescient-filter-method '(prefix literal)
      prescient-sort-length-enable nil
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      selectrum-count-style 'current/matches
      selectrum-highlight-candidates-function #'orderless-highlight-matches
      selectrum-refine-candidates-function #'orderless-filter)

(defun my-complete-presorted-completion-table (completions)
  "Keep completion table order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity)
                   (cycle-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))

(defun my-complete-find-file-from-bookmark ()
  "Find file starting from bookmark."
  (interactive)
  (let* ((bmk-list (bookmark-all-names))
         (name (completing-read "Use dir of bookmark: " (my-complete-presorted-completion-table bmk-list) nil t))
         bmk)
    (setq bmk (bookmark-get-bookmark name))
    (when bmk
      (setq bookmark-alist (delete bmk bookmark-alist))
      (push bmk bookmark-alist)
      (let ((filename (bookmark-get-filename bmk)) dir)
        (if (file-directory-p filename)
            (setq dir filename)
          (setq dir (file-name-directory filename)))
        (let ((default-directory dir))
          (call-interactively 'find-file))))))

(defun my-complete-recentf-file ()
  "Find a file in the recently opened file list using completing-read."
  (interactive)
  (let* ((file-alist (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) recentf-list))
         (choice-list (delete-dups (mapcar 'car file-alist)))
         (filename (completing-read "Find recent file: " (my-complete-presorted-completion-table choice-list) nil t))
         (result-list (delq nil (mapcar (lambda (x) (when (string= (car x) filename) (cdr x))) file-alist)))
         (result-length (length result-list)))
    (if (and filename (not (string= filename "")))
        (find-file
         (cond
          ((= result-length 0) filename)
          ((= result-length 1) (car result-list))
          (t (completing-read "Multiple matches: " result-list nil t))))
      (call-interactively 'find-file))))

;; (defun orderless-filter (string table &optional pred)
;;   "Split STRING into components and find entries TABLE matching all.
;; The predicate PRED is used to constrain the entries in TABLE."
;;   (condition-case nil
;;       (save-match-data
;;         (pcase-let* ((`(,prefix . ,pattern)
;;                       (orderless--prefix+pattern string table pred))
;;                      (completion-regexp-list
;;                       (funcall orderless-pattern-compiler pattern))
;;                      (completion-ignore-case
;;                       (if orderless-smart-case
;;                           (cl-loop for regexp in completion-regexp-list
;;                                    always (isearch-no-upper-case-p regexp t))
;;                         completion-ignore-case)))
;;           (sort (mapcar
;;                  #'my-complete-score-orderless-match
;;                  (all-completions prefix table pred))
;;                 #'my-complete-sort-orderless-matches)))
;;     (invalid-regexp nil)))

(defun my-complete-score-orderless-match (str)
  "Score an orderless match"
  (let ((score 0)
        (prev-regexp-start -1)
        regexp-start)
    (when (> (length completion-regexp-list) 1)
      (dolist (regexp completion-regexp-list)
        (setq regexp-start (string-match regexp str))
        (when (> regexp-start prev-regexp-start)
          (setq score (1+ score)))
        (setq prev-regexp-start regexp-start)))
    (propertize str 'score score)))

(defun my-complete-sort-orderless-matches (s1 s2)
  "Sort by score, then shortest length, then alphabetically"
  (let ((score1 (get-text-property 0 'score s1))
        (score2 (get-text-property 0 'score s2)))
    (if (= score1 score2)
        (let ((length1 (length s1))
              (length2 (length s2)))
          (if (= length1 length2)
              (string< s1 s2)
            (< length1 length2)))
      (> score1 score2))))

(defun my-complete-selectrum-setup ()
  (when (and selectrum-active-p
             minibuffer-completing-file-name)
    (let ((map (copy-keymap (current-local-map))))
      (define-key map (kbd "RET") 'my-complete-selectrum-select-current-candidate)
      (use-local-map map))))

(add-hook 'minibuffer-setup-hook 'my-complete-selectrum-setup 100)

(defun my-complete-selectrum-select-current-candidate (&optional arg)
  (interactive "P")
  (let* ((index (selectrum--index-for-arg arg))
         (candidate (selectrum--get-candidate index))
         (path (expand-file-name (substitute-in-file-name (selectrum--get-full candidate)))))
    (call-interactively
     (if (and (file-directory-p path)
              (not (eq minibuffer-completion-predicate
                       'file-directory-p)))
         'selectrum-insert-current-candidate
       'selectrum-select-current-candidate))))

(defvar my-complete-showing-completions nil)

(defun my-complete-completion-all-completions (orig-fun string table pred point &optional metadata)
  (if (and selectrum-active-p my-complete-showing-completions)
      (copy-sequence (selectrum-get-current-candidates t))
    (apply orig-fun (list string table pred point metadata))))

(advice-add 'completion-all-completions :around #'my-complete-completion-all-completions)

(defun my-complete-show-completions ()
  (interactive)
  (let ((marginalia-annotators nil)
        (my-complete-showing-completions t))
    (minibuffer-completion-help)))

(define-key selectrum-minibuffer-map (kbd "C-o") 'my-complete-show-completions)
(define-key selectrum-minibuffer-map (kbd "C-x C-n") 'other-window)
(define-key selectrum-minibuffer-map (kbd "C-x C-p") (lambda () (interactive (other-window -1))))
(define-key selectrum-minibuffer-map (kbd "M-N") 'selectrum-next-page)
(define-key selectrum-minibuffer-map (kbd "M-P") 'selectrum-previous-page)
(define-key selectrum-minibuffer-map (kbd "M-j") 'my-minibuffer-backward-kill)
(define-key selectrum-minibuffer-map (kbd "M-k") 'my-minibuffer-forward-kill)

(provide 'my-complete)
