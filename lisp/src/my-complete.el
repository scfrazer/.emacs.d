;;; my-complete.el  -*- lexical-binding: t; -*-

(require 'selectrum)
(require 'selectrum-prescient)
(require 'orderless)
(require 'marginalia)
(require 'bookmark)
(require 'recentf)
(require 'project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion from other sources

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

(defun my-complete-switch-to-buffer ()
  "`switch-to-buffer', or choose from `recentf-list', or `project-find-file'"
  (interactive)
  (let (buffer-or-name filename)
    (let ((confirm-nonexistent-file-or-buffer nil))
      (setq buffer-or-name (read-buffer-to-switch "Switch to buffer: ")))
    (if (and buffer-or-name (get-buffer buffer-or-name))
        (switch-to-buffer buffer-or-name)
      (setq filename (my-complete-get-recentf-file nil buffer-or-name))
      (if (and filename
               (not (string= filename ""))
               (file-exists-p filename))
          (find-file filename)
        (let* ((pr (project-current t))
               (dirs (list (project-root pr)))
               (project-read-file-name-function #'my-complete-project--read-file-cpd-relative))
          (project-find-file-in buffer-or-name dirs pr))))))

(defun my-complete-get-recentf-file (&optional require-match initial-input)
  "Get a file from recentf file list."
  (let* ((file-alist (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) recentf-list))
         (choice-list (delete-dups (mapcar 'car file-alist)))
         (filename (completing-read "Find recent file: " (my-complete-presorted-completion-table choice-list) nil require-match initial-input))
         (result-list (delq nil (mapcar (lambda (x) (when (string= (car x) filename) (cdr x))) file-alist)))
         (result-length (length result-list)))
    (if (and filename (not (string= filename "")))
        (cond
         ((= result-length 0) filename)
         ((= result-length 1) (car result-list))
         (t (completing-read "Multiple matches: " result-list nil require-match initial-input)))
      filename)))

(defun my-complete-project--read-file-cpd-relative (prompt all-files &optional predicate hist default)
  "Like `project--read-file-cpd-relative' but DEFAULT becomes INITIAL-INPUT."
  (let* ((common-parent-directory
          (let ((common-prefix (try-completion "" all-files)))
            (if (> (length common-prefix) 0)
                (file-name-directory common-prefix))))
         (cpd-length (length common-parent-directory))
         (prompt (if (zerop cpd-length)
                     prompt
                   (concat prompt (format " in %s" common-parent-directory))))
         (substrings (mapcar (lambda (s) (substring s cpd-length)) all-files))
         (new-collection (project--file-completion-table substrings))
         (res (completing-read prompt new-collection predicate t default hist)))
    (concat common-parent-directory res)))

(defun my-complete-recentf-file ()
  "Find a file in the recently opened file list using completing-read."
  (interactive)
  (let ((filename (my-complete-get-recentf-file t)))
    (if filename
        (find-file filename)
      (call-interactively 'find-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion sorting

(defmacro my-complete-prescient--sort-compare ()
  "Same as `prescient--sort-compare', but do my own sorting."
  `(progn
     (let* ((p1 (gethash c1 hist len))
            (p2 (gethash c2 hist len)))
       (or (< p1 p2)
           (and (eq p1 p2)
                (let* ((f1 (gethash c1 freq 0))
                       (f2 (gethash c2 freq 0)))
                  (or (> f1 f2)
                      (and (eq f1 f2)
                           (let ((s1 (get-text-property 0 'score c1))
                                 (s2 (get-text-property 0 'score c2)))
                             (if (= s1 s2)
                                 (let ((length1 (length c1))
                                       (length2 (length c2)))
                                   (if (= length1 length2)
                                       (string< c1 c2)
                                     (< length1 length2)))
                               (> s1 s2)))))))))))

(defun my-complete-prescient-sort (candidates)
  "Same as `prescient-sort', but call my own sorting algorithm."
  (when (and prescient-persist-mode (not prescient--cache-loaded))
    (prescient--load))
  (let ((hist prescient--history)
        (len prescient-history-length)
        (freq prescient--frequency))
    (sort
     (mapcar #'my-complete-score-match candidates)
     (lambda (c1 c2)
       (my-complete-prescient--sort-compare)))))

(defun my-complete-score-match (str)
  "Score a completion match"
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

(defun my-complete-selectrum-prescient--preprocess (candidates)
  "Same as `selectrum-prescient--preprocess', but call my own sorting algorithm."
  (when selectrum-should-sort
    (setq candidates (my-complete-prescient-sort candidates)))
  candidates)

(defun my-selectrum-prescient-mode-hook ()
  "Swap in my own sorting."
  (if selectrum-prescient-mode
      (setq selectrum-preprocess-candidates-function #'my-complete-selectrum-prescient--preprocess)
    (when (eq selectrum-preprocess-candidates-function #'selectrum-prescient--preprocess)
      (setq selectrum-preprocess-candidates-function selectrum-prescient--old-preprocess-function))))

(add-hook 'selectrum-prescient-mode-hook #'my-selectrum-prescient-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion interface

(selectrum-mode 1)
(selectrum-prescient-mode 1)
(marginalia-mode)

(setq completion-styles '(orderless)
      marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)
      orderless-matching-styles '(orderless-prefixes)
      prescient-filter-method '(prefix)
      prescient-sort-length-enable nil
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      selectrum-count-style 'current/matches
      selectrum-highlight-candidates-function #'orderless-highlight-matches
      selectrum-refine-candidates-function #'orderless-filter)

(defun my-complete-selectrum-setup ()
  (when (and selectrum-is-active
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
  (if (and selectrum-is-active my-complete-showing-completions)
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
