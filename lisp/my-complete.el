;;; my-complete.el  -*- lexical-binding: t; -*-

(require 'vertico)
(require 'orderless)
(require 'marginalia)
(require 'bookmark)
(require 'recentf)
(require 'project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion from other sources

(defvar my-complete-category nil)
(defvar my-complete-collection nil)

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
      (setq buffer-or-name (my-complete-read-buffer-to-switch)))
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

(defun my-complete-read-buffer-to-switch ()
  "Like `read-buffer-to-switch', but you can kill buffers with C-k"
  (interactive)
  (setq my-complete-category 'buffer)
  (setq my-complete-collection nil)
  (let (buf-name)
    (dolist (buf (buffer-list))
      (setq buf-name (buffer-name buf))
      (unless (my-buf-ignore-buffer buf-name)
        (push buf-name my-complete-collection))))
  (completing-read "Switch to buffer: "
                   (lambda (str pred action)
                     (complete-with-action action my-complete-collection str pred))))

(defun my-complete-get-recentf-file (&optional require-match initial-input)
  "Get a file from recentf file list."
  (let* ((my-complete-category 'recentf)
         (file-alist (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) recentf-list))
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

(defun my-complete-vertico-exit ()
  "Exit completion with current candidate or insert directory."
  (interactive)
  (if (and minibuffer-completing-file-name
           (>= vertico--index 0)
           (string-suffix-p "/" (vertico--candidate)))
      (vertico-insert)
    (vertico-exit)))

(defun my-complete-vertico-kill ()
  "Kill what the current candidate is pointing to."
  (interactive)
  (let ((item (vertico--candidate)))
    (cond ((eq my-complete-category 'buffer)
           (kill-buffer item))
          ((eq my-complete-category 'recentf)
           (my-complete-vertico-kill-recentf item))
          (minibuffer-completing-file-name
           (my-complete-vertico-kill-file item)))
    (setq my-complete-collection (delete item my-complete-collection))
    (setq vertico--input t)))

(defun my-complete-vertico-kill-recentf (filename)
  "Remove filename from the recentf list."
  (setq recentf-list (delq t (mapcar
                              (lambda (x) (or (equal x filename)
                                              (equal (file-name-nondirectory x) filename)
                                              x))
                              recentf-list))))

(defun my-complete-vertico-kill-file (filename)
  "Delete filename."
  ;; TODO
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion interface

(vertico-mode 1)
(marginalia-mode)

(setq completion-category-defaults nil
      completion-styles '(orderless)
      orderless-matching-styles '(orderless-literal)
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(define-key vertico-map (kbd "C-j") #'vertico-exit-input)
(define-key vertico-map (kbd "C-k") #'my-complete-vertico-kill)
(define-key vertico-map (kbd "C-o") (lambda () (interactive)
                                      (marginalia-mode -1)
                                      (call-interactively #'minibuffer-completion-help)
                                      (marginalia-mode +1)))
(define-key vertico-map (kbd "RET") #'my-complete-vertico-exit)

(provide 'my-complete)
