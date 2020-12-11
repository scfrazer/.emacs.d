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
      orderless-matching-styles '(orderless-prefixes orderless-regexp)
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)
      marginalia-truncate-width 1000
      selectrum-count-style 'current/matches
      selectrum-refine-candidates-function #'orderless-filter
      selectrum-highlight-candidates-function #'orderless-highlight-matches)

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

(define-key selectrum-minibuffer-map (kbd "M-N") 'selectrum-next-page)
(define-key selectrum-minibuffer-map (kbd "M-P") 'selectrum-previous-page)
(define-key selectrum-minibuffer-map (kbd "M-j") 'my-minibuffer-backward-kill)
(define-key selectrum-minibuffer-map (kbd "M-k") 'my-minibuffer-forward-kill)

;; (setq-default icomplete-compute-delay 0.2
;;               icomplete-delay-completions-threshold 100
;;               icomplete-max-delay-chars 2
;;               icomplete-tidy-shadowed-file-names t)
;;
;; (define-key icomplete-minibuffer-map (kbd "<down>") 'icomplete-forward-completions)
;; (define-key icomplete-minibuffer-map (kbd "<up>")   'icomplete-backward-completions)
;; (define-key icomplete-minibuffer-map (kbd "C-j")    'exit-minibuffer)
;; (define-key icomplete-minibuffer-map (kbd "C-n")    'icomplete-forward-completions)
;; (define-key icomplete-minibuffer-map (kbd "C-p")    'icomplete-backward-completions)
;;
;; (defun my-complete--fido-mode-setup ()
;;   "Customize icomplete fido mode."
;;   (setq-local completion-styles '(orderless)))
;;
;; (advice-add #'icomplete--fido-mode-setup :after #'my-complete--fido-mode-setup)
;;
;; (defun my-complete-vertical-format-completions (reformatted)
;;   "Customize completions format."
;;   (save-match-data
;;     (if (string-match "[[]Matched[]]" reformatted)
;;         (progn
;;           (add-face-text-property (match-beginning 0) (match-end 0) 'success nil reformatted)
;;           (setq reformatted (concat " " reformatted)))
;;       (when (string-match "[(]No matches[)]" reformatted)
;;         (add-face-text-property (match-beginning 0) (match-end 0) 'error nil reformatted)
;;         (setq reformatted (concat " " reformatted)))))
;;   reformatted)
;;
;; (advice-add #'icomplete-vertical-format-completions :filter-return #'my-complete-vertical-format-completions)
;;
;; (defun my-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
;;   "Like `completing-read', but don't sort COLLECTION"
;;   (completing-read prompt (my-complete-presorted-completion-table collection) predicate require-match initial-input hist def inherit-input-method))
;;
;; (icomplete-vertical-mode 1)
;; (fido-mode 1)

(provide 'my-complete)
