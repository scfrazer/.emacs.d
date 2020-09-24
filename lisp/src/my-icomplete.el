;;; my-icomplete.el  -*- lexical-binding: t; -*-

(require 'icomplete)
(require 'orderless)
(require 'icomplete-vertical)
(require 'bookmark)
(require 'recentf)

(setq-default completion-styles '(orderless)
              icomplete-compute-delay 0.2
              icomplete-delay-completions-threshold 100
              icomplete-max-delay-chars 2
              icomplete-tidy-shadowed-file-names t
              orderless-matching-styles '(orderless-prefixes)
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t)

(define-key icomplete-minibuffer-map (kbd "<down>") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<up>")   'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-j")    'exit-minibuffer)
(define-key icomplete-minibuffer-map (kbd "C-n")    'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p")    'icomplete-backward-completions)

(defun my-icomplete-fido-slash ()
  "Insert slash, or exit minibuffer, or enter directory."
  (interactive)
  (let* ((dir (and (eq (icomplete--category) 'file)
                   (file-name-directory (icomplete--field-string))))
         (current (car completion-all-sorted-completions))
         (probe (and dir current
                     (expand-file-name (directory-file-name current) dir))))
    (cond ((and (eq (icomplete--category) 'file)
                (string= (file-name-nondirectory (icomplete--field-string)) "~"))
           (insert "/"))
          ((and probe (file-directory-p probe) (not (string= current "./")))
           (icomplete-force-complete))
          (t
           (icomplete-force-complete-and-exit)))))
(define-key icomplete-fido-mode-map (kbd "/") 'my-icomplete-fido-slash)

(defun my-icomplete--fido-mode-setup ()
  "Customize icomplete fido mode."
  (setq-local completion-styles '(orderless)))

(advice-add #'icomplete--fido-mode-setup :after #'my-icomplete--fido-mode-setup)

(defun my-icomplete-vertical-format-completions (reformatted)
  "Customize completions format."
  (save-match-data
    (if (string-match "[[]Matched[]]" reformatted)
        (progn
          (add-face-text-property (match-beginning 0) (match-end 0) 'success nil reformatted)
          (setq reformatted (concat " " reformatted)))
      (when (string-match "[(]No matches[)]" reformatted)
        (add-face-text-property (match-beginning 0) (match-end 0) 'error nil reformatted)
        (setq reformatted (concat " " reformatted)))))
  reformatted)

(advice-add #'icomplete-vertical-format-completions :filter-return #'my-icomplete-vertical-format-completions)

(defun my-icomplete-presorted-completion-table (completions)
  "Keep completion table order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity)
                   (cycle-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))

(defun my-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Like `completing-read', but don't sort COLLECTION"
  (completing-read prompt (my-icomplete-presorted-completion-table collection) predicate require-match initial-input hist def inherit-input-method))

(defun my-icomplete-find-file-from-bookmark ()
  "Find file starting from bookmark."
  (interactive)
  (let* ((bmk-list (bookmark-all-names))
         (name (completing-read "Use dir of bookmark: " (my-icomplete-presorted-completion-table bmk-list) nil t))
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

(defun my-icomplete-recentf-file ()
  "Find a file in the recently opened file list using completing-read."
  (interactive)
  (let* ((file-alist (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) recentf-list))
         (choice-list (delete-dups (mapcar 'car file-alist)))
         (filename (completing-read "Find recent file: " (my-icomplete-presorted-completion-table choice-list) nil t))
         (result-list (delq nil (mapcar (lambda (x) (when (string= (car x) filename) (cdr x))) file-alist)))
         (result-length (length result-list)))
    (if (and filename (not (string= filename "")))
        (find-file
         (cond
          ((= result-length 0) filename)
          ((= result-length 1) (car result-list))
          (t (completing-read "Multiple matches: " result-list nil t))))
      (call-interactively 'find-file))))

(icomplete-vertical-mode 1)
(fido-mode 1)

(provide 'my-icomplete)
