;;; my-ibuffer.el

(require 'ibuffer)
(require 'bookmark)

(setq-default ibuffer-default-sorting-mode 'filename/process
              ibuffer-display-summary nil
              ibuffer-filter-group-name-face 'font-lock-keyword-face
              ibuffer-movement-cycle nil
              ibuffer-never-show-predicates '("TAGS$")
              ibuffer-expert t
              ibuffer-show-empty-filter-groups nil
              ibuffer-use-other-window t)

(defvar my-ibuffer-vc-regexp (regexp-opt (list "*cc-status"
                                               "*clearcase-config-spec"
                                               "*magit:")))

(defvar my-ibuffer-star-regexp (regexp-opt (list "*Find"
                                                 "*Occur"
                                                 "*calculator"
                                                 "*compilation"
                                                 "*grep"
                                                 "*info"
                                                 "*scratch"
                                                 "*shell"
                                                 "*terminal")))

(defvar my-ibuffer-bookmark-subs nil)

(defun my-ibuffer-build-bookmark-subs ()
  "Build bookmark substitutions."
  (interactive)
  (setq my-ibuffer-bookmark-subs nil)
  (bookmark-load bookmark-default-file t t)
  (let (name filename)
    (dolist (bmk bookmark-alist)
      (setq name (bookmark-name-from-full-record bmk)
            filename (bookmark-get-filename bmk))
      (when (file-directory-p filename)
        (unless (string-match "[^a-zA-Z0-9_.~/]" name)
          (push (cons (concat "^" (expand-file-name filename) "\\(.*\\)")
                      (concat "$" name))
                my-ibuffer-bookmark-subs)))))
  (setq my-ibuffer-bookmark-subs
        (sort my-ibuffer-bookmark-subs (lambda (x y) (string< (car y) (car x))))))

(my-ibuffer-build-bookmark-subs)

(define-ibuffer-column buffer
  (:name "Name" :inline nil)
  (propertize (buffer-name) 'font-lock-face (ibuffer-buffer-name-face buffer mark)))

(define-ibuffer-column bmk-filename
  (:name "Bookmark/Filename" :inline nil)
  (let ((path (or (buffer-file-name buffer)
                  (and dired-directory
                       (expand-file-name dired-directory)))))
    (if (null path)
        ""
      (catch 'done
        (dolist (sub my-ibuffer-bookmark-subs)
          (when (string-match (car sub) path)
            (throw 'done (concat (propertize (cdr sub) 'font-lock-face 'font-lock-variable-name-face)
                                 "/" (match-string 1 path)))))
        path))))

(setq ibuffer-formats
      '((mark modified read-only "  "
              (buffer -1 -1 :left :elide) "  "
              bmk-filename)
        (mark modified read-only "  "
              (buffer -1 -1 :left :elide) "  "
              filename)))

(setq ibuffer-fontification-alist
      `((4 (eq major-mode 'dired-mode) font-lock-type-face)
        (3 (or (string-match (concat my-ibuffer-vc-regexp "\\|" my-ibuffer-star-regexp) (buffer-name))
               (eq major-mode 'Custom-mode)) font-lock-type-face)
        (2 (string-match "^*" (buffer-name)) font-lock-comment-face)
        (1 buffer-read-only font-lock-doc-face)))

(setq ibuffer-saved-filter-groups
      `(("my-groups"
         ("VC" (name . ,my-ibuffer-vc-regexp))
         ("Org" (mode . org-mode))
         ("ELisp" (mode . emacs-lisp-mode))
         ("VOB" (filename . "/vob"))
         ("Files" (filename . "."))
         ("Dired" (mode . dired-mode))
         ("*" (or (mode . Custom-mode)
                  (name . ,my-ibuffer-star-regexp)))
         )))

(defvar my-ibuffer-name-column-width 0)
(defadvice ibuffer-redisplay-engine (around my-ibuffer-redisplay-engine activate)
  "Dynamically change the width of the 'buffer' column"
  (let ((bufs (ad-get-arg 0))
        (max-width 0)
        formats new-format)
    (dolist (buf bufs)
      (setq max-width (max max-width (length (buffer-name (car buf))))))
    (unless (= my-ibuffer-name-column-width max-width)
      (setq my-ibuffer-name-column-width max-width)
      (dolist (old-format ibuffer-formats)
        (setq new-format nil)
        (dolist (item old-format)
          (when (and (listp item) (eq (car item) 'buffer))
            (setcar (nthcdr 1 item) max-width)
            (setcar (nthcdr 2 item) max-width))
          (setq new-format (append new-format (list item))))
        (push new-format formats))
      (setq ibuffer-formats (nreverse formats))
      (ibuffer-recompile-formats)
      (setq ibuffer-cached-formats ibuffer-formats
            ibuffer-cached-eliding-string ibuffer-eliding-string
            ibuffer-cached-elide-long-columns (with-no-warnings ibuffer-elide-long-columns))
      (when (featurep 'ibuf-ext)
        (setq ibuffer-cached-filter-formats ibuffer-filter-format-alist))))
  ad-do-it)

(defun my-ibuffer ()
  "Open ibuffer with point on last buffer name."
  (interactive)
  (let ((buf (buffer-name)))
    (ibuffer)
    (ibuffer-jump-to-buffer buf)))

(defun my-ibuffer-mode-hook ()
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "my-groups")
  (setq ibuffer-hidden-filter-groups '("Default"))
  (define-key ibuffer-mode-map (kbd "RET") 'ibuffer-visit-buffer-1-window)
  (define-key ibuffer-mode-map (kbd "s r") 'ibuffer-do-sort-by-recency)
  (define-key ibuffer-mode-map (kbd "N") 'ibuffer-forward-filter-group)
  (define-key ibuffer-mode-map (kbd "P") 'ibuffer-backward-filter-group)
  (hl-line-mode 1))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)

(provide 'my-ibuffer)
