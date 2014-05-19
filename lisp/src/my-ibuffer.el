;;; my-ibuffer.el

(require 'ibuffer)
(require 'bookmark)
(require 'my-ediff)

(setq-default ibuffer-default-sorting-mode 'recency
              ibuffer-display-summary nil
              ibuffer-filter-group-name-face 'my-ibuffer-group-name-face
              ibuffer-movement-cycle nil
              ibuffer-expert t
              ibuffer-show-empty-filter-groups nil
              ibuffer-use-other-window t)

(defface my-ibuffer-group-name-face
  '((t (:foreground "black" :background "gray85")))
  "ibuffer group name face"
  :group 'faces)

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

(defvar my-ibuffer-env-vars-as-bookmarks (list "RESULTSDIR"))
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
      (when (string-match ".+/$" filename)
        (unless (string-match "[^a-zA-Z0-9_.~/]" name)
          (push (cons (concat "^" (expand-file-name filename) "\\(.*\\)")
                      (concat "$" name))
                my-ibuffer-bookmark-subs)))))
  (dolist (var my-ibuffer-env-vars-as-bookmarks)
    (let ((dir (getenv var)))
      (when dir
        (push (cons (concat "^" (expand-file-name dir) "/\\(.*\\)")
                    (concat "$" var))
              my-ibuffer-bookmark-subs))))
  (setq my-ibuffer-bookmark-subs
        (sort my-ibuffer-bookmark-subs (lambda (x y) (string< (car y) (car x))))))

(my-ibuffer-build-bookmark-subs)

(defvar my-ibuffer-current-buf)
(define-ibuffer-column current
  (:name "C" :inline nil)
  (if (eq my-ibuffer-current-buf buffer) "." " "))

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
      '((mark current read-only modified "  "
              (buffer -1 -1 :left :elide) "  "
              bmk-filename)
        (mark current read-only modified "  "
              (buffer -1 -1 :left :elide) "  "
              filename)))

(setq ibuffer-fontification-alist
      `((6 (eq major-mode 'dired-mode) font-lock-type-face)
        (5 (or (string-match (concat my-ibuffer-vc-regexp "\\|" my-ibuffer-star-regexp) (buffer-name))
               (eq major-mode 'Custom-mode)) font-lock-type-face)
        (4 (string-match "^*sqlplus:" (buffer-name)) font-lock-string-face)
        (3 (string-match "^*" (buffer-name)) font-lock-comment-face)
        (2 (and (null (buffer-file-name)) (string-match "^[^*]" (buffer-name))) font-lock-string-face)
        (1 buffer-read-only font-lock-doc-face)))

(setq ibuffer-saved-filter-groups
      `(("my-groups"
         ("VC" (name . ,my-ibuffer-vc-regexp))
         ("SQL" (or (mode . sqlplus-mode)
                    (name . "^*sqlplus:")))
         ("Dired" (mode . dired-mode))
         ("Org" (mode . org-mode))
         ("ELisp" (mode . emacs-lisp-mode))
         ("VOB" (filename . "/vob"))
         ("Files" (predicate . (my-ibuffer-filter-files)))
         ("Temp" (predicate . (my-ibuffer-filter-buffers)))
         ("*" (or (mode . Custom-mode)
                  (name . ,my-ibuffer-star-regexp)))
         )))

(defun my-ibuffer-filter-files ()
  "Filter to match non-TAGS files."
  (and (buffer-file-name)
       (not (string-match "^\\(.+/\\)?TAGS$" (buffer-file-name)))))

(defun my-ibuffer-filter-buffers ()
  "Filter to match temp buffers."
  (and (not (buffer-file-name))
       (string-match "^[^*]" (buffer-name))))

(defvar my-ibuffer-header-line-format nil)
(defun ibuffer-update-title-and-summary (format)
  (ibuffer-assert-ibuffer-mode)
  (setq my-ibuffer-header-line-format (propertize " " 'display '((space :align-to 0))))
  (let ((after-change-functions nil))
    (dolist (element format)
      (setq my-ibuffer-header-line-format
            (concat my-ibuffer-header-line-format
                    (if (stringp element)
                        element
                      (pcase-let ((`(,sym ,min ,_max ,align) element))
                        (when (cl-minusp min)
                          (setq min (- min)))
                        (let* ((name (or (get sym 'ibuffer-column-name)
                                         (error "Unknown column %s in ibuffer-formats" sym)))
                               (len (length name))
                               (hmap (get sym 'header-mouse-map))
                               (strname (if (< len min)
                                            (ibuffer-format-column name
                                                                   (- min len)
                                                                   align)
                                          name)))
                          strname))))))))
(defadvice ibuffer-update (after my-ibuffer-update activate)
  (setq header-line-format my-ibuffer-header-line-format))
(defadvice ibuffer-switch-format (after my-ibuffer-switch-format activate)
  (setq header-line-format my-ibuffer-header-line-format))

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

(defun my-ibuffer-toggle-filter-group ()
  "Toggle the current filter group."
  (interactive)
  (beginning-of-line)
  (unless (looking-at "\\[ ")
    (ibuffer-backward-filter-group))
  (ibuffer-toggle-filter-group))

(defvar my-ibuffer-hidden-filter-groups nil)
(defun my-ibuffer-toggle-hidden-filter-groups ()
  "Toggle showing-all/rehiding filter groups."
  (interactive)
  (if ibuffer-hidden-filter-groups
      (setq my-ibuffer-hidden-filter-groups ibuffer-hidden-filter-groups
            ibuffer-hidden-filter-groups nil)
    (setq ibuffer-hidden-filter-groups my-ibuffer-hidden-filter-groups
          my-ibuffer-hidden-filter-groups nil))
  (ibuffer-update nil t))

(defun my-ibuffer-do-delete ()
  "`ibuffer-do-delete' but stay on the same line."
  (interactive)
  (ibuffer-do-delete)
  (forward-line))

(defun my-ibuffer-diff ()
  "Smart diff against current buffer."
  (interactive)
  (ibuffer-visit-buffer-1-window)
  (my-ediff-dwim))

(defun my-ibuffer ()
  "Open ibuffer with point on last buffer name."
  (interactive)
  (setq my-ibuffer-current-buf (current-buffer))
  (ibuffer)
  (ibuffer-jump-to-buffer (buffer-name my-ibuffer-current-buf)))

(defun my-ibuffer-mode-hook ()
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "my-groups")
  (setq ibuffer-hidden-filter-groups '("Default"))
  (define-key ibuffer-mode-map (kbd "=") 'my-ibuffer-diff)
  (define-key ibuffer-mode-map (kbd "C-x C-f") nil)
  (define-key ibuffer-mode-map (kbd "D") 'my-ibuffer-do-delete)
  (define-key ibuffer-mode-map (kbd "M->") (lambda () (interactive) (goto-char (point-max)) (forward-line -1)))
  (define-key ibuffer-mode-map (kbd "N") 'ibuffer-forward-filter-group)
  (define-key ibuffer-mode-map (kbd "P") 'ibuffer-backward-filter-group)
  (define-key ibuffer-mode-map (kbd "RET") 'ibuffer-visit-buffer-1-window)
  (define-key ibuffer-mode-map (kbd "i") (lambda() (interactive) (ido-switch-buffer) (delete-other-windows)))
  (define-key ibuffer-mode-map (kbd "TAB") 'my-ibuffer-toggle-filter-group)
  (define-key ibuffer-mode-map (kbd "a") 'my-ibuffer-toggle-hidden-filter-groups)
  (define-key ibuffer-mode-map (kbd "s r") 'ibuffer-do-sort-by-recency)
  (hl-line-mode 1))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)

(provide 'my-ibuffer)
