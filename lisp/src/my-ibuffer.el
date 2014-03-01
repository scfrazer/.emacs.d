;;; my-ibuffer.el

(require 'ibuffer)
(require 'my-bookmark)

(setq-default ibuffer-default-sorting-mode 'filename-and-dired
              ibuffer-display-summary nil
              ibuffer-filter-group-name-face 'font-lock-keyword-face
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
                                                 "*shell"
                                                 "*terminal"
                                                 "*scratch")))

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

(define-ibuffer-column bmk-filename
  (:name "Filename" :inline nil)
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
              (name 18 18 :left :elide) "  "
              (mode 16 16 :left :elide) "  "
              bmk-filename)
        (mark modified read-only "  "
              (name 18 18 :left :elide) "  "
              (mode 16 16 :left :elide) "  "
              filename-and-process)))

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
         ("Files" (or (mode . dired-mode)
                      (filename . "/[^v][^o][^b]")))
         ("*" (or (mode . Custom-mode)
                  (name . ,my-ibuffer-star-regexp)))
         )))

(define-ibuffer-sorter filename-and-dired
  "Sort the buffers by their pathname."
  (:description "Filenames plus dired")
  (string-lessp
   (with-current-buffer (car a)
     (or buffer-file-name
         (if (eq major-mode 'dired-mode)
             (expand-file-name dired-directory))
         ;; so that all non pathnames are at the end
         "~"))
   (with-current-buffer (car b)
     (or buffer-file-name
         (if (eq major-mode 'dired-mode)
             (expand-file-name dired-directory))
         ;; so that all non pathnames are at the end
         "~"))))

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
  (define-key ibuffer-mode-map (kbd "s f") 'ibuffer-do-sort-by-filename-and-dired)
  (define-key ibuffer-mode-map (kbd "RET") 'ibuffer-visit-buffer-1-window)
  (hl-line-mode 1))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)

(provide 'my-ibuffer)
