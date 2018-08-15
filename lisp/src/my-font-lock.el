;; my-font-lock.el

(require 'font-lock)
(require 'whitespace)

(setq-default lazy-lock-mode nil
              whitespace-style '(face tabs trailing empty))
(global-font-lock-mode t)

;; Extra font-lock faces

(defface my-debug-face
  '((t (:foreground "black" :background "orange2")))
  "todo/fixme highlighting."
  :group 'faces)

(defface my-todo-face
  '((t :inherit warning))
  "todo/fixme highlighting."
  :group 'faces)

(defface my-fixme-face
  '((t :inherit error))
  "todo/fixme highlighting."
  :group 'faces)

(defun my-font-lock-whitespace-mode-hook ()
  "Turn extra whitespace indication on/off."
  (if whitespace-mode
      (progn (unless buffer-display-table
               (setq buffer-display-table
                     (if standard-display-table
                         (copy-sequence standard-display-table)
                       (make-display-table))))
             (aset buffer-display-table ?\t [?» ?\t]))
    (when buffer-display-table
      (aset buffer-display-table ?\t [?\t]))))

(add-hook 'whitespace-mode-hook 'my-font-lock-whitespace-mode-hook)

(defun my-font-lock-disable-whitespace ()
  "Disable whitespace for this buffer."
  (or buffer-read-only
      (string-match "\\s-*\\*.+\\*" (buffer-name))
      (member major-mode '(Info-mode))
      (and (stringp (buffer-file-name))
           (string-match "\\(\.log\\|config_tree.txt\\|\.el\.gz\\)\\'" (buffer-file-name)))))

(defun my-font-lock-whitespace-hook ()
  "Turn whitespace on/off."
  (when my-font-lock-auto-whitespace
    (whitespace-mode (if (my-font-lock-disable-whitespace) -1 1))))

(add-hook 'read-only-mode-hook 'my-font-lock-whitespace-hook)
(add-hook 'after-revert-hook 'my-font-lock-whitespace-hook)

(defvar my-font-lock-auto-whitespace nil
  "Automatically turn on whitespace-mode with font-lock.")

(defun my-font-lock-mode-hook ()
  "Font-lock mode hook."
  (my-font-lock-whitespace-hook)
  (when (or (and comment-start font-lock-keywords
                 (not (eq major-mode 'org-mode)))
            (eq major-mode 'dired-mode))
    (font-lock-add-keywords nil (list (cons "\\<\\(DEBUG\\)\\>" (list '(1 'my-debug-face t)))) 'add-to-end)
    (font-lock-add-keywords nil (list (cons "\\_<\\([Tt][Oo][Dd][Oo]\\)\\_>" (list '(1 'my-todo-face t)))) 'add-to-end)
    (font-lock-add-keywords nil (list (cons "\\_<\\(XXX\\)\\_>" (list '(1 'my-todo-face t)))) 'add-to-end)
    (font-lock-add-keywords nil (list (cons "\\_<\\([Ff][Ii][Xx]\\([Mm][Ee]\\)?\\)\\_>" (list '(1 'my-fixme-face t)))) 'add-to-end)))

(add-hook 'font-lock-mode-hook 'my-font-lock-mode-hook)

(provide 'my-font-lock)
