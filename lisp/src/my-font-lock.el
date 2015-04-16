;; my-font-lock.el

(require 'font-lock)

(setq-default show-trailing-whitespace t
              lazy-lock-mode nil)
(global-font-lock-mode t)

;; Extra font-lock faces

(defface my-tab-face
  '((t (:background "#EEEEEE")))
  "Visible tab chars."
  :group 'faces)

(defface my-debug-face
  '((t (:foreground "#000000" :background "#FF8700")))
  "todo/fixme highlighting."
  :group 'faces)

(defface my-todo-face
  '((t (:foreground "#000000" :background "#FFFF00")))
  "todo/fixme highlighting."
  :group 'faces)

(defface my-fixme-face
  '((t (:foreground "#FFFFFF" :background "#CD0000")))
  "todo/fixme highlighting."
  :group 'faces)

;; Show whitespace functionality

(defun my-font-lock-show-whitespace (&optional arg)
  "Toggle display of whitespace.
Turn on iff arg is > 0, off iff arg is <= 0, otherwise toggle."
  (interactive "P")
  (let ((prev-val show-trailing-whitespace))
    (if (and arg (numberp arg))
        (if (> arg 0)
            (setq show-trailing-whitespace t)
          (setq show-trailing-whitespace nil))
      (setq show-trailing-whitespace (not show-trailing-whitespace)))
    (when (or (and arg (numberp arg))
              (not (equal show-trailing-whitespace prev-val)))
      (if show-trailing-whitespace
          (font-lock-add-keywords nil '(("\t+" (0 'my-tab-face prepend))) 'add-to-end)
        (font-lock-remove-keywords nil '(("\t+" (0 'my-tab-face prepend)))))
      (font-lock-fontify-buffer))))

;; Hooks for adding font-lock faces

(defun my-font-lock-mode-hook ()
  "Font-lock mode hook."
  (if (or buffer-read-only
          (string-match "\\s-*\\*.+\\*" (buffer-name))
          (and (stringp (buffer-file-name))
               (string-match "\.el\.gz$" (buffer-file-name))))
      (my-font-lock-show-whitespace -1)
    (my-font-lock-show-whitespace 1))
  (when (or (and comment-start font-lock-keywords
                 (not (eq major-mode 'org-mode)))
            (eq major-mode 'dired-mode))
    (font-lock-add-keywords nil (list (cons "\\<\\(DEBUG\\)\\>" (list '(1 'my-debug-face t)))) 'add-to-end)
    (font-lock-add-keywords nil (list (cons "\\<\\([Tt][Oo][Dd][Oo]\\)\\>" (list '(1 'my-todo-face t)))) 'add-to-end)
    (font-lock-add-keywords nil (list (cons "\\<\\([Ff][Ii][Xx][Mm][Ee]\\)\\>" (list '(1 'my-fixme-face t)))) 'add-to-end)))

(defvar my-font-lock-whitespace-state t)
(make-variable-buffer-local 'my-font-lock-whitespace-state)

(defun my-font-lock-whitespace-hook ()
  "Turn whitespace on/off with read-only status"
  (if buffer-read-only
      (my-font-lock-show-whitespace -1)
    (when my-font-lock-whitespace-state
      (my-font-lock-show-whitespace 1))))

(defun my-font-lock-before-revert-hook ()
  (setq my-font-lock-whitespace-state show-trailing-whitespace))

(add-hook 'font-lock-mode-hook 'my-font-lock-mode-hook)
(add-hook 'read-only-mode-hook 'my-font-lock-whitespace-hook)
(add-hook 'before-revert-hook 'my-font-lock-before-revert-hook)
(add-hook 'after-revert-hook 'my-font-lock-whitespace-hook)

(provide 'my-font-lock)
