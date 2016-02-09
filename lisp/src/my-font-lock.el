;; my-font-lock.el

(require 'font-lock)
(require 'whitespace)

(setq-default lazy-lock-mode nil
              whitespace-style '(face tabs trailing empty tab-mark))
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

;; Hooks for adding font-lock faces

(defun my-font-lock-mode-hook ()
  "Font-lock mode hook."
  (if (or buffer-read-only
          (string-match "\\s-*\\*.+\\*" (buffer-name))
          (and (stringp (buffer-file-name))
               (string-match "\.el\.gz$" (buffer-file-name))))
      (whitespace-mode -1)
    (whitespace-mode 1))
  (when (or (and comment-start font-lock-keywords
                 (not (eq major-mode 'org-mode)))
            (eq major-mode 'dired-mode))
    (font-lock-add-keywords nil (list (cons "\\<\\(DEBUG\\)\\>" (list '(1 'my-debug-face t)))) 'add-to-end)
    (font-lock-add-keywords nil (list (cons "\\_<\\([Tt][Oo][Dd][Oo]\\)\\_>" (list '(1 'my-todo-face t)))) 'add-to-end)
    (font-lock-add-keywords nil (list (cons "\\_<\\([Ff][Ii][Xx]\\([Mm][Ee]\\)?\\)\\_>" (list '(1 'my-fixme-face t)))) 'add-to-end)))

(defun my-font-lock-whitespace-hook ()
  "Turn whitespace on/off with read-only status"
  (whitespace-mode (if buffer-read-only -1 1)))

(add-hook 'font-lock-mode-hook 'my-font-lock-mode-hook)
(add-hook 'read-only-mode-hook 'my-font-lock-whitespace-hook)
(add-hook 'after-revert-hook 'my-font-lock-whitespace-hook)

(provide 'my-font-lock)
