;; my-font-lock.el

(require 'font-lock)

(setq-default lazy-lock-mode nil)
(global-font-lock-mode t)

;; Extra font-lock faces

(defface my-tab-face
  '((t (:background "seashell")))
  "Visible tab chars."
  :group 'faces)

(defface my-trailing-space-face
  '((t (:background "lemonchiffon")))
  "Visible trailing space."
  :group 'faces)

(defface my-todo-face
  '((t (:bold on :weight bold :box t :foreground "firebrick2" :background "yellow")))
  "todo/fixme highlighting."
  :group 'faces)

;; Font size faces and functions

(defface my-large-face
  '((t (:height 1.1 :family "sfrazer")))
  "Large face."
  :group 'faces)

(defface my-medium-face
  '((t (:family "sfrazer")))
  "Medium face."
  :group 'faces)

(defface my-small-face
  '((t (:family "proggycleansz")))
  "Small face."
  :group 'faces)

(defface my-tiny-face
  '((t (:height 0.78 :family "gohufont")))
  "Tiny face."
  :group 'faces)

(defun my-font-large ()
  "Switch to large font."
  (interactive)
  (set-frame-font "-sfrazer-*-medium-*-*-*-15-*-*-*-*-*-*-*"))

(defun my-font-medium ()
  "Switch to medium font."
  (interactive)
  (set-frame-font "-sfrazer-*-medium-*-*-*-14-*-*-*-*-*-*-*"))

(defun my-font-small ()
  "Switch to small font."
  (interactive)
  (set-frame-font "-*-proggycleansz-*-*-*-*-*-*-*-*-*-*-*-*"))

(defun my-font-tiny ()
  "Switch to tiny font."
  (interactive)
  (set-frame-font "-*-gohufont-medium-*-*-*-11-*"))

;; Show whitespace functionality

(defvar my-font-lock-show-whitespace t
  "*Show whitespace")
(make-variable-buffer-local 'my-font-lock-show-whitespace)

(defun my-font-lock-show-whitespace (&optional arg)
  "Toggle display of whitespace.
Turn on iff arg is > 0, off iff arg is <= 0, otherwise toggle."
  (interactive "P")
  (unless (string-match "\\s-*\\*.+\\*" (buffer-name))
    (let ((prev-val my-font-lock-show-whitespace))
      (if arg
          (if (> arg 0)
              (setq my-font-lock-show-whitespace t)
            (setq my-font-lock-show-whitespace nil))
        (setq my-font-lock-show-whitespace (not my-font-lock-show-whitespace)))
      (unless (equal prev-val my-font-lock-show-whitespace)
        (if my-font-lock-show-whitespace
            (my-font-lock-add-whitespace)
          (my-font-lock-remove-whitespace))
        (font-lock-fontify-buffer)))))

(defun my-font-lock-add-whitespace ()
  (unless (string-match "\\s-*\\*.+\\*" (buffer-name))
    (font-lock-add-keywords nil
                            '(("\t+" (0 'my-tab-face t))
                              ("[ \t]+$" (0 'my-trailing-space-face t)))
                            'add-to-end)))

(defun my-font-lock-remove-whitespace ()
  (unless (string-match "\\s-*\\*.+\\*" (buffer-name))
    (font-lock-remove-keywords nil
                               '(("\t+" (0 'my-tab-face t))
                                 ("[ \t]+$" (0 'my-trailing-space-face t))))))

;; Hooks and advice for adding font-lock faces

(defun my-font-lock-mode-hook ()
  (when comment-start
    (font-lock-add-keywords nil
                            (list (cons "\\<\\([Tt][Oo][Dd][Oo]\\|[Ff][Ii][Xx][Mm][Ee]\\)\\>"
                                        (list '(1 'my-todo-face t))))
                            'add-to-end)
    (when my-font-lock-show-whitespace
      (my-font-lock-add-whitespace))))

(defun my-font-lock-find-file-hook ()
  (when buffer-read-only
    (my-font-lock-show-whitespace -1)))

(defadvice toggle-read-only (after my-font-lock-toggle-read-only activate)
  "Turn whitespace on/off with read-only status"
  (if buffer-read-only
      (my-font-lock-show-whitespace 0)
    (my-font-lock-show-whitespace 1)))

(defadvice revert-buffer (after my-font-lock-revert-buffer activate)
  "Turn whitespace on/off with read-only status"
  (if buffer-read-only
      (my-font-lock-show-whitespace 0)
    (my-font-lock-show-whitespace 1)))

(add-hook 'font-lock-mode-hook 'my-font-lock-mode-hook)
(add-hook 'find-file-hook 'my-font-lock-find-file-hook)

(provide 'my-font-lock)
