;;; my-bs.el

(require 'bs)

;; Ignore buffers

(defvar my-bs-always-show-regexps (list (regexp-opt
                                         (list "*scratch*"
                                               "*info*"
                                               "*grep*"
                                               "*Find*"
                                               "*compilation*"
                                               "*shell*"
                                               "*terminal*"
                                               ))
                                        "*magit.+"
                                        "*cc-status.+")
  "*Buffer regexps to always show when buffer switching.")

(defvar my-bs-never-show-regexps '("^\\s-" "^\\*" "TAGS$")
  "*Buffer regexps to never show when buffer switching.")

(defvar my-ido-ignore-dired-buffers t
  "*If non-nil, buffer switching should ignore dired buffers.")

(defun my-bs-str-in-regexp-list (str regexp-list)
  "Return non-nil if str matches anything in regexp-list."
  (let ((case-fold-search nil))
    (catch 'done
      (dolist (regexp regexp-list)
        (when (string-match regexp str)
          (throw 'done t))))))

(defun my-bs-ignore-buffer (name)
  "Return non-nil if the named buffer should be ignored."
  (or (and (not (my-bs-str-in-regexp-list name my-bs-always-show-regexps))
           (my-bs-str-in-regexp-list name my-bs-never-show-regexps))
      (and my-ido-ignore-dired-buffers
           (with-current-buffer name
             (and (equal major-mode 'dired-mode)
                  (not (string= name "*Find*")))))))

;; Toggle buffers

(defun my-bs-toggle ()
  "Toggle buffers, ignoring certain ones."
  (interactive)
  (catch 'done
    (dolist (buf (buffer-list))
      (unless (or (equal (current-buffer) buf)
                  (my-bs-ignore-buffer (buffer-name buf)))
        (switch-to-buffer buf)
        (throw 'done t)))))

;; Configurations

(setq bs-configurations
      '(("all" nil nil nil nil nil)
        ("files" nil nil nil (lambda (buf) (my-bs-ignore-buffer (buffer-name buf))) nil)))
(setq bs-cycle-configuration-name "files")
(setq bs-max-window-height 100)

;; font-lock

(setq bs-mode-font-lock-keywords
  (list
   ; Headers
   (list "^[ ]+\\([-M].*\\)$" 1 font-lock-keyword-face)
   ; Boring buffers
   (list "^\\(.*\\*.*\\*.*\\)$" 1 font-lock-comment-face)
   ; Dired buffers
   '("^[ .*%]+\\(Dired.*\\)$" 1 font-lock-type-face)
   ; Modified buffers
   '("^[ .]+\\(\\*\\)" 1 font-lock-warning-face)
   ; Read-only buffers
   '("^[ .*]+\\(\\%\\)" 1 font-lock-variable-name-face)))

;; Header

(setq bs-attributes-list
      (quote (("" 2 2 left bs--get-marked-string)
              ("M" 1 1 left bs--get-modified-string)
              ("R" 2 2 left bs--get-readonly-string)
              ("" 2 2 left "  ")
              ("Mode" 16 16 left bs--get-mode-name)
              ("" 2 2 left "  ")
              ("Buffer" bs--get-name-length 30 left bs--get-name))))

;; Other customization

(defun my-bs-split-window-vertically (&optional arg)
  "Split window vertically, switch to other window and previous buffer."
  (interactive "P")
  (split-window-vertically)
  (recenter)
  (unless arg
    (other-window 1)
    (my-bs-toggle)
    (recenter 4)
    (other-window -1)))

(defun my-bs-split-window-horizontally (&optional arg)
  "Split window horizontally, switch to other window and previous buffer."
  (interactive "P")
  (split-window-horizontally)
  (recenter)
  (unless arg
    (other-window 1)
    (my-bs-toggle)
    (recenter 4)
    (other-window -1)))

(defadvice bs-show (after my-bs-advice activate)
  "Turn on hl-line-mode."
  (hl-line-mode 1))

(defadvice bs--redisplay (after my-bs-advice activate)
  "Turn on hl-line-mode."
  (hl-line-mode 1))

;; Done

(provide 'my-bs)
