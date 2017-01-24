;;; my-buf.el

;; Ignore buffers

(defvar my-buf-always-show-regexps (list (concat "^"
                                                 (regexp-opt (list "*Find"
                                                                   "*Man"
                                                                   "*Occur"
                                                                   "*ag"
                                                                   "*cc-status"
                                                                   "*clearcase-config-spec"
                                                                   "*compilation"
                                                                   "*grep"
                                                                   "*info"
                                                                   "*magit:"
                                                                   "*scratch"
                                                                   "*shell"
                                                                   "*git-simple"
                                                                   "*terminal"
                                                                   "*regman"
                                                                   "*vcs-compile"))))
  "*Buffer regexps to always show when buffer switching.")

(defvar my-buf-never-show-regexps '("^\\s-" "^\\*" "TAGS$")
  "*Buffer regexps to never show when buffer switching.")

(defvar my-buf-ignore-dired-buffers t
  "*If non-nil, buffer switching should ignore dired buffers.")

(defun my-buf-str-in-regexp-list (str regexp-list)
  "Return non-nil if str matches anything in regexp-list."
  (let ((case-fold-search nil))
    (catch 'done
      (dolist (regexp regexp-list)
        (when (string-match regexp str)
          (throw 'done t))))))

(defun my-buf-ignore-buffer (name)
  "Return non-nil if the named buffer should be ignored."
  (or (and (not (my-buf-str-in-regexp-list name my-buf-always-show-regexps))
           (my-buf-str-in-regexp-list name my-buf-never-show-regexps))
      (and my-buf-ignore-dired-buffers
           (condition-case nil
               (with-current-buffer name
                 (and (equal major-mode 'dired-mode)
                      (not (string= name "*Find*"))))
             (error nil)))))

;; Toggle buffers

(defun my-buf-toggle ()
  "Toggle buffers, ignoring certain ones."
  (interactive)
  (catch 'done
    (dolist (buf (buffer-list))
      (unless (or (equal (current-buffer) buf)
                  (my-buf-ignore-buffer (buffer-name buf)))
        (switch-to-buffer buf)
        (throw 'done t)))))

;; Done

(provide 'my-buf)
