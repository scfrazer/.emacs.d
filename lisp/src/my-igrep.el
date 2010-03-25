;;; my-igrep.el

(require 'igrep)

(setq-default grep-program "/bin/egrep"
              igrep-find-file-clause "-type f"
              igrep-find-file-clause "-type f"
              igrep-find-prune-clause nil
              igrep-find-use-xargs t
              igrep-menu-bar nil
              igrep-options "-d skip -I -i"
              igrep-program "/bin/egrep"
              igrep-read-multiple-files nil
              igrep-read-multiple-files t
              igrep-save-buffers nil)

(defun my-igrep-regex-default ()
  (if (and transient-mark-mode mark-active)
      (buffer-substring (region-beginning) (region-end))
    (current-word)))

(setq igrep-regex-default 'my-igrep-regex-default)

(defun my-igrep-buffer-file-name-pattern ()
  "*")

(setq igrep-files-default 'my-igrep-buffer-file-name-pattern)

(defun my-igrep-current-file (program regex files &optional options)
  (interactive (my-igrep-read-args))
  (igrep program regex files options))

(defun my-igrep-read-args ()
  "Read and return a list: (PROGRAM REGEX current-file OPTIONS)."
  (let* ((pre-prefix (if (and igrep-find (eq igrep-verbose-prompts t))
                         "[find] "))
         (program
          (igrep-read-program pre-prefix))
         (prefix (if (and program (eq igrep-verbose-prompts t))
                     (igrep-prefix pre-prefix program " ")
                   pre-prefix))
         (options
          (igrep-read-options prefix))
         (post-prefix (if (and options (eq igrep-verbose-prompts t))
                          (igrep-prefix prefix options " ")
                        prefix)))
    (list program
          (igrep-read-regex post-prefix)
          (file-name-nondirectory (buffer-file-name))
          options)))

(defun my-grep-setup-hook ()
  (setenv "TERM" "xterm"))

(add-hook 'grep-setup-hook 'my-grep-setup-hook)

(provide 'my-igrep)
