;;; my-project.el  -*- lexical-binding: t; -*-

(require 'project)

(setq-default project-vc-ignores (list "*.elc"))

(defun my-project-find (dir)
  (when-let ((ws (getenv "WORKSPACE")))
    (cons 'transient ws)))
(add-to-list 'project-find-functions #'my-project-find)

(defun my-project--files-in-directory (dir ignores &optional files)
  (let* ((default-directory dir)
         ;; Make sure ~/ etc. in local directory name is
         ;; expanded and not left for the shell command
         ;; to interpret.
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(advice-add #'project--files-in-directory :override #'my-project--files-in-directory)

(defun my-project-find-file-below ()
  (interactive)
  (let ((command "fd -t f -0 ."))
    (find-file
     (completing-read
      (concat "Find file in " default-directory ": ")
      (split-string (shell-command-to-string command) "\0" t) nil t))))

(provide 'my-project)
