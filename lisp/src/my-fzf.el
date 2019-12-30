;;; my-fzf.el

(require 'fzf)
(require 'my-dired)

(defun my-fzf-start (type &optional all)
  (let ((dir (if all
                 default-directory
               (or (car (project-roots (project-current))) default-directory))))
    (let ((process-environment
           (cons (concat "FZF_DEFAULT_COMMAND=fd --type " type " --hidden --exclude .git" (when all " --no-ignore")) process-environment)))
      (fzf/start dir))))

(defun my-fzf-project-file ()
  "Find a project file."
  (interactive)
  (my-fzf-start "f"))

(defun my-fzf-any-local-file ()
  "Find any local file"
  (interactive)
  (my-fzf-start "f" t))

(defun my-fzf-project-directory ()
  "Find a project directory."
  (interactive)
  (my-fzf-start "d"))

(defun my-fzf-any-local-directory ()
  "Find a any local directory."
  (interactive)
  (my-fzf-start "d" t))

(defun fzf/after-term-handle-exit (process-name msg)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (lines (split-string text "\n" t "\s*>\s+"))
         (line (car (last (butlast lines 1))))
         (selected (split-string line ":"))
         (file (expand-file-name (pop selected)))
         (linenumber (pop selected)))
    (kill-buffer "*fzf*")
    (jump-to-register :fzf-windows)
    (when (file-exists-p file)
      (if (file-directory-p file)
          (progn
            (setq my-dired-prev-dir (dired-current-directory))
            (find-alternate-file file))
        (find-file file)))
    (when linenumber
      (goto-char (point-min))
      (forward-line (- (string-to-number linenumber) 1))
      (back-to-indentation)))
  (advice-remove 'term-handle-exit #'fzf/after-term-handle-exit))

(provide 'my-fzf)
