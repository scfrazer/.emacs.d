(setq default-frame-alist
      '((width . 120) (height . 50)))

;; Exec this line to insert the current font as a setting:
;; (insert "\n(set-default-font \"" (cdr (assoc 'font (frame-parameters))) "\")")
(set-default-font "-apple-Source_Code_Pro-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")

(setq exec-path (append exec-path (list "/usr/local/bin"
                                        "/usr/local/git/bin"
                                        "/usr/X11/bin"
                                        "/opt/local/bin")))

(eval-after-load "grep"
  '(progn
     (grep-apply-setting 'grep-template "grep -nH -d skip -I -E -e <R> <C> <F>")
     (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -print0 | xargs -0 grep -nH -I -E -e <R> <C>")))

(defcustom my-tern-enable nil
  "Non-nil means use tern-mode where appropriate."
  :type 'boolean)

(defun my-tern-enable ()
  "Enable tern-mode where appropriate."
  (interactive)
  (setq my-tern-enable t))

(defun my-tern-mode ()
  "Turn on tern-mode when appropriate."
  (when my-tern-enable
    (tern-mode t)))

(eval-after-load "js2-mode"
  '(progn
     (add-hook 'js2-mode-hook 'my-tern-mode)))

(eval-after-load "web-mode"
  '(progn
     (add-hook 'web-mode-hook 'my-tern-mode)))

(my-theme-light)
