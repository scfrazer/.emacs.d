;;; my-vc.el

(require 'vc)
(require 'vc-dir)
(require 'vc-git)

(setq vc-annotate-background nil
      vc-annotate-background-mode nil
      vc-annotate-color-map '((20 . "red3")
                              (40 . "magenta4")
                              (60 . "maroon3")
                              (80 . "orangered3")
                              (100 . "chocolate3")
                              (120 . "gold4")
                              (140 . "darkolivegreen4")
                              (160 . "green4")
                              (180 . "aquamarine4")
                              (200 . "cyan4")
                              (220 . "lightcyan4")
                              (240 . "skyblue4")
                              (260 . "dodgerblue4")
                              (280 . "blue4")
                              (300 . "royalblue4")
                              (320 . "slateblue4")
                              (340 . "gray30")
                              (360 . "black"))
      vc-annotate-very-old-color nil)

(defun my-vc-git-command (verb fn)
  (let* ((fileset-arg (vc-deduce-fileset nil t))
         (backend (car fileset-arg))
         (files (nth 1 fileset-arg)))
    (if (eq backend 'Git)
        (progn (funcall fn files)
               (message (concat verb " " (number-to-string (length files))
                                " file(s).")))
      (message "Not in a vc git buffer."))))

(defun my-vc-git-add (&optional revision vc-fileset comment)
  (interactive "P")
  (my-vc-git-command "Staged" 'vc-git-register))

(defun my-vc-git-reset (&optional revision vc-fileset comment)
  (interactive "P")
  (my-vc-git-command "Unstaged"
                     (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))

(defun my-vc-activate ()
  (interactive)
  (unless vc-mode
    (let ((vc-handled-backends my-vc-handled-backends))
      (vc-find-file-hook))))

(defun my-vc-ediff (&optional rev)
  "Use ediff with vc."
  (interactive)
  (unless (buffer-file-name)
    (error "Current buffer is not visiting a file"))
  (when (and (buffer-modified-p)
             (y-or-n-p (message "Buffer %s is modified. Save buffer? " (buffer-name))))
    (save-buffer (current-buffer)))
  (my-vc-activate)
  (ediff-load-version-control)
  (ediff-vc-internal (or rev "") ""))

;; (define-key vc-prefix-map [(r)] 'vc-revert-buffer)
;; (define-key vc-dir-mode-map [(r)] 'vc-revert-buffer)
(define-key vc-prefix-map [(a)] 'my-vc-git-add)
(define-key vc-dir-mode-map [(a)] 'my-vc-git-add)
(define-key vc-prefix-map [(u)] 'my-vc-git-reset)
(define-key vc-dir-mode-map [(u)] 'my-vc-git-reset)

;; hide up to date files after refreshing in vc-dir
(define-key vc-dir-mode-map [(g)]
  (lambda () (interactive) (vc-dir-refresh) (vc-dir-hide-up-to-date)))

(provide 'my-vc)
