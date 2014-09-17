;;; ag2.el

(require 'compile)
(require 'grep)

(defgroup ag2 nil
  "View results from 'ag'."
  :group 'tools)

(defvar ag2-search-history nil)

(defun ag2-word-at-point ()
  "Get the closest word at point."
  (save-excursion
    (unless (member (char-syntax (char-after (point))) (list ?w ?_))
      (skip-syntax-backward "^w_"))
    (skip-syntax-backward "w_")
    (let ((beg (point)))
      (skip-syntax-forward "w_")
      (buffer-substring-no-properties beg (point)))))

;;;###autoload
(defun ag2 (&optional arg)
  "Run 'ag'.  With prefix arg, take search string from region."
  (interactive "P")
  (let* ((default-string
           (if arg
               (buffer-substring-no-properties (region-beginning) (region-end))
             (ag2-word-at-point)))
         (search-string
          (read-from-minibuffer
           (concat "Search for (default \"" default-string "\"): ")
           nil nil nil 'ag2-search-history default-string))
         (search-dir
          (read-directory-name "In directory: " nil default-directory t)))
    (when (string= search-string "")
      (setq search-string default-string))
    (let ((default-directory search-dir))
      (compilation-start (concat "ag --nogroup --line-number --column --color --color-match 1\\;31 --smart-case -- " search-string) 'ag2-mode))))

;;;###autoload
(define-compilation-mode ag2-mode "ag2"
  "Mode for viewing results from 'ag'."
  :group 'ag2
  (set (make-local-variable 'compilation-error-face) grep-hit-face)
  (add-hook 'compilation-filter-hook 'grep-filter nil t))

(provide 'ag2)
