;;; my-debug.el

(require 'll-debug)

(setq-default ll-debug-output-prefix (concat "DEBUG-" (getenv "USER") "-")
              ll-debug-print-filename nil)

(defvar my-debug-str (concat "DEBUG$\\|" ll-debug-output-prefix))

(defun my-debug-insert-ll (&optional arg)
  "Swap default style of ll-debug-insert."
  (interactive "*P")
  (ll-debug-insert (if arg nil 1)))

(defun my-debug-insert-line ()
  "Insert a commented DEBUG line."
  (interactive "*")
  (beginning-of-line)
  (unless (looking-at "^\\s-*$")
    (newline)
    (forward-line -1))
  (insert "DEBUG")
  (comment-region (point-at-bol) (point-at-eol)))

(defun my-debug-comment-region ()
  "Comment out the current region and insert a DEBUG line above it."
  (interactive "*")
  (let ((beg (save-excursion (goto-char (region-beginning)) (point-at-bol)))
        (end (save-excursion (goto-char (region-end)) (point-at-bol))))
    (when (= beg end)
      (setq end (point-at-bol 2)))
    (comment-region beg end)
    (save-excursion
      (goto-char beg)
      (my-debug-insert-line))))

(defun my-debug-comment-region-after-copy ()
  "Insert a copy of the region and comment the original with a DEBUG line above it."
  (interactive "*")
  (let ((beg (save-excursion (goto-char (region-beginning)) (point-at-bol)))
        (end (save-excursion (goto-char (region-end)) (point-at-bol))))
    (when (= beg end)
      (setq end (point-at-bol 2)))
    (kill-ring-save beg end)
    (goto-char end)
    (save-excursion (yank))
    (comment-region beg end)
    (save-excursion
      (goto-char beg)
      (my-debug-insert-line))))

(defun my-debug-next ()
  "Go to next DEBUG statement."
  (interactive)
  (forward-char 1)
  (if (re-search-forward my-debug-str nil t)
      (goto-char (match-beginning 0))
    (backward-char 1)
    (error "No more debug statements")))

(defun my-debug-previous ()
  "Go to previous DEBUG statement."
  (interactive)
  (unless (re-search-backward my-debug-str nil t)
    (error "No more debug statements")))

(defun my-debug-occur ()
  "Run occur for DEBUG in the current buffer."
  (interactive)
  (let ((case-fold-search nil))
    (occur my-debug-str)))

(defun my-debug-multi-occur ()
  "Run occur for DEBUG in all buffers."
  (interactive)
  (let ((case-fold-search nil))
    (multi-occur-in-matching-buffers ".+" my-debug-str)))

(defvar my-debug-isearch nil)
(defun my-debug-isearch-mode-hook ()
  (when my-debug-isearch
    (setq my-debug-isearch nil)
    (setq isearch-string my-debug-str
          isearch-message my-debug-str)))
(add-hook 'isearch-mode-hook 'my-debug-isearch-mode-hook)

(defun my-debug-isearch-forward ()
  "Use isearch to look for DEBUG."
  (interactive)
  (setq my-debug-isearch t)
  (call-interactively 'isearch-forward-regexp))

(defun my-debug-isearch-backward()
  "Use isearch to look for DEBUG."
  (interactive)
  (setq my-debug-isearch t)
  (call-interactively 'isearch-backward-regexp))

(define-prefix-command 'my-debug-map)
(define-key my-debug-map (kbd "C") 'my-debug-comment-region-after-copy)
(define-key my-debug-map (kbd "C-r") 'my-debug-isearch-backward)
(define-key my-debug-map (kbd "C-s") 'my-debug-isearch-forward)
(define-key my-debug-map (kbd "O") 'my-debug-multi-occur)
(define-key my-debug-map (kbd "R") 'll-debug-revert)
(define-key my-debug-map (kbd "c") 'my-debug-comment-region)
(define-key my-debug-map (kbd "d") 'my-debug-insert-line)
(define-key my-debug-map (kbd "i") 'my-debug-insert-ll)
(define-key my-debug-map (kbd "n") 'my-debug-next)
(define-key my-debug-map (kbd "o") 'my-debug-occur)
(define-key my-debug-map (kbd "p") 'my-debug-previous)
(define-key my-debug-map (kbd "r") 'll-debug-renumber)

(provide 'my-debug)
