;;; my-iswitchb.el

(require 'iswitchb)
(require 'bookmark)
(require 'my-imenu)
(require 'my-recentf)

;;(iswitchb-mode 1)
(setq iswitchb-regexp t)
(setq iswitchb-max-to-show 8)

;; iswitchb + bookmarks

(defun my-iswitchb-dired-jump ()
  "Interactively switch to bookmark using `iswitchb'."
  (interactive)
  (let* ((iswitchb-make-buflist-hook
          (lambda()
            (setq iswitchb-temp-buflist (bookmark-all-names))))
         (name (iswitchb-read-buffer "Jump to bookmark: "))
         (bmk (bookmark-get-bookmark name)))
    (when bmk
      (setq bookmark-alist (delete bmk bookmark-alist))
      (push bmk bookmark-alist)
      (let ((filename (bookmark-get-filename name)))
        (if (string-match "/$" filename)
            (if (and (boundp 'dired-buffers)
                     dired-buffers
                     (buffer-live-p (cdar dired-buffers)))
                (progn
                  (switch-to-buffer (cdar dired-buffers))
                  (find-alternate-file filename))
              (dired filename))
          (bookmark-jump name))))))

;; iswitchb + imenu

(defun my-iswitchb-imenu-add-symbols (symbol-list result)
  (when (listp symbol-list)
    (mapcar (lambda (symbol)
              (cond ((and (listp symbol) (imenu--subalist-p symbol))
                     (setq result (my-iswitchb-imenu-add-symbols symbol result)))
                    ((listp symbol)
                     (push symbol result))
                    ((stringp symbol)
                     (let ((pos (get-text-property 1 'org-imenu-marker symbol)))
                       (when pos
                         (push (cons symbol pos) result))))))
            symbol-list))
  result)

(defun my-iswitchb-imenu-goto-symbol ()
  "Goto to an imenu symbol using iswitchb."
  (interactive)
  (imenu--make-index-alist)
  (let (items)
    (setq items (nreverse (my-iswitchb-imenu-add-symbols imenu--index-alist items)))
    (let* ((iswitchb-make-buflist-hook
            (lambda ()
              (setq iswitchb-temp-buflist (mapcar 'car items))))
           (name (iswitchb-read-buffer "Goto: "))
           (pos (cdr (assoc name items))))
      (when pos
        (goto-char pos)))))

;; iswitchb + recentf

(defun my-iswitchb-recentf-file ()
  "find a file in the recently open file using iswitchb for completion"
  (interactive)
  (let* ((all-files recentf-list)
         (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
         (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
         (iswitchb-make-buflist-hook
          (lambda ()
            (setq iswitchb-temp-buflist filename-list)))
         (filename (iswitchb-read-buffer "Find Recent File: "))
         (result-list (delq nil (mapcar (lambda (x) (if (string= (car x) filename) (cdr x))) file-assoc-list)))
         (result-length (length result-list)))
    (find-file
     (cond
      ((= result-length 0) filename)
      ((= result-length 1) (car result-list))
      (t
       (let ((iswitchb-make-buflist-hook
              (lambda ()
                (setq iswitchb-temp-buflist result-list))))
         (iswitchb-read-buffer (format "%d matches:" result-length))))))))

;; Setup keys

(defun my-iswitchb-local-keys ()
  (define-key iswitchb-mode-map (kbd "<right>") 'iswitchb-next-match)
  (define-key iswitchb-mode-map (kbd "<left>") 'iswitchb-prev-match)
  (define-key iswitchb-mode-map (kbd "<up>") 'nil)
  (define-key iswitchb-mode-map (kbd "<down>") 'nil))
(add-hook 'iswitchb-define-mode-map-hook 'my-iswitchb-local-keys)

(provide 'my-iswitchb)
