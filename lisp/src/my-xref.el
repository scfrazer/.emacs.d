;;; my-xref.el

(require 'xref)

(setq-default default-tags-table-function 'my-tags-table-function
              tags-add-tables t
              tags-case-fold-search nil
              tags-revert-without-query t)

(defun my-tags-table-function ()
  "Locate any dominating TAGS file."
  (let ((tags-dir (locate-dominating-file default-directory "TAGS")))
    (when tags-dir
      (concat tags-dir "TAGS"))))

(defun my-xref-pop-marker-stack-kill-buffer ()
  (interactive)
  (let ((buf (current-buffer)))
    (xref-pop-marker-stack)
    (unless (equal (current-buffer) buf)
      (kill-buffer buf))))

(defun my-xref-next-line ()
  (interactive)
  (end-of-line)
  (re-search-forward "^\\s-*[0-9]+:" nil t)
  (beginning-of-line))

(defun my-xref-prev-line ()
  (interactive)
  (beginning-of-line)
  (re-search-backward "^\\s-*[0-9]+:" nil t))

(defun my-xref--show-xref-buffer (xrefs alist)
  (with-current-buffer (get-buffer xref-buffer-name)
    (fit-window-to-buffer nil (/ (frame-height) 2))
    (my-xref-next-line)))
(advice-add #'xref--show-xref-buffer :after #'my-xref--show-xref-buffer)

(define-key xref--button-map (kbd "RET") #'xref-quit-and-goto-xref)
(define-key xref--button-map (kbd "TAB")  #'xref-goto-xref)
(define-key xref--xref-buffer-mode-map (kbd "n") #'my-xref-next-line)
(define-key xref--xref-buffer-mode-map (kbd "p") #'my-xref-prev-line)

(provide 'my-xref)
