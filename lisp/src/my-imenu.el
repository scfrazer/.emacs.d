;;; my-imenu-extras.el

(require 'imenu)
(require 'my-ido)

(setq imenu-max-items 25)
(setq imenu-max-length 100)
(setq imenu-sort-function nil)
(add-hook 'imenu-after-jump-hook 'recenter)

;; ido + imenu

(defvar my-ido-imenu-prefix-symbols nil
  "*Prefix symbols with their menu drill-downs.")
(make-local-variable 'my-ido-imenu-prefix-symbols)

(defun my-ido-imenu-add-symbols (prefix symbols result)
  (when (listp symbols)
    (dolist (symbol symbols)
      (cond ((and (listp symbol) (imenu--subalist-p symbol))
             (setq result (my-ido-imenu-add-symbols (if my-ido-imenu-prefix-symbols
                                                        (concat prefix (car symbol) " > ")
                                                      "") symbol result)))
            ((listp symbol)
             (push (cons (concat (when my-ido-imenu-prefix-symbols prefix) (car symbol)) (cdr symbol)) result))
            ((stringp symbol)
             (let ((pos (get-text-property 1 'org-imenu-marker symbol)))
               (when pos
                 (push (cons (concat (when my-ido-imenu-prefix-symbols prefix) symbol) pos) result)))))))
  result)

(defun my-ido-imenu-goto-symbol ()
  "Goto to an imenu symbol using ido"
  (interactive)
  (imenu--cleanup)
  (setq imenu--index-alist nil)
  (imenu--make-index-alist)
  (let ((items nil)
        (guess (concat "\\(.+::\\)?"
                       (buffer-substring-no-properties
                        (save-excursion (skip-syntax-backward "w_") (point))
                        (save-excursion (skip-syntax-forward "w_") (point))))))
    (setq items (nreverse (my-ido-imenu-add-symbols nil imenu--index-alist items)))
    (catch 'done
      (dotimes (idx (length items))
        (if (string-match guess (caar items))
            (throw 'done t)
          (when (cdr items)
            (setq items (nconc (cdr items) (list (car items))))))))
    (let ((pos (cdr (assoc (ido-completing-read "Goto symbol: " (mapcar 'car items) nil t) items))))
      (when pos
        (let ((recenter (not (pos-visible-in-window-p pos))))
          (goto-char pos)
          (when recenter
            (recenter)))))))

;; Better imenu entry point

(defun imenu (index-item)
  "Jump to a place in the buffer chosen using a buffer menu or mouse menu.
INDEX-ITEM specifies the position.  See `imenu-choose-buffer-index'
for more information."
  (interactive (list (imenu-choose-buffer-index)))
  (if (stringp index-item)
      (setq index-item (assoc index-item (imenu--make-index-alist))))
  (and index-item
       (progn
         (push-mark)
         (let* ((is-special-item (listp (cdr index-item)))
                (function
                 (if is-special-item
                     (nth 2 index-item) imenu-default-goto-function))
                (position (if is-special-item
                              (cadr index-item) (cdr index-item)))
                (rest (if is-special-item (cddr index-item))))
           (apply function (car index-item) position rest)))
       (run-hooks 'imenu-after-jump-hook)))

(provide 'my-imenu)
