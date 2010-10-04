;;; cc-status.el

;; TODO Checked out files (R/U) can be checked in, or unco -rm/-keep
;; TODO Uncatalogued files can be deleted or added (mkelem)

(defvar cc-status-item-regexp "^[IUXDA ] [RU ] \\([-a-zA-Z0-9_./]+\\)$"
  "Item regexp.")

(defvar cc-status-dir-name nil
  "ClearCase status directory name.")

;; Interactive functions

(defun cc-status-tree ()
  "Create ClearCase status tree."
  (interactive)
  (let ((dir (read-directory-name "ClearCase status for dir? " default-directory nil t)))
    (pop-to-buffer (get-buffer-create "*cc-status-tree*"))
    (set (make-local-variable 'cc-status-dir-name) dir)
    (cc-status-tree-populate)
    (cc-status-tree-mode)))

(defun cc-status-mode-next ()
  "Go to next file."
  (interactive)
  (when (re-search-forward cc-status-item-regexp nil t)
    (goto-char (match-beginning 1))))

(defun cc-status-mode-prev ()
  "Go to previous file."
  (interactive)
  (when (re-search-backward cc-status-item-regexp nil t)
    (goto-char (match-beginning 1))))

;; Non-interactive functions

(defun cc-status-tree-populate ()
  "Populate the cc-status-mode buffer."
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "  % Filename\n")
  (insert "  - --------\n")
  (call-process-shell-command (concat
                               "(cleartool lscheckout -me -r -fmt '%n (%Rf)\\n' "
                               cc-status-dir-name
                               " ; cleartool lspri -other "
                               cc-status-dir-name ")") nil t)
  (goto-char (point-min))
  (cc-status-mode-next)
  (sort-regexp-fields nil cc-status-item-regexp "\\1" (point) (point-max))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

;; Map

(defvar cc-status-mode-map nil
  "`cc-status-mode' keymap.")

(if (not cc-status-mode-map)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<down>") 'cc-status-mode-next)
      (define-key map (kbd "<up>") 'cc-status-mode-prev)
      (define-key map (kbd "C-n") 'cc-status-mode-next)
      (define-key map (kbd "C-p") 'cc-status-mode-prev)
      (define-key map "n" 'cc-status-mode-next)
      (define-key map "p" 'cc-status-mode-prev)
      (setq cc-status-mode-map map)))

;; Mode

(defun cc-status-tree-mode ()
  "Major mode for working with ClearCase tree status.

Key Bindings:

\\{cc-status-mode-map}"
  (interactive)
  (setq truncate-lines t)
  (setq major-mode 'cc-status-mode)
  (setq mode-name "cc-status")
  (use-local-map cc-status-mode-map)
;   (set (make-local-variable 'font-lock-defaults) '(cc-status-mode-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'cc-status-mode-hook))

(provide 'cc-status)
