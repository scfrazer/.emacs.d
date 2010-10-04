;;; cc-status.el

(defvar cc-status-item-regexp "^[IUXDA ] [RU ] \\([-a-zA-Z0-9_./]+\\)$"
  "Item regexp.")

(defvar cc-status-tree-dir-name nil
  "ClearCase status tree directory name.")
(make-variable-buffer-local 'cc-status-tree-dir-name)

;; Interactive functions

(defun cc-status-tree ()
  "Create ClearCase status tree."
  (interactive)
  (let ((buf (get-buffer-create "*cc-status-tree*")))
    (set-buffer buf)
    (unless cc-status-tree-dir-name
      (setq cc-status-tree-dir-name
            (replace-regexp-in-string
             "/$" ""
             (read-directory-name "ClearCase status for dir? "
                                  default-directory nil t)))
      (cc-status-tree-refresh)
      (cc-status-tree-mode))
    (pop-to-buffer buf)
    (fit-window-to-buffer nil (/ (frame-height) 2))))

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

(defun cc-status-tree-refresh ()
  "Refresh the cc-status buffer."
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert cc-status-tree-dir-name ":\n\n")
  (insert "  % Filename\n")
  (insert "  - --------\n")
  (call-process-shell-command (concat
                               "(cleartool lscheckout -me -r -fmt '%n (%Rf)\\n' "
                               cc-status-tree-dir-name
                               " ; cleartool lspri -other "
                               cc-status-tree-dir-name ")") nil t)
  (goto-char (point-min))
  (cc-status-mode-next)
  (sort-regexp-fields nil cc-status-item-regexp "\\1" (point) (point-max))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

;; Map

(defvar cc-status-tree-mode-map nil
  "`cc-status-mode' keymap.")

(if (not cc-status-tree-mode-map)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<down>") 'cc-status-mode-next)
      (define-key map (kbd "<up>") 'cc-status-mode-prev)
      (define-key map (kbd "C-n") 'cc-status-mode-next)
      (define-key map (kbd "C-p") 'cc-status-mode-prev)
      (define-key map "n" 'cc-status-mode-next)
      (define-key map "p" 'cc-status-mode-prev)

      ;; TODO
      ;; Buffer commands
      ;; q - bury buffer
      ;; g - refresh
      ;; RET - open file
      ;; j - jump to a different dir
      ;; x - execute commands
      ;;
      ;; Element files/dirs:
      ;; i - mark for checkin
      ;; u - mark for unco -keep
      ;; U - mark for unco -rm
      ;; = - ediff against predecessor
      ;;
      ;; Non-element files/dirs:
      ;; d - mark for delete
      ;; a - mark for mkelem

      (setq cc-status-tree-mode-map map)))

;; Font-lock

(defvar cc-status-mode-font-lock-keywords
  '(
    ("^  % .+$"
     (0 font-lock-keyword-face))
    ("^  - -+$"
     (0 font-lock-keyword-face))
    ("^\\(.+\\):$"
     (1 font-lock-function-name-face))
    ("^  [RU] .+"
     (0 font-lock-preprocessor-face))
    ("^[IA].+"
     (0 font-lock-variable-name-face))
    ("^[UXD].+"
     (0 font-lock-warning-face))
    )
  "Keyword highlighting specification for cc-status.")

;; Mode

(defun cc-status-tree-mode ()
  "Major mode for working with ClearCase tree status.

Key Bindings:

\\{cc-status-tree-mode-map}"
  (interactive)
  (setq truncate-lines t)
  (setq major-mode 'cc-status-tree-mode)
  (setq mode-name "cc-status-tree")
  (use-local-map cc-status-tree-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(cc-status-mode-font-lock-keywords))
  (turn-on-font-lock)
  (run-hooks 'cc-status-tree-mode-hook))

(provide 'cc-status)
