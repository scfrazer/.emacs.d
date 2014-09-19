;;; ag2.el

(require 'compile)
(require 'grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup ag2 nil
  "View results from 'ag'."
  :group 'tools)

(defcustom ag2-executable
  "ag"
  "The ag executable."
  :type 'string
  :group 'ag2)

(defcustom ag2-default-all-text nil
  "Use --all-text."
  :type 'boolean
  :group 'ag2)
(defvar ag2-option-all-text nil)

(defcustom ag2-default-all-types nil
  "Use --all-types."
  :type 'boolean
  :group 'ag2)
(defvar ag2-option-all-types nil)

(defcustom ag2-default-case-sensitive nil
  "Use --case-sensitive."
  :type 'boolean
  :group 'ag2)
(defvar ag2-option-case-sensitive nil)

(defcustom ag2-default-depth nil
  "Use --depth NUM."
  :type 'integer
  :group 'ag2)
(defvar ag2-option-depth nil)

(defcustom ag2-default-file-types nil
  "List of file types to search."
  :type '(repeat string)
  :group 'ag2)
(defvar ag2-option-file-types nil)

(defcustom ag2-default-follow nil
  "Use --follow."
  :type 'boolean
  :group 'ag2)
(defvar ag2-option-follow nil)

(defcustom ag2-default-hidden nil
  "Use --hidden."
  :type 'boolean
  :group 'ag2)
(defvar ag2-option-hidden nil)

(defcustom ag2-default-ignore nil
  "Use --ignore PATTERN."
  :type 'string
  :group 'ag2)
(defvar ag2-option-ignore nil)

(defcustom ag2-default-ignore-case nil
  "Use --ignore-case."
  :type 'boolean
  :group 'ag2)
(defvar ag2-option-ignore-case nil)

(defcustom ag2-default-ignore-dir nil
  "Use --ignore-dir NAME."
  :type 'string
  :group 'ag2)
(defvar ag2-option-ignore-dir nil)

(defcustom ag2-default-literal nil
  "Use --literal."
  :type 'boolean
  :group 'ag2)
(defvar ag2-option-literal nil)

(defcustom ag2-default-search-binary nil
  "Use --search-binary."
  :type 'boolean
  :group 'ag2)
(defvar ag2-option-search-binary nil)

(defcustom ag2-default-search-zip nil
  "Use --search-zip."
  :type 'boolean
  :group 'ag2)
(defvar ag2-option-search-zip nil)

(defcustom ag2-default-unrestricted nil
  "Use --unrestricted."
  :type 'boolean
  :group 'ag2)
(defvar ag2-option-unrestricted nil)

(defcustom ag2-default-word-regexp nil
  "Use --word-regexp."
  :type 'boolean
  :group 'ag2)
(defvar ag2-option-word-regexp nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ag2-search-history nil)
(defvar ag2-files-history nil)
(defvar ag2-dir-history nil)
(defvar ag2-types-history nil)
(defvar ag2-built-in-file-types nil)

(defun ag2-word-at-point ()
  "Get the closest word at point."
  (save-excursion
    (unless (and (not (eobp))
                 (member (char-syntax (char-after (point))) (list ?w ?_)))
      (skip-syntax-backward "^w_"))
    (skip-syntax-backward "w_")
    (let ((beg (point)))
      (skip-syntax-forward "w_")
      (buffer-substring-no-properties beg (point)))))

(defun ag2-options-to-string ()
  "Turn the ag2-option-* variables into a string."
  (let ((string ""))
    (when ag2-option-all-text
      (setq string (concat string " -t")))
    (when ag2-option-all-types
      (setq string (concat string " -a")))
    (when ag2-option-case-sensitive
      (setq string (concat string " -s")))
    (when ag2-option-depth
      (setq string
            (concat string " --depth " (number-to-string ag2-option-depth))))
    (when ag2-option-follow
      (setq string (concat string " -f")))
    (when ag2-option-hidden
      (setq string (concat string " --hidden")))
    (when ag2-option-ignore
      (setq string
            (concat string " --ignore "
                    (shell-quote-argument ag2-option-ignore))))
    (when ag2-option-ignore-case
      (setq string (concat string " -i")))
    (when ag2-option-ignore-dir
      (setq string
            (concat string " --ignore-dir "
                    (shell-quote-argument ag2-option-ignore-dir))))
    (when ag2-option-literal
      (setq string (concat string " -Q")))
    (when ag2-option-search-binary
      (setq string (concat string " --search-binary")))
    (when ag2-option-search-zip
      (setq string (concat string " -z")))
    (when ag2-option-unrestricted
      (setq string (concat string " -u")))
    (when ag2-option-word-regexp
      (setq string (concat string " -w")))
    string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ag2-popup-buffer-name " *ag2 options*")

(defvar ag2-popup-map nil)

(defvar ag2-popup-minibuffer-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-o")
      (lambda ()
        (interactive)
        (pop-to-buffer ag2-popup-buffer-name)))
    map)
  "Keymap for getting to popup options.")

(defun ag2-popup-start ()
  "Common popup start code."
  (with-current-buffer (get-buffer-create ag2-popup-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq ag2-popup-map (make-sparse-keymap))
    (define-key ag2-popup-map (kbd "RET")
      (lambda ()
        (interactive)
        (select-window (active-minibuffer-window))))))

(defun ag2-popup-end ()
  "Common popup end code."
  (with-current-buffer ag2-popup-buffer-name
    (insert "\n")
    (insert "Press C-o to switch to this buffer and change options\n")
    (insert "Press RET when finished to switch back to the minibuffer")
    (beginning-of-line)
    (use-local-map ag2-popup-map)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil))
  (pop-to-buffer ag2-popup-buffer-name)
  (fit-window-to-buffer))

(defun ag2-popup-insert-boolean (key-string option-string symbol)
  "Insert a boolean option."
  (define-key ag2-popup-map (kbd key-string)
    `(lambda () (interactive) (ag2-popup-toggle-option ,key-string (quote ,symbol))))
  (insert key-string ": " option-string)
  (when (symbol-value (intern-soft symbol))
    (put-text-property
     (point-at-bol) (point-at-eol) 'face 'font-lock-warning-face))
  (insert "\n"))

(defun ag2-popup-toggle-option (key-string symbol)
  "Toggle an option."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^" key-string ":") nil t)
      (set symbol (not (symbol-value symbol)))
      (setq buffer-read-only nil)
      (if (symbol-value symbol)
          (put-text-property
           (point-at-bol) (point-at-eol) 'face 'font-lock-warning-face)
        (put-text-property
         (point-at-bol) (point-at-eol) 'face 'default))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))))

(defun ag2-popup-search-options ()
  "Let user change default search options."
  (interactive)
  (ag2-popup-start)
  (with-current-buffer ag2-popup-buffer-name
    (ag2-popup-insert-boolean "Q" "--literal" 'ag2-option-literal)
    (ag2-popup-insert-boolean "i" "--ignore-case" 'ag2-option-ignore-case)
    (ag2-popup-insert-boolean "s" "--case-sensitive" 'ag2-option-case-sensitive)
    (ag2-popup-insert-boolean "w" "--word-regexp" 'ag2-option-word-regexp))
  (ag2-popup-end))

(defun ag2-popup-file-options ()
  "Let user change default file options."
  (interactive)
  (ag2-popup-start)
  (define-key ag2-popup-map (kbd "-") 'ag2-popup-choose-file-types)
  (with-current-buffer ag2-popup-buffer-name
    (ag2-popup-insert-file-types) (insert "\n")
    (ag2-popup-insert-boolean "a" "--all-types" 'ag2-option-all-types)
    (ag2-popup-insert-boolean "b" "--search-binary" 'ag2-option-search-binary)
    (ag2-popup-insert-boolean "f" "--follow" 'ag2-option-follow)
    (insert "i: --ignore PATTERN (TODO)\n")
    (ag2-popup-insert-boolean "h" "--hidden" 'ag2-option-hidden)
    (ag2-popup-insert-boolean "t" "--all-text" 'ag2-option-all-text)
    (ag2-popup-insert-boolean "u" "--unrestricted" 'ag2-option-unrestricted)
    (ag2-popup-insert-boolean "z" "--search-zip" 'ag2-option-search-zip))
  (ag2-popup-end))

(defun ag2-popup-insert-file-types ()
  "Insert file types option."
    (insert "-: Search file types")
    (when ag2-option-file-types
      (insert " = ")
      (dolist (type ag2-option-file-types)
        (insert type ","))
      (delete-char -1)
      (put-text-property
       (point-at-bol) (point-at-eol) 'face 'font-lock-warning-face)))

;; TODO User-defined types
(defun ag2-popup-choose-file-types ()
  "Choose file types."
  (interactive)
  (ag2-popup-get-built-in-file-types)
  (let ((enable-recursive-minibuffers t)
        (crm-default-separator ",")
        (type-string ""))
    (when ag2-option-file-types
      (dolist (type ag2-option-file-types)
        (setq type-string (concat type-string type ",")))
      (setq type-string (substring type-string 0 -1)))
    (setq ag2-option-file-types
          (completing-read-multiple
           "File types (comma separated): "
           ag2-built-in-file-types
           nil nil type-string 'ag2-types-history)))
  (goto-char (point-min))
  (re-search-forward "^-:" nil t)
  (setq buffer-read-only nil)
  (delete-region (point-at-bol) (point-at-eol))
  (ag2-popup-insert-file-types)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (goto-char (point-max))
  (beginning-of-line))

(defun ag2-popup-get-built-in-file-types ()
  "Get the file types ag knows about."
  (unless ag2-built-in-file-types
    (with-temp-buffer
      (shell-command (concat ag2-executable " --list-file-types") t)
      (goto-char (point-min))
      (while (re-search-forward "^\\s-+--\\([a-zA-Z0-9_]+\\)" nil t)
        (push (match-string-no-properties 1) ag2-built-in-file-types)))
    (setq ag2-built-in-file-types (sort ag2-built-in-file-types 'string<))))

(defun ag2-popup-dir-options ()
  "Let user change default directory options."
  (interactive)
  (ag2-popup-start)
  (insert "d: --depth NUM (TODO)\n")
  (insert "i: --ignore-dir NAME (TODO)\n")
  (ag2-popup-end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun ag2 (&optional arg)
  "Run 'ag'.  With prefix arg, take search string from region."
  (interactive "P")
  (setq ag2-option-file-types (copy-sequence ag2-default-file-types))
  (setq ag2-option-all-text ag2-default-all-text
        ag2-option-all-types ag2-default-all-types
        ag2-option-case-sensitive ag2-default-case-sensitive
        ag2-option-depth ag2-default-depth
        ag2-option-follow ag2-default-follow
        ag2-option-hidden ag2-default-hidden
        ag2-option-ignore ag2-default-ignore
        ag2-option-ignore-case ag2-default-ignore-case
        ag2-option-ignore-dir ag2-default-ignore-dir
        ag2-option-literal ag2-default-literal
        ag2-option-search-binary ag2-default-search-binary
        ag2-option-search-zip ag2-default-search-zip
        ag2-option-unrestricted ag2-default-unrestricted
        ag2-option-word-regexp ag2-default-word-regexp)
  (let ((default-search
          (if (and arg (mark t))
              (buffer-substring-no-properties (region-beginning) (region-end))
            (ag2-word-at-point)))
        (default-dir default-directory)
        search-string search-files search-dir)
    (unwind-protect
        (progn
          ;; Search
          (ag2-popup-search-options)
          (setq search-string
                (read-from-minibuffer
                 (concat "Search for (default \"" default-search "\"): ")
                 nil ag2-popup-minibuffer-map nil 'ag2-search-history default-search))
          (when (string= search-string "")
            (setq search-string default-search))
          ;; File
          (ag2-popup-file-options)
          (setq search-files
                (read-from-minibuffer
                 "Filename regexp (default to all): "
                 nil ag2-popup-minibuffer-map nil 'ag2-files-history))
          ;; Directory
          (ag2-popup-dir-options)
          (setq search-dir
                (read-from-minibuffer
                 "In directory: "
                 default-dir ag2-popup-minibuffer-map nil 'ag2-dir-history))
          (when (string= search-dir "")
            (setq search-dir default-dir))
          ;; Cleanup
          (with-current-buffer ag2-popup-buffer-name
            (kill-buffer-and-window)))
      (with-current-buffer ag2-popup-buffer-name
        (kill-buffer-and-window)))
    ;; Execute
    (let ((default-directory search-dir)
          (type-string ""))
      (dolist (type ag2-option-file-types)
        (setq type-string (concat type-string " --" type)))
      (compilation-start
       (concat ag2-executable
               type-string
               " --nogroup --column --color --color-match 1\\;31"
               (ag2-options-to-string)
               (if (string= "" search-files) "" (concat " -G " (shell-quote-argument search-files)))
               " -- " (shell-quote-argument search-string))
       'ag2-mode))))

;;;###autoload
(defun ag2-local (&optional arg)
  "Run 'ag' with depth 0.  With prefix arg, take search string from region."
  (interactive "P")
  (let ((ag2-default-depth 0))
    (call-interactively 'ag2)))

;;;###autoload
(define-compilation-mode ag2-mode "ag2"
  "Mode for viewing results from 'ag'."
  :group 'ag2
  (set (make-local-variable 'compilation-error-face) grep-hit-face)
  (add-hook 'compilation-filter-hook 'grep-filter nil t))

(provide 'ag2)
