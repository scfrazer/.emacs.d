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

(defcustom ag2-files-aliases-alist nil
  "Alist of aliases for the filename regexp.
If the key matches the filename regexp input exactly, the
corresponding value will be used instead."
  :type '(alist :key-type string :value-type string)
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

(defcustom ag2-default-file-type nil
  "File type to search."
  :type 'string
  :group 'ag2)
(defvar ag2-option-file-type nil)

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
(defvar ag2-type-history nil)
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
    (define-key map (kbd "M--")
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
    (setq ag2-popup-map (make-sparse-keymap))))

(defun ag2-popup-end ()
  "Common popup end code."
  (with-current-buffer ag2-popup-buffer-name
    (setq buffer-read-only nil)
    (insert "\n")
    (insert "Press M-- to switch to this buffer, then choose an option\n")
    (beginning-of-line)
    (use-local-map ag2-popup-map)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil))
  (pop-to-buffer ag2-popup-buffer-name)
  (fit-window-to-buffer))

(defun ag2-popup-insert-boolean (key-string option-string symbol)
  "Insert a boolean option."
  (define-key ag2-popup-map (kbd key-string)
    `(lambda () (interactive) (ag2-popup-toggle-boolean ,key-string (quote ,symbol))))
  (setq buffer-read-only nil)
  (insert key-string ": " option-string)
  (when (symbol-value (intern-soft symbol))
    (put-text-property
     (point-at-bol) (point-at-eol) 'face 'font-lock-warning-face))
  (insert "\n")
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

(defun ag2-popup-toggle-boolean (key-string symbol)
  "Toggle an option."
  (goto-char (point-min))
  (re-search-forward (concat "^" key-string ":") nil t)
  (beginning-of-line)
  (set symbol (not (symbol-value symbol)))
  (setq buffer-read-only nil)
  (if (symbol-value symbol)
      (put-text-property
       (point-at-bol) (point-at-eol) 'face 'font-lock-warning-face)
    (put-text-property
     (point-at-bol) (point-at-eol) 'face 'default))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (select-window (active-minibuffer-window)))

(defun ag2-popup-insert-option-line (key-string option-string symbol &optional newline)
  "Insert option line text."
  (setq buffer-read-only nil)
  (delete-region (point-at-bol) (point-at-eol))
  (insert key-string ": " option-string)
  (let ((value (symbol-value (intern-soft symbol))))
    (when value
      (insert " = ")
      (cond ((stringp value)
             (insert value))
            ((numberp value)
             (insert (number-to-string value))))
      (put-text-property
       (point-at-bol) (point-at-eol) 'face 'font-lock-warning-face)))
  (when newline
    (insert "\n"))
  (beginning-of-line)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

(defun ag2-popup-insert-number (key-string option-string symbol prompt)
  "Insert a number option."
  (define-key ag2-popup-map (kbd key-string)
    `(lambda () (interactive) (ag2-popup-set-number-option ,key-string ,option-string (quote ,symbol) ,prompt)))
  (ag2-popup-insert-option-line key-string option-string symbol t))

(defun ag2-popup-set-number-option (key-string option-string symbol prompt)
  "Set a number option."
  (goto-char (point-min))
  (re-search-forward (concat "^" key-string ":") nil t)
  (beginning-of-line)
  (let* ((enable-recursive-minibuffers t)
         (sym-val (symbol-value symbol))
         (value (read-from-minibuffer
                 prompt (and sym-val (number-to-string sym-val)))))
    (if (string= value "")
        (set symbol nil)
      (set symbol (string-to-number value))))
  (ag2-popup-insert-option-line key-string option-string symbol)
  (select-window (active-minibuffer-window)))

(defun ag2-popup-insert-string (key-string option-string symbol prompt)
  "Insert a string option."
  (define-key ag2-popup-map (kbd key-string)
    `(lambda () (interactive) (ag2-popup-set-string-option ,key-string ,option-string (quote ,symbol) ,prompt)))
  (ag2-popup-insert-option-line key-string option-string symbol t))

(defun ag2-popup-set-string-option (key-string option-string symbol prompt)
  "Set a string option."
  (goto-char (point-min))
  (re-search-forward (concat "^" key-string ":") nil t)
  (beginning-of-line)
  (let* ((enable-recursive-minibuffers t)
         (value (read-from-minibuffer prompt (symbol-value symbol))))
    (if (string= value "")
        (set symbol nil)
      (set symbol value)))
  (ag2-popup-insert-option-line key-string option-string symbol)
  (select-window (active-minibuffer-window)))

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
  (define-key ag2-popup-map (kbd "-") 'ag2-popup-choose-file-type)
  (with-current-buffer ag2-popup-buffer-name
    (ag2-popup-insert-option-line "-" "Search file type" 'ag2-option-file-type t)
    (ag2-popup-insert-boolean "a" "--all-types" 'ag2-option-all-types)
    (ag2-popup-insert-boolean "b" "--search-binary" 'ag2-option-search-binary)
    (ag2-popup-insert-boolean "f" "--follow" 'ag2-option-follow)
    (ag2-popup-insert-string "i" "--ignore" 'ag2-option-ignore "Ignore file/directory regexp: ")
    (ag2-popup-insert-boolean "h" "--hidden" 'ag2-option-hidden)
    (ag2-popup-insert-boolean "t" "--all-text" 'ag2-option-all-text)
    (ag2-popup-insert-boolean "u" "--unrestricted" 'ag2-option-unrestricted)
    (ag2-popup-insert-boolean "z" "--search-zip" 'ag2-option-search-zip))
  (ag2-popup-end))

(defun ag2-popup-choose-file-type ()
  "Choose file type."
  (interactive)
  (ag2-popup-get-built-in-file-types)
  (goto-char (point-min))
  (re-search-forward "^-:" nil t)
  (beginning-of-line)
  (let ((enable-recursive-minibuffers t))
    (setq ag2-option-file-type
          (completing-read "File type: " ag2-built-in-file-types
           nil t ag2-option-file-type 'ag2-type-history)))
  (when (string= ag2-option-file-type "")
    (setq ag2-option-file-type nil))
  (ag2-popup-insert-option-line "-" "Search file type" 'ag2-option-file-type)
  (select-window (active-minibuffer-window)))

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
  (ag2-popup-insert-number "d" "--depth" 'ag2-option-depth "Depth: ")
  (ag2-popup-end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun ag2 (&optional arg)
  "Run 'ag'.  With prefix arg, take search string from region."
  (interactive "P")
  (setq ag2-option-all-text ag2-default-all-text
        ag2-option-all-types ag2-default-all-types
        ag2-option-case-sensitive ag2-default-case-sensitive
        ag2-option-depth ag2-default-depth
        ag2-option-file-type ag2-default-file-type
        ag2-option-follow ag2-default-follow
        ag2-option-hidden ag2-default-hidden
        ag2-option-ignore ag2-default-ignore
        ag2-option-ignore-case ag2-default-ignore-case
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
          (let ((cell (assoc search-files ag2-files-aliases-alist)))
            (when cell
              (setq search-files (cdr cell))))
          ;; Directory
          (ag2-popup-dir-options)
          (setq search-dir
                (read-from-minibuffer
                 "In directory: "
                 default-dir ag2-popup-minibuffer-map nil 'ag2-dir-history))
          (when (string= search-dir "")
            (setq search-dir default-dir)))
      ;; Cleanup
      (with-current-buffer ag2-popup-buffer-name
        (kill-buffer-and-window)))
    ;; Execute
    (let ((default-directory search-dir))
      (compilation-start
       (concat ag2-executable
               (if ag2-option-file-type (concat " --" ag2-option-file-type) "")
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
