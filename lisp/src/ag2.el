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

(defconst ag2-options-buffer-name " *ag2 options*")

(defun ag2-popup-insert-boolean (key-string option-string symbol)
  "Insert a boolean option."
  (define-key ag2-popup-map (kbd key-string)
    `(lambda () (interactive) (ag2-popup-toggle-option ,key-string (quote ,symbol))))
  (insert key-string ": " option-string)
  (when (symbol-value (intern-soft symbol))
    (put-text-property (point-at-bol) (point-at-eol) 'face 'font-lock-warning-face))
  (insert "\n"))

(defun ag2-popup-toggle-option (key-string symbol)
  "Toggle an option."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^" key-string ":") nil t)
      (set symbol (not (symbol-value symbol)))
      (setq buffer-read-only nil)
      (if (symbol-value symbol)
          (put-text-property (point-at-bol) (point-at-eol) 'face 'font-lock-warning-face)
        (put-text-property (point-at-bol) (point-at-eol) 'face 'default))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))))

(defun ag2-popup-map nil)

(defun ag2-popup-search-options ()
  "Let user change default search options."
  (interactive)
  (with-current-buffer (get-buffer-create ag2-options-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq ag2-popup-map (make-sparse-keymap))
    (define-key ag2-popup-map (kbd "RET")
      (lambda ()
        (interactive)
        (kill-buffer-and-window)
        (select-window (active-minibuffer-window))))
    (ag2-popup-insert-boolean "Q" "--literal" 'ag2-option-literal)
    (ag2-popup-insert-boolean "i" "--ignore-case" 'ag2-option-ignore-case)
    (ag2-popup-insert-boolean "s" "--case-sensitive" 'ag2-option-case-sensitive)
    (ag2-popup-insert-boolean "w" "--word-regexp" 'ag2-option-word-regexp)
    (insert "\n")
    (insert "Press RET when finished")
    (use-local-map ag2-popup-map)
    (beginning-of-line)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil))
  (pop-to-buffer ag2-options-buffer-name)
  (fit-window-to-buffer))

(defun ag2-popup-file-options ()
  "Let user change default file options."
  (interactive)
  (message "TODO file"))

(defun ag2-popup-dir-options ()
  "Let user change default directory options."
  (interactive)
  (message "TODO dir"))

(defvar ag2-search-local-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "M--") 'ag2-popup-search-options)
    map)
  "Keymap for getting to search options.")

(defvar ag2-file-local-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "M--") 'ag2-popup-file-options)
    map)
  "Keymap for getting to file options.")

(defvar ag2-dir-local-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "M--") 'ag2-popup-dir-options)
    map)
  "Keymap for getting to directory options.")

;;;###autoload
(defun ag2 (&optional arg)
  "Run 'ag'.  With prefix arg, take search string from region."
  (interactive "P")
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
  (let* ((default-string
           (if (and arg (mark t))
               (buffer-substring-no-properties (region-beginning) (region-end))
             (ag2-word-at-point)))
         (search-string
          (read-from-minibuffer
           (concat "Search for (default \"" default-string "\"): ")
           nil ag2-search-local-map nil 'ag2-search-history default-string))
         (search-files
          (read-from-minibuffer
           "Filename regexp (default to all): "
           nil ag2-file-local-map nil 'ag2-files-history))
         (search-dir
          (read-from-minibuffer
           "In directory: "
           default-directory ag2-dir-local-map nil 'ag2-dir-history)))
    (when (string= search-string "")
      (setq search-string default-string))
    (when (string= search-dir "")
      (setq search-dir default-directory))
    (let ((default-directory search-dir))
      (compilation-start
       (concat ag2-executable
               " --nogroup --line-number --column --color --color-match 1\\;31"
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
