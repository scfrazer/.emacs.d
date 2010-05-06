;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el

;; Time load time

(defvar *emacs-load-start* (current-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/org")

;; Need these first to avoid font-lock/dired issues

(require 'my-font-lock)
(require 'my-dired)

(require 'ahg)
(require 'csh-mode)
(require 'etags)
(require 'etags-select)
(require 'etags-stack)
(require 'etags-table)
(require 'iflipb)
(require 'filladapt)
(require 'magit)
(require 'mdabbrev)
(require 'midnight)
(require 'motion-and-kill-dwim)
(require 'protbuf)
(require 'redo+)
(require 'revbufs)
(require 'task)
(require 'uniquify)

(require 'my-bs)
(require 'my-bookmark)
(require 'my-cc-mode)
(require 'my-clearcase)
(require 'my-comment)
(require 'my-ediff)
(require 'my-erc)
(require 'my-grep-ed)
(require 'my-ido)
(require 'my-grep)
(require 'my-imenu)
(require 'my-increment-number)
(require 'my-isearch)
(require 'my-occur)
(require 'my-org)
(require 'my-pop-back)
(require 'my-recentf)
(require 'my-rect)
(require 'my-set-cursor-color)
(require 'my-yank-target)
(require 'my-yasnippet)

(require 'my-theme)

(autoload 'align "align" nil t)
(autoload 'align-regexp "align" nil t)
(autoload 'all "all" nil t)
(autoload 'antlr3-mode "antlr3-mode" "ANTLR code editing mode" t)
(autoload 'browse-kill-ring "browse-kill-ring" nil t)
(autoload 'compile "compile" nil t)
(autoload 'e-mode "e-mode" "Specman 'e' code editing mode" t)
(autoload 'elog-mode "elog-mode" nil t)
(autoload 'expand-abbrev "abbrev" nil t)
(autoload 'ffap "ffap" nil t)
(autoload 'file-template-auto-insert "file-template" nil t)
(autoload 'file-template-find-file-not-found-hook "file-template" nil t)
(autoload 'file-template-insert "file-template" nil t)
(autoload 'find-file-at-point "ffap" nil t)
(autoload 'find-files-glob "find-files" nil t)
(autoload 'grep-buffers "grep-buffers" nil t)
(autoload 'htmlize-region "htmlize" nil t)
(autoload 'js2-mode "js2" nil t)
(autoload 'll-debug-insert "ll-debug" nil t)
(autoload 'makefile-mode "make-mode" nil t)
(autoload 'my-confluence-highlight "my-confluence" nil t)
(autoload 'my-confluence-html "my-confluence" nil t)
(autoload 'rdl-mode "rdl-mode" nil t)
(autoload 'rst-mode "rst" "reStructured Text Mode" t)
(autoload 'specterx-mode "specterx-mode" "SpecterX mode" t)
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t)
(autoload 'vsif-mode "vsif-mode" "VSIF mode" t)

(show-paren-mode t)
(delete-selection-mode t)
(blink-cursor-mode 1)
(set-scroll-bar-mode nil)
(tool-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode -1)
(winner-mode 1)

(setq-default ahg-diff-use-git-format nil
              backup-inhibited t
              blink-matching-paren-distance nil
              browse-kill-ring-display-duplicates nil
              browse-kill-ring-highlight-current-entry nil
              browse-kill-ring-maximum-display-length 400
              browse-kill-ring-no-duplicates t
              browse-kill-ring-quit-action (quote kill-and-delete-window)
              browse-kill-ring-separator "----------8<----------8<----------8<----------8<----------8<----------"
              browse-kill-ring-separator-face (quote font-lock-keyword-face)
              browse-kill-ring-use-fontification t
              case-fold-search t
              column-number-mode t
              comment-column 0
              comment-fill-column 120
              compare-ignore-whitespace t
;;               compilation-error-regexp-alist nil
              cperl-break-one-line-blocks-when-indent nil
              cperl-continued-brace-offset -4
              cperl-continued-statement-offset 4
              cperl-fix-hanging-brace-when-indent nil
              cperl-highlight-variables-indiscriminately t
              cperl-indent-left-aligned-comments nil
              cperl-indent-level 4
              cperl-invalid-face (quote underline)
              cperl-label-offset -4
              cperl-merge-trailing-else nil
              cursor-in-non-selected-windows nil
              cursor-type 'box
              dabbrev-case-fold-search nil
              etags-select-use-short-name-completion t
              etags-table-search-up-depth 10
              even-window-heights nil
              ffap-url-regexp nil
              file-template-insert-automatically 'ask
              file-template-paths (list "~/.emacs.d/templates/")
              fill-column 78
              filladapt-mode t
              filladapt-mode-line-string ""
              flyspell-mode-map nil
              highlight-changes-active-string " Chg+"
              highlight-changes-passive-string " Chg-"
              highlight-changes-global-modes nil
              htmlize-output-type 'font
              ido-enable-tramp-completion nil
              iflipb-ignore-buffers 'my-bs-ignore-buffer
              indicate-buffer-boundaries t
              indent-tabs-mode nil
              inhibit-startup-message t
              isearch-lazy-highlight-initial-delay 0
              js2-basic-offset 4
              kill-whole-line t
              line-move-visual t
              line-number-mode t
              ll-debug-output-prefix (concat "DEBUG-" (getenv "USER") "-")
              lpr-command "lpr"
              lpr-lp-system t
              lpr-switches ""
              make-backup-files nil
              mouse-autoselect-window t
              mouse-highlight 1
              mouse-yank-at-point t
              nxml-sexp-element-flag t
              parens-require-spaces nil
              protect-buffer-bury-p nil
              ps-always-build-face-reference t
              ps-bold-faces (quote (font-lock-keyword-face font-lock-function-name-face))
              ps-bottom-margin 36
              ps-build-face-reference nil
              ps-font-size 8
              ps-header-font-size 10
              ps-header-lines 1
              ps-header-offset 12
              ps-header-title-font-size 10
              ps-inter-column 18
              ps-italic-faces (quote (font-lock-comment-face))
              ps-left-header (quote (ps-get-buffer-name))
              ps-left-margin 36
              ps-n-up-margin 18
              ps-n-up-printing 2
              ps-paper-type (quote letter)
              ps-print-color-p nil
              ps-right-header (quote ("/pagenumberstring load"))
              ps-right-margin 36
              ps-top-margin 36
              ps-underlined-faces (quote (font-lock-string-face))
              python-continuation-offset 4
              python-indent 4
              rst-mode-lazy nil
              save-abbrevs nil
              scroll-conservatively 10000
              scroll-preserve-screen-position t
              show-paren-delay 0
              tags-revert-without-query t
              tempo-interactive t
              uniquify-buffer-name-style 'forward
              user-mail-address (concat "<" (getenv "USER") "@cisco.com>")
              vc-handled-backends nil ;; maybe '(Hg) later
              verilog-auto-endcomments nil
              verilog-auto-indent-on-newline nil
              verilog-auto-lineup '(all)
              verilog-auto-newline nil
              verilog-case-indent 4
              verilog-indent-begin-after-if nil
              verilog-indent-level 4
              verilog-indent-level-behavioral 0
              verilog-indent-level-declaration 0
              verilog-indent-level-directive 0
              verilog-indent-level-module 0
              verilog-minimum-comment-distance 40
              verilog-tab-always-indent t
              verilog-imenu-flatten t
              verilog-imenu-qualify-names nil
              visible-bell t
              warning-suppress-types (list '(undo discard-info))
              winner-boring-buffers (list "*Completions*" "*Help*" "*Apropos*" "*buffer-selection*")
              winner-ring-size 50)

(add-to-list 'ffap-string-at-point-mode-alist
             '(file "--:\\\\$+<>@-Z_[:alpha:]~*?{}" "<@" "@>;.,!:"))

(setq frame-title-format "%F")

(setq default-mode-line-format
      '("  " mode-line-modified
        (list 'line-number-mode "  L%l/")
        (list 'line-number-mode (:eval (int-to-string (count-lines (point-min) (point-max)))))
        "  %p"
        (list 'column-number-mode "  C%c")
        "  " mode-line-buffer-identification
        "  " mode-line-modes
        (:eval (if (not clearcase-servers-online)
                   ""
                 (concat "  [View: " (or clearcase-setview-viewtag "** NONE **") "]")))
        (:eval (concat "  [Task: " (or task-current-name "NONE") "]"))))

(nbutlast mode-line-modes 1)

(add-to-list 'auto-mode-alist '("Makefile.*$" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.csh$" . csh-mode))
(add-to-list 'auto-mode-alist '("\\.cshrc$" . csh-mode))
(add-to-list 'auto-mode-alist '("\\.e$" . e-mode))
(add-to-list 'auto-mode-alist '("\\.elog$" . elog-mode))
(add-to-list 'auto-mode-alist '("\\.g$" . antlr3-mode))
(add-to-list 'auto-mode-alist '("\\.g3.*$" . antlr3-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.rdlh?$" . rdl-mode))
(add-to-list 'auto-mode-alist '("\\.s$" . specterx-mode))
(add-to-list 'auto-mode-alist '("\\.sv$" . verilog-mode))
(add-to-list 'auto-mode-alist '("\\.vh$" . verilog-mode))
(add-to-list 'auto-mode-alist '("very.*\\.log$" . elog-mode))
(add-to-list 'auto-mode-alist '("\\.vsif$" . vsif-mode))
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))

;; Don't use sh-mode for csh files

(dolist (elt interpreter-mode-alist)
  (when (member (car elt) (list "csh" "tcsh"))
    (setcdr elt 'csh-mode)))

;; Aliases

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'perl-mode 'cperl-mode)

;; isearch scroll

(setq isearch-allow-scroll t)
(put 'my-recenter 'isearch-scroll t)

;; Snippets

(yas/load-directory "~/.emacs.d/lisp/yasnippet/snippets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun my-7x14 ()
  "Switch to 7x14 font"
  (interactive)
  (set-frame-font "-sfrazer-*-medium-*-*-*-14-*-*-*-*-*-*-*"))

(defun my-9x15 ()
  "Switch to 9x15 font"
  (interactive)
  (set-frame-font "-sfrazer-*-medium-*-*-*-15-*-*-*-*-*-*-*"))

(defun my-align ()
  "Align declarations, etc."
  (interactive)
  (save-excursion
    (if (and transient-mark-mode mark-active)
        (align (region-beginning) (region-end))
      (align (point-at-bol) (point-at-eol)))))

(defun my-apply-macro-to-region-lines (top bottom)
  "Apply macro to region lines and deactivate mark"
  (interactive "r")
  (apply-macro-to-region-lines top bottom)
  (deactivate-mark))

(defun my-create-or-other-frame ()
  "Switch to other frame, creating one if necessary"
  (interactive)
  (if (> (length (frame-list)) 1)
      (let ((frame (get-other-frame)))
        (if (eq (frame-visible-p frame) 'icon)
            (make-frame-visible frame)
          (other-frame 1)))
    (make-frame-command)))

(defun my-clone-file (filename)
  "Clone the current buffer and write it into FILENAME."
  (interactive "FClone to file: ")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename nil nil nil 'confirm)))

(defun my-convert-to-base (arg)
  "Convert to decimal, or with prefix arg to hex."
  (interactive "P")
  (if arg
      (my-dec-to-hex)
    (my-hex-to-dec)))

(defun my-delete-whitespace-after-cursor ()
  "Delete spaces/tabs after cursor."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun my-delete-trailing-whitespace ()
  "Remove trailing spaces and excess blank lines in the buffer."
  (interactive "*")
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil t)
          (replace-match "\n\n" nil nil))
        (goto-char (point-min))
        (when (re-search-forward "\n\n+\\'" nil t)
          (replace-match "\n" nil nil))
        (message "Trailing whitespace and excess blank lines removed.")))))

(defun my-diff-buffer-with-file ()
  "View the differences between current buffer and it's associated file."
  (interactive)
  (let* ((buffer (current-buffer))
         (basefile
          (or (buffer-file-name buffer)
              (error "Buffer %s has no associated file" buffer)))
         (tempfile (make-temp-file "buffer-content-")))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (write-region (point-min) (point-max) tempfile nil 'silent)))
    (shell-command (concat "tkdiff " basefile " " tempfile))
    (delete-file tempfile)))

(defun my-dos2unix ()
  "Remove ^M's from file."
  (interactive)
  (goto-char (point-min))
  (while (search-forward (string ?\C-m) nil t)
    (replace-match ""))
  (goto-char (point-min)))

(defun my-ffap (&optional arg)
  "ffap, or ffap-other-window when preceded with C-u."
  (interactive "P")
  (call-interactively (if arg 'ffap-other-window 'ffap)))

(defun my-fill ()
  "Fill region if active, paragraph if not."
  (interactive)
  (if (region-active-p)
      (fill-region (region-beginning) (region-end))
    (fill-paragraph nil)))

(defun my-fill-sentence ()
  "Fill sentence separated by punctuation or blank lines."
  (interactive)
  (let (start end)
    (save-excursion
      (re-search-backward "\\(^\\s-*$\\|[.?!]\\)" nil t)
      (skip-syntax-forward "^w")
      (setq start (point-at-bol)))
    (save-excursion
      (re-search-forward "\\(^\\s-*$\\|[.?!]\\)" nil t)
      (setq end (point)))
    (save-restriction
      (narrow-to-region start end)
      (fill-paragraph nil))))

(defun my-fit-window (&optional arg)
  "Fit window to buffer or `frame-height' / 4.
Prefix with C-u to fit the `next-window'."
  (interactive "P")
  (let ((win (if arg (next-window) (get-buffer-window))))
    (fit-window-to-buffer win (/ (frame-height) 4))))

(defun my-hash-to-string (hash)
  "Make a hash into a printable string"
  (let (str)
    (maphash (lambda (key value)
               (setq str (concat str
                                 (prin1-to-string key t)
                                 " => "
                                 (prin1-to-string value t)
                                 "\n")))
             hash)
    str))

(defun my-highlight-regexp ()
  "highlight-regexp, but use region if active or symbol at point if not."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (progn
        (highlight-regexp (regexp-quote (buffer-substring (region-beginning) (region-end))) 'hi-pink)
        (deactivate-mark))
    (highlight-regexp (read-from-minibuffer "Regexp to highlight: "
                                            (regexp-quote (substring-no-properties (thing-at-point 'symbol)))
                                            nil nil nil 'hi-lock-replace-history)
                      'hi-pink)))

(defun my-inc-num (arg)
  "Increment decimal number, or with arg hex number"
  (interactive "P")
  (if arg
      (my-increment-number-hexadecimal)
    (my-increment-number-decimal)))

(defun my-insert-comment-line ()
  "Insert an 80-column comment line"
  (interactive)
  (let (char)
    (insert comment-start)
    (delete-horizontal-space)
    (setq char (char-before))
    (insert-char char (- 80 (- (point) (point-at-bol))))))

(defun my-kill-buffer (arg buffer)
  "Kill buffer and delete window if there is more than one."
  (interactive "P\nbKill buffer: ")
  (kill-buffer buffer)
  (when (and (not arg) (> (count-windows) 1))
    (delete-window)))

(defun my-kill-frame-or-emacs ()
  "Kill a frame or emacs"
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-emacs)))

(defun my-kill-results-buffer ()
  "Kill a compliation/grep/*whatever* buffer in a second window."
  (interactive)
  (when (> (count-windows) 1)
    (other-window 1)
    (unless (and (not (string= "*scratch*" (buffer-name)))
                 (posix-string-match "[ *]+.*[ *]+" (buffer-name)))
      (other-window 1))
    (when (and (posix-string-match "[ *]+.*[ *]+" (buffer-name))
               (not (string= "*scratch*" (buffer-name))))
      (kill-buffer nil)
      (delete-window))))

(defun my-kill-ring-pop ()
  "Pop the last kill off the ring."
  (interactive)
  (when kill-ring
    (setq kill-ring (cdr kill-ring)))
  (when kill-ring-yank-pointer
    (setq kill-ring-yank-pointer kill-ring))
  (message "Last kill popped off kill-ring."))

(defun my-kill-this-buffer (arg)
  "Kill buffer and delete window if there is more than one."
  (interactive "P")
  (kill-buffer nil)
  (when (and (not arg) (> (count-windows) 1))
    (delete-window)))

(defun my-make-script-executable ()
  "If file starts with a shebang, make it executable."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
                 (not (file-executable-p buffer-file-name)))
        (set-file-modes buffer-file-name
                        (logior (file-modes buffer-file-name) #o111))
        (message (concat "Made " buffer-file-name " executable"))))))

(defun my-minibuffer-backward ()
  "Move backward words or path elements in the minibuffer."
  (interactive)
  (unless (looking-back "[a-zA-Z0-9_.-]")
    (skip-chars-backward "^a-zA-Z0-9_.-"))
  (skip-chars-backward "a-zA-Z0-9_.-"))

(defun my-minibuffer-backward-kill ()
  "Kill backward words or path elements in the minibuffer."
  (interactive)
  (delete-region
   (save-excursion
     (my-minibuffer-backward)
     (point))
   (point)))

(defun my-minibuffer-forward ()
  "Move forward words or path elements in the minibuffer."
  (interactive)
  (when (looking-at "[a-zA-Z0-9_.-]")
    (skip-chars-forward "a-zA-Z0-9_.-"))
  (skip-chars-forward "^a-zA-Z0-9_.-"))

(defun my-minibuffer-forward-kill ()
  "Kill forward words or path elements in the minibuffer."
  (interactive)
  (delete-region
   (save-excursion
     (my-minibuffer-forward)
     (point))
   (point)))

(defun my-pop-tag-mark-kill-buffer ()
  "Pop tag mark and kill previous buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (pop-tag-mark)
    (unless (equal buf (current-buffer))
      (kill-buffer buf))))

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (let ((x-select-enable-clipboard t))
        (kill-new filename)
        (message filename)))))

(defun my-prettify ()
  "Remove trailing space, untabify, reindent."
  (interactive)
  (my-delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defvar my-recenter-count nil)
(defun my-recenter (&optional arg)
  "Recenter high/middle/low."
  (interactive "P")
  (if arg
      (progn
        (setq my-recenter-count 0)
        (recenter arg))
    (unless (equal last-command 'my-recenter)
      (setq my-recenter-count 0))
    (cond ((= my-recenter-count 0)
           (recenter))
          ((= my-recenter-count 1)
           (recenter (/ (window-text-height) 4)))
          (t
           (recenter (/ (* (window-text-height) 3) 4))))
    (setq my-recenter-count (1+ my-recenter-count))
    (when (> my-recenter-count 2)
      (setq my-recenter-count 0))))

(defun my-rotate-case ()
  "Rotate case to capitalized, uppercase, lowercase."
  (interactive)
  (let ((case-fold-search nil) beg end)
    (save-excursion
      (skip-syntax-forward "w_")
      (setq end (point))
      (skip-syntax-backward "w_")
      (skip-chars-forward "^a-zA-Z")
      (setq beg (point))
      (if (< (- end beg) 2)
          (if (looking-at "[A-Z]")
              (downcase-region beg end)
            (upcase-region beg end))
        (if (looking-at "[a-z]")
            (upcase-region beg (1+ beg))
          (forward-char)
          (if (re-search-forward "[a-z]" end t)
              (upcase-region beg end)
            (downcase-region beg end)))))))

(defun my-rotate-window-buffers()
  "Rotate the window buffers"
  (interactive)
  (let* ((windows (window-list))
         (buffers (mapcar #'window-buffer windows))
         (wpoints (mapcar #'window-point windows))
         (w (pop windows)))
    (setq windows (append windows `(,w)))
    (dolist (w windows)
      (let ((b (pop buffers))
            (p (pop wpoints)))
        (set-window-buffer w b)
        (set-window-point w p)))))

(defun my-set-title (title)
  "Set the frame title"
  (interactive "sTitle: ")
  (setq frame-title-format title))

(defun my-set-selective-display (&optional col)
  "Set selective display based on cursor column."
  (interactive "P")
  (set-selective-display (or col (1+ (current-column)))))

(defun my-shell-command-on-current-file (command &optional output-buffer error-buffer)
  "Run a shell command on the current file (or marked dired files).
In the shell command, the file(s) will be substituted wherever a '%' is."
  (interactive (list (read-from-minibuffer "Shell command: "
                                           nil nil nil 'shell-command-history)
                     current-prefix-arg
                     shell-command-default-error-buffer))
  (cond ((buffer-file-name)
         (setq command (replace-regexp-in-string "%" (buffer-file-name) command nil t)))
        ((and (equal major-mode 'dired-mode) (save-excursion (dired-move-to-filename)))
         (setq command (replace-regexp-in-string "%" (mapconcat 'identity (dired-get-marked-files) " ") command nil t))))
  (shell-command command output-buffer error-buffer))

(defun my-skip-lines-matching-regexp (regexp)
  "Skip lines matching a regexp."
  (interactive "sSkip lines matching regexp: ")
  (beginning-of-line)
  (while (and (not (eobp)) (looking-at regexp))
    (forward-line 1)))

(defun my-tip-of-the-day ()
  "Tip of the day"
  (interactive)
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip of the day is:\n========================\n\n"
               (describe-function command)
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))

(defun my-toggle-window-split ()
  "Toggle between horizontal/vertical split.
Only works if there are exactly two windows."
  (interactive)
  (when (= (count-windows) 2)
    (let* ((this-win-buffer (window-buffer))
           (next-win-buffer (window-buffer (next-window)))
           (this-win-edges (window-edges (selected-window)))
           (next-win-edges (window-edges (next-window)))
           (this-win-2nd (not (and (<= (car this-win-edges)
                                       (car next-win-edges))
                                   (<= (cadr this-win-edges)
                                       (cadr next-win-edges)))))
           (splitter
            (if (= (car this-win-edges)
                   (car (window-edges (next-window))))
                'split-window-horizontally
              'split-window-vertically)))
      (delete-other-windows)
      (let ((first-win (selected-window)))
        (funcall splitter)
        (when this-win-2nd (other-window 1))
        (set-window-buffer (selected-window) this-win-buffer)
        (set-window-buffer (next-window) next-win-buffer)
        (select-window first-win)
        (when this-win-2nd (other-window 1))))))

(defun my-unbold ()
  "Unbold all faces."
  (interactive)
  (mapc (lambda (face)
          (set-face-attribute face nil :weight 'normal :underline nil))
        (face-list)))

(defun my-unfill ()
  "Unfill region if active, paragraph if not."
  (interactive)
  (let ((fill-column (point-max)))
    (if (region-active-p)
        (fill-region (region-beginning) (region-end))
      (fill-paragraph 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice

(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (char-equal (char-syntax (char-before (point))) ?\))
    (let ((matching-text (blink-matching-open)))
      (when matching-text
        (message matching-text)))))

(defadvice narrow-to-region (after my-narrow-to-region activate)
  "After narrowing to region, deactivate region and go to top."
  (deactivate-mark)
  (goto-char (point-min)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(defun my-after-save-hook ()
  (my-make-script-executable))

(defun my-diff-mode-hook ()
  (define-key diff-mode-map "q" 'my-kill-this-buffer)
  (define-key diff-mode-map "n" 'diff-hunk-next)
  (define-key diff-mode-map "p" 'diff-hunk-prev))

(defun my-elog-mode-hook ()
  (my-whitespace-off-hook))

(defun my-emacs-lisp-mode-hook ()
  (setq comment-column 0))

(defun my-etags-select-hook ()
  (hl-line-mode 1))

(defun my-grep-mode-hook ()
  (define-key grep-mode-map "h" 'hl-line-mode))

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0)
  (local-set-key "\C-z" 'undo)
  (local-set-key [(control ?/)] 'dabbrev-expand)
  (local-set-key (kbd "M-h") 'my-minibuffer-backward)
  (local-set-key (kbd "M-j") 'my-minibuffer-backward-kill)
  (local-set-key (kbd "M-k") 'my-minibuffer-forward-kill)
  (local-set-key (kbd "M-l") 'my-minibuffer-forward)
  (local-set-key (kbd "M-$") (lambda ()
                               (interactive)
                               (let* ((enable-recursive-minibuffers t)
                                      (dir (my-ido-get-bookmark-dir)))
                                 (when dir
                                   (insert dir))))))

(defun my-sh-mode-hook ()
  (use-local-map nil))

(defun my-task-after-load-hook ()
  (when clearcase-setview-viewtag
    (dolist (buf (buffer-list))
      (when (buffer-file-name buf)
        (set-buffer buf)
        (clearcase-hook-find-file-hook)))))

(defun my-whitespace-off-hook ()
  (my-font-lock-show-whitespace -1))

(defun my-verilog-hook ()
  (define-key verilog-mode-map "`" nil)
  (define-key verilog-mode-map (kbd "RET") nil)
  (define-key verilog-mode-map ";" nil))

(add-hook 'Info-mode-hook 'my-whitespace-off-hook)
(add-hook 'after-save-hook 'my-after-save-hook)
(add-hook 'diff-mode-hook 'my-diff-mode-hook)
(add-hook 'dired-mode-hook 'my-whitespace-off-hook)
(add-hook 'elog-mode-hook 'my-elog-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'etags-select-mode-hook 'my-etags-select-hook)
(add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)
(add-hook 'grep-mode-hook 'my-grep-mode-hook)
(add-hook 'midnight-hook 'recentf-cleanup)
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'task-after-load-hook 'my-task-after-load-hook)
(add-hook 'verilog-mode-hook 'my-verilog-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; After loads

(eval-after-load "compile"
  '(progn
     (defun my-compilation-mode-hook ()
       (setq truncate-lines 'one-line-each)
       (goto-char (point-max)))
     (add-hook 'compilation-mode-hook 'my-compilation-mode-hook)))

(eval-after-load "e-mode"
  '(progn
     (require 'my-e-mode)
     (require 'e-mode-dbg)
     (defun my-e-mode-hook ()
       (setq ll-debug-print-filename nil)
       (set (make-local-variable 'compile-command) "cat $work/test/results/specman.elog"))
     (add-hook 'e-mode-hook 'my-e-mode-hook)))

(eval-after-load "elog-mode"
  '(progn
     (defun my-elog-mode-hook ()
       (setq truncate-lines 'one-line-each))
     (add-hook 'elog-mode-hook 'my-elog-mode-hook)))

(eval-after-load "file-template"
  '(progn
     (add-to-list 'file-template-mapping-alist '("\\.e$" . "template.e"))
     (add-to-list 'file-template-mapping-alist '("\\.s$" . "template.s"))
     (add-to-list 'file-template-mapping-alist '("\\.v$" . "template.v"))
     (add-to-list 'file-template-mapping-alist '("\\.csh$" . "template.csh"))))

(eval-after-load "grep"
  '(define-key grep-mode-map "q" 'my-kill-results-buffer))

(eval-after-load "make-mode"
  '(progn
     (defun my-makefile-mode-hook ()
       (modify-syntax-entry ?= ". 14" makefile-mode-syntax-table))
     (add-hook 'makefile-mode-hook 'my-makefile-mode-hook)))

(eval-after-load "speedbar"
  '(progn
     (speedbar-add-supported-extension ".e")
     (speedbar-add-supported-extension ".v")))

(eval-after-load "verilog-mode"
  '(require 'my-verilog))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(defmacro my-keys-define (key fn)
  (list 'define-key 'my-keys-minor-mode-map (list 'kbd key) fn))

(my-keys-define "<C-f4>" 'my-apply-macro-to-region-lines)
(my-keys-define "<C-return>" 'my-yasnippet-or-abbrev-expand)
(my-keys-define "<C-tab>" 'other-window)
(my-keys-define "<M-return>" 'makd-open-line-above)
(my-keys-define "<S-f6>" 'task-bmk-buf-prev)
(my-keys-define "<S-f7>" 'task-bmk-all-prev)
(my-keys-define "<delete>" 'delete-char)
(my-keys-define "<f2>" 'undefined)
(my-keys-define "<f5>" 'task-bmk-toggle)
(my-keys-define "<f6>" 'task-bmk-buf-next)
(my-keys-define "<f7>" 'task-bmk-all-next)
(my-keys-define "<tab>" 'makd-indent)
(my-keys-define "C-&" 'my-pop-back-ffap-kill-buffer)
(my-keys-define "C-*" 'my-pop-back-ffap)
(my-keys-define "C-," 'iflipb-previous-buffer)
(my-keys-define "C-." 'iflipb-next-buffer)
(my-keys-define "C-/" 'dabbrev-expand)
(my-keys-define "C-?" 'my-ffap)
(my-keys-define "C-M-;" 'comment-set-column)
(my-keys-define "C-M-w" 'clipboard-kill-ring-save)
(my-keys-define "C-M-y" 'clipboard-yank)
(my-keys-define "C-S-SPC" 'my-rect-toggle)
(my-keys-define "C-S-o" 'my-create-or-other-frame)
(my-keys-define "C-S-s" 'my-isearch-word-at-point)
(my-keys-define "C-^" 'my-pop-back-imenu)
(my-keys-define "C-`" 'next-error)
(my-keys-define "C-c #" 'my-convert-to-base)
(my-keys-define "C-c $" 'my-delete-trailing-whitespace)
(my-keys-define "C-c +" 'my-inc-num)
(my-keys-define "C-c ." 'my-kill-results-buffer)
(my-keys-define "C-c ;" 'my-insert-comment-line)
(my-keys-define "C-c A" 'align-regexp)
(my-keys-define "C-c C" 'my-comment-region)
(my-keys-define "C-c C-c" 'my-comment-region-toggle)
(my-keys-define "C-c C-f" 'my-ido-recentf-file)
(my-keys-define "C-c C-g" 'grep-buffers)
(my-keys-define "C-c C-k" 'my-kill-ring-pop)
(my-keys-define "C-c C-l" 'll-debug-renumber)
(my-keys-define "C-c G" 'rgrep)
(my-keys-define "C-c H" 'unhighlight-regexp)
(my-keys-define "C-c L" 'll-debug-revert)
(my-keys-define "C-c N" (lambda () (interactive) (find-file "~/Doc/scfrazer-notes.texi")))
(my-keys-define "C-c R" 'revbufs)
(my-keys-define "C-c SPC" 'my-delete-whitespace-after-cursor)
(my-keys-define "C-c a" 'my-align)
(my-keys-define "C-c c" 'my-comment-region-after-copy)
(my-keys-define "C-c e" 'my-ediff-buffer-with-file)
(my-keys-define "C-c f" 'font-lock-fontify-buffer)
(my-keys-define "C-c g" 'lgrep)
(my-keys-define "C-c i" (lambda () (interactive) (find-file "~/.emacs.d/ideas.org")))
(my-keys-define "C-c j" 'makd-join-line-with-next)
(my-keys-define "C-c k" 'browse-kill-ring)
(my-keys-define "C-c l" (lambda () (interactive) (ll-debug-insert 1)))
(my-keys-define "C-c m" 'compile)
(my-keys-define "C-c n" (lambda () (interactive (info "scfrazer-notes"))))
(my-keys-define "C-c o" 'my-occur)
(my-keys-define "C-c p" 'ps-print-buffer-with-faces)
(my-keys-define "C-c r" 'revert-buffer)
(my-keys-define "C-c s" 'my-rotate-window-buffers)
(my-keys-define "C-c t" (lambda () (interactive) (find-file "~/org/work.org")))
(my-keys-define "C-c v" 'toggle-truncate-lines)
(my-keys-define "C-c w" 'my-font-lock-show-whitespace)
(my-keys-define "C-i" 'my-yank-target-map)
(my-keys-define "C-k" 'makd-kill-line)
(my-keys-define "C-o" 'my-bs-toggle)
(my-keys-define "C-w" 'makd-kill-unit)
(my-keys-define "C-x $" 'my-set-selective-display)
(my-keys-define "C-x -" 'my-fit-window)
(my-keys-define "C-x 2" 'my-bs-split-window-vertically)
(my-keys-define "C-x 3" 'my-bs-split-window-horizontally)
(my-keys-define "C-x C-c" 'my-kill-frame-or-emacs)
(my-keys-define "C-x K" 'kill-buffer)
(my-keys-define "C-x SPC" 'fixup-whitespace)
(my-keys-define "C-x _" (lambda () (interactive) (my-fit-window t)))
(my-keys-define "C-x k" 'my-kill-buffer)
(my-keys-define "C-x m" ahg-global-map) ;; Yes, this one is not quoted
(my-keys-define "C-x r a" 'append-to-register)
(my-keys-define "C-x t" 'task-map)
(my-keys-define "C-x w" 'my-clone-file)
(my-keys-define "C-x |" 'my-toggle-window-split)
(my-keys-define "C-y" 'makd-yank)
(my-keys-define "C-z" 'undo)
(my-keys-define "C-~" 'previous-error)
(my-keys-define "M-!" 'my-shell-command-on-current-file)
(my-keys-define "M-%" 'makd-query-replace)
(my-keys-define "M-&" 'my-pop-tag-mark-kill-buffer)
(my-keys-define "M-)" 'delete-pair)
(my-keys-define "M-*" 'pop-tag-mark)
(my-keys-define "M-." 'etags-select-find-tag)
(my-keys-define "M-/" 'mdabbrev-expand)
(my-keys-define "M-;" 'comment-indent)
(my-keys-define "M-?" 'etags-select-find-tag-at-point)
(my-keys-define "M-G" 'my-ido-imenu-goto-symbol)
(my-keys-define "M-Q" 'my-unfill)
(my-keys-define "M-S" (lambda () (interactive) (makd-yank t)))
(my-keys-define "M-SPC" (lambda () (interactive) (push-mark)))
(my-keys-define "M-[" 'insert-pair)
(my-keys-define "M-\"" 'insert-pair)
(my-keys-define "M-^" 'etags-stack-show)
(my-keys-define "M-b" 'task-bmk-show-all)
(my-keys-define "M-c" 'my-rotate-case)
(my-keys-define "M-d" 'my-dired-pop-to-or-create)
(my-keys-define "M-e" 'makd-select-word-at-point)
(my-keys-define "M-g" 'goto-line)
(my-keys-define "M-i" 'ido-switch-buffer)
(my-keys-define "M-o" 'bs-show)
(my-keys-define "M-q" 'my-fill)
(my-keys-define "M-w" 'makd-copy-unit)
(my-keys-define "M-z" 'redo)

;; These have to be in this order

(my-keys-define "C-c h" 'help-command)
(my-keys-define "C-c h a" 'apropos)

;; Movement

(my-keys-define "M-u" 'my-recenter)

(my-keys-define "C-M-h" 'backward-sexp)
(my-keys-define "C-M-l" 'forward-sexp)
(my-keys-define "C-M-n" 'up-list)
(my-keys-define "C-M-p" 'backward-up-list)

(my-keys-define "M-N" 'makd-page-down)
(my-keys-define "M-P" 'makd-page-up)

(my-keys-define "M-h" 'makd-backward-word)
(my-keys-define "M-l" 'makd-forward-word)
(my-keys-define "M-n" 'makd-forward-paragraph)
(my-keys-define "M-p" 'makd-backward-paragraph)

(my-keys-define "C->" 'makd-forward-block)
(my-keys-define "C-<" 'makd-backward-block)

(my-keys-define "M-H" 'makd-backward-word-end)
(my-keys-define "M-L" 'makd-forward-word-end)

(my-keys-define "C-S-h" 'makd-backward-word-section)
(my-keys-define "C-S-l" 'makd-forward-word-section)

(my-keys-define "C-h" 'backward-char)
(my-keys-define "C-l" 'forward-char)
(my-keys-define "C-n" 'next-line)
(my-keys-define "C-p" 'previous-line)

(my-keys-define "C-S-n" 'makd-scroll-down)
(my-keys-define "C-S-p" 'makd-scroll-up)

;; Kill

(my-keys-define "M-j" 'makd-backward-kill)
(my-keys-define "M-k" 'makd-forward-kill)

(my-keys-define "M-J" 'makd-backward-kill-section)
(my-keys-define "M-K" 'makd-forward-kill-section)

;; Because of file vs. dired mapping

(global-set-key (kbd "C-v") clearcase-prefix-map)

;; Keybinding minor mode

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(global-set-key (kbd "<C-down-mouse-1>") nil)
(global-set-key (kbd "<C-mouse-1>") (lambda (event)
                                      (interactive "e")
                                      (mouse-set-point event)
                                      (makd-select-word-at-point)))

(global-set-key (kbd "<S-down-mouse-3>") 'imenu)
(global-set-key (kbd "<C-down-mouse-3>") 'mouse-popup-menubar)

(global-set-key (kbd "<mouse-4>") (lambda () "Scroll up." (interactive) (makd-scroll-up 5)))
(global-set-key (kbd "<mouse-5>") (lambda () "Scroll down." (interactive) (makd-scroll-down 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cisco setup

(setq etags-table-alist
      (list
       '("/vob/asicproc/verification/evc_lib/.*\\.e$" "/nfs/astro_scratch/tags/dv/TAGS")
       '("/vob/astro/.*\\.e$" "/nfs/astro_scratch/tags/dv/TAGS")
       '("/vob/astro/.*\\.vh?$" "/nfs/astro_scratch/tags/rtl/TAGS")
       '("/vob/astro/.*\\.s$" "/nfs/astro_scratch/tags/rtl/TAGS")
       '("/vob/wolverine/.*\\.e$" "/nfs/wolverine_scratch/tags/dv/TAGS")
       '("/vob/wolverine/.*\\.v$" "/nfs/wolverine_scratch/tags/rtl/TAGS")
       '("/vob/hkp/.*\\.e$" "/nfs/hkp_scratch/tags/dv/TAGS")
       '("/vob/hkp/.*\\.v$" "/nfs/hkp_scratch/tags/rtl/TAGS")
       ))

(unless (getenv "SPECMAN_PATH")
  (setenv "SPECMAN_PATH"
          ".:/vob/astro/verification/testbench:/vob/astro/verification/evc_lib:/vob/asicproc/verification/evc_lib"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finish up

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS-specific setup

(let ((extra-config (concat "~/.emacs.d/" (symbol-name system-type) ".el")))
  (when (file-exists-p extra-config)
    (load-file extra-config)))

;; Time emacs load time

(message ".emacs loaded in %.3f s"
         (let* ((now (current-time))
                (start-time (+ (first *emacs-load-start*) (second *emacs-load-start*)
                               (/ (float (third *emacs-load-start*)) 1e6)))
                (end-time (+ (first now) (second now) (/ (float (third now)) 1e6))))
           (- end-time start-time)))

(put 'erase-buffer 'disabled nil)
