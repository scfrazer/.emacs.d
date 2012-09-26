;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el

(tool-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode -1)

;; Time load time

(defvar *emacs-load-start* (current-time))
(defun my-get-load-time ()
  "Get current load time."
  (let* ((now (current-time))
         (start-time (+ (car *emacs-load-start*) (cadr *emacs-load-start*)
                        (/ (float (cadr (cdr *emacs-load-start*))) 1e6)))
         (end-time (+ (car now) (cadr now) (/ (float (cadr (cdr now))) 1e6))))
    (- end-time start-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/org")

(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))

;; Need these first to avoid font-lock/dired issues

(require 'my-font-lock)
(require 'my-dired)

(require 'csh-mode)
(require 'etags)
(require 'etags-select)
(require 'etags-stack)
(require 'etags-table)
(require 'hdl-dbg-vcs)
(require 'hl-line)
(require 'iflipb)
(require 'jump-to-prev-pos)
(require 'mdabbrev)
(require 'midnight)
(require 'mode-fn)
(require 'narrow-nested)
(require 'num3)
(require 'quick-edit)
(require 'rect)
(require 'redo+)
(require 'revbufs)
(require 'sb-imenu)
(require 'scf-mode)
(require 'task)
(require 'uniquify)

(require 'my-bs)
(require 'my-bookmark)
(require 'my-calculator)
(require 'my-cc-mode)
(require 'my-clearcase)
(require 'my-comment)
(require 'my-doxymacs)
(require 'my-ediff)
(require 'my-edit)
(require 'my-expand)
(require 'my-ffap)
(require 'my-grep-ed)
(require 'my-ido)
(require 'my-grep)
(require 'my-imenu)
(require 'my-increment-number)
(require 'my-isearch)
(require 'my-magit)
(require 'my-mode-line)
(require 'my-occur)
(require 'my-org)
(require 'my-pair)
(require 'my-perl)
(require 'my-pop-back)
(require 'my-python)
(require 'my-recentf)
(require 'my-reformat)
(require 'my-register-list)
(require 'my-sv-mode)
(require 'my-tags-search)
(require 'my-theme)
(require 'my-vc)

(message "~/.emacs.d/init.el 0 load time = %.3f s" (my-get-load-time))

(autoload 'align "align" nil t)
(autoload 'align-regexp "align" nil t)
(autoload 'antlr3-mode "antlr3-mode" "ANTLR code editing mode" t)
(autoload 'browse-kill-ring "browse-kill-ring" nil t)
(autoload 'compile "compile" nil t)
(autoload 'e-mode "e-mode" "Specman 'e' code editing mode" t)
(autoload 'elog-mode "elog-mode" nil t)
(autoload 'expand-abbrev "abbrev" nil t)
(autoload 'file-template-auto-insert "file-template" nil t)
(autoload 'file-template-find-file-not-found-hook "file-template" nil t)
(autoload 'file-template-insert "file-template" nil t)
(autoload 'find-files-glob "find-files" nil t)
(autoload 'grep-buffers "grep-buffers" nil t)
(autoload 'htmlize-region "htmlize" nil t)
(autoload 'js2-mode "js2" nil t)
(autoload 'll-debug-insert "ll-debug" nil t)
(autoload 'll-debug-revert "ll-debug" nil t)
(autoload 'll-debug-renumber "ll-debug" nil t)
(autoload 'lua-mode "lua-mode" nil t)
(autoload 'makefile-mode "make-mode" nil t)
(autoload 'my-confluence-highlight "my-confluence" nil t)
(autoload 'my-confluence-html "my-confluence" nil t)
(autoload 'rdl-mode "rdl-mode" nil t)
(autoload 'rst-mode "rst" "reStructured Text Mode" t)
(autoload 'sse-log-mode "sse-log-mode" nil t)
(autoload 'specterx-mode "specterx-mode" "SpecterX mode" t)
(autoload 'sv-mode "sv-mode" "SystemVerilog mode" t)
(autoload 'uvm-log-mode "uvm-log-mode" nil t)
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t)
(autoload 'vsif-mode "vsif-mode" "VSIF mode" t)

(show-paren-mode t)
(delete-selection-mode t)
(blink-cursor-mode 1)
(set-scroll-bar-mode nil)
(winner-mode 1)
(when (eq emacs-major-version 24)
  (electric-pair-mode 1))
(global-num3-mode 1)

(setq-default backup-inhibited t
              blink-matching-paren-distance nil
              browse-kill-ring-display-duplicates nil
              browse-kill-ring-highlight-current-entry nil
              browse-kill-ring-maximum-display-length 400
              browse-kill-ring-no-duplicates t
              browse-kill-ring-quit-action (quote kill-and-delete-window)
              browse-kill-ring-separator "----------8<----------8<----------8<----------8<----------8<----------"
              browse-kill-ring-separator-face (quote font-lock-keyword-face)
              browse-kill-ring-use-fontification t
              buffer-face-mode-face 'my-buffer-face-mode-face
              case-fold-search t
              column-number-mode t
              comment-column 0
              comment-fill-column 120
              compare-ignore-whitespace t
;;               compilation-error-regexp-alist nil
              completions-format 'vertical
              confirm-kill-emacs 'y-or-n-p
              cursor-in-non-selected-windows nil
              cursor-type 'box
              dabbrev-case-fold-search nil
              dired-auto-revert-buffer t
              echo-keystrokes 0.1
              etags-select-use-short-name-completion t
              etags-table-search-up-depth 10
              even-window-heights nil
              file-template-insert-automatically 'ask
              file-template-paths (list "~/.emacs.d/templates/")
              fill-column 78
              flyspell-mode-map nil
              font-lock-verbose nil
              highlight-changes-active-string " Chg+"
              highlight-changes-passive-string " Chg-"
              highlight-changes-global-modes nil
              htmlize-output-type 'font
              iflipb-ignore-buffers 'my-bs-ignore-buffer
              indicate-buffer-boundaries t
              indent-tabs-mode nil
              inhibit-startup-message t
              isearch-lazy-highlight-initial-delay 0
              js2-basic-offset 4
              kill-do-not-save-duplicates t
              kill-whole-line t
              large-file-warning-threshold nil
              line-move-visual t
              line-number-mode t
              ll-debug-output-prefix (concat "DEBUG-" (getenv "USER") "-")
              ll-debug-print-filename nil
              lpr-command "lpr"
              lpr-lp-system t
              lpr-switches ""
              magit-repo-dirs (list "~/.emacs.d" "~/Projects")
              magit-repo-dirs-depth 2
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
              redisplay-dont-pause t
              rst-mode-lazy nil
              save-abbrevs nil
              save-interprogram-paste-before-kill t
              scroll-conservatively 10000
              scroll-preserve-screen-position t
              shift-select-mode nil
              show-paren-delay 0
              speedbar-initial-expansion-list-name "sb-imenu"
              speedbar-use-images nil
              split-width-threshold nil
              tags-revert-without-query t
              tempo-interactive t
              truncate-partial-width-windows nil
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

(setq-default select-active-regions t ;; nil
              mouse-drag-copy-region t
              x-select-enable-primary t
              x-select-enable-clipboard nil)

(setq frame-title-format (concat "%F" (if (and clearcase-servers-online clearcase-setview-viewtag)
                                          (concat " - " clearcase-setview-viewtag)
                                        "")))

(add-to-list 'auto-mode-alist '("Makefile.*$" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.aop$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.csh$" . csh-mode))
(add-to-list 'auto-mode-alist '("\\.cshrc$" . csh-mode))
(add-to-list 'auto-mode-alist '("\\.e$" . e-mode))
(add-to-list 'auto-mode-alist '("\\.elog$" . elog-mode))
(add-to-list 'auto-mode-alist '("\\.g$" . antlr3-mode))
(add-to-list 'auto-mode-alist '("\\.g3.*$" . antlr3-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.rdlh?$" . rdl-mode))
(add-to-list 'auto-mode-alist '("\\.s$" . specterx-mode))
(add-to-list 'auto-mode-alist '("\\.sv$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.sva$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.svh$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.v$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.vh$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.vsif$" . vsif-mode))
(add-to-list 'auto-mode-alist '("run.log$" . uvm-log-mode))
(add-to-list 'auto-mode-alist '("very.*\\.log$" . elog-mode))

;; Don't use sh-mode for csh files

(dolist (elt interpreter-mode-alist)
  (when (member (car elt) (list "csh" "tcsh"))
    (setcdr elt 'csh-mode)))

;; isearch scroll

(setq isearch-allow-scroll t)
(put 'my-recenter 'isearch-scroll t)

;; Snippets

(message "~/.emacs.d/init.el 1 load time = %.3f s" (my-get-load-time))

(yas/load-directory "~/.emacs.d/lisp/yasnippet/snippets")

(message "~/.emacs.d/init.el 2 load time = %.3f s" (my-get-load-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun my-align ()
  "Align declarations, etc."
  (interactive)
  (save-excursion
    (if (and transient-mark-mode mark-active)
        (align (region-beginning) (region-end))
      (align (point-at-bol)
             (save-excursion
               (forward-paragraph)
               (point-at-eol))))))

(defun my-apply-macro-to-region-lines (top bottom)
  "Apply macro to region lines and deactivate mark"
  (interactive "r")
  (apply-macro-to-region-lines top bottom)
  (deactivate-mark))

(defun my-ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
  (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (save-excursion
    (let ((i -1))
      (insert "ASCII characters 0 thru 127.\n\n")
      (insert "Hex   Dec  Char| Hex   Dec  Char| Hex   Dec  Char| Hex   Dec  Char\n")
      (while (< i 31)
        (insert (format "0x%-2X %4d %4s | 0x%-2X %4d %4s | 0x%-2X %4d %4s | 0x%-2X %4d %4s\n"
                        (setq i (+ 1  i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)))
        (setq i (- i 96))))))

(defun my-backward-paragraph-rect ()
  "Move backward to the same column in the first line before a blank line."
  (interactive)
  (let ((col (current-column))
        (again t))
    (while again
      (forward-line -1)
      (setq again (not (bobp)))
      (unless (and (not (looking-at "^\\s-*$"))
                   (= (move-to-column col) col))
        (forward-line 1)
        (move-to-column col)
        (setq again nil)))))

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

(defun my-count-lines ()
  "Count lines in region or to next blank line."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (message "%d lines in region"
               (count-lines (region-beginning) (region-end)))
    (message "%d lines to end of paragraph"
             (count-lines (point-at-bol)
                          (save-excursion (forward-paragraph) (point-at-eol))))))

(defun my-create-scratch ()
  "Recreate the *scratch* buffer."
  (interactive)
  (get-buffer-create "*scratch*")
  (switch-to-buffer "*scratch*")
  (erase-buffer)
  (insert ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

")
  (emacs-lisp-mode)
  (set-buffer-modified-p nil))

(defun my-delete-whitespace-after-cursor ()
  "Delete spaces/tabs after cursor."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t\n") (point))))

(defun my-delete-trailing-whitespace ()
  "Remove trailing spaces and excess blank lines in the buffer."
  (interactive "*")
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (delete-trailing-whitespace)
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil t)
          (backward-char)
          (delete-blank-lines))
        (goto-char (point-max))
        (delete-blank-lines)
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

(defun my-forward-paragraph-rect ()
  "Move forward to the same column in the last line before a blank line."
  (interactive)
  (let ((col (current-column))
        (again t))
    (while again
      (forward-line 1)
      (setq again (not (eobp)))
      (unless (and (not (looking-at "^\\s-*$"))
                   (= (move-to-column col) col))
        (forward-line -1)
        (move-to-column col)
        (setq again nil)))))

(defun my-goto-line-column (&optional arg)
  "Goto line:column.
With a numeric prefix, goto that window line."
  (interactive "P")
  (if arg
      (move-to-window-line arg)
    (let ((line-col (read-from-minibuffer "Goto (line:column): ")) line col)
      (when (string-match "\\([0-9]+\\)?\\(:\\([0-9]+\\)\\)?" line-col)
        (setq line (match-string 1 line-col)
              col (match-string 3 line-col))
        (when line
          (goto-line (string-to-number line)))
        (when col
          (move-to-column (string-to-number col)))))))

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

(defun my-indent ()
  "Indent entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun my-insert-comment-line ()
  "Insert an 80-column comment line"
  (interactive)
  (let (char)
    (insert comment-start)
    (delete-horizontal-space)
    (setq char (char-before))
    (insert-char char (- 80 (- (point) (point-at-bol))))))

(defun my-ll-debug-insert (&optional arg)
  "Swap default style of ll-debug-insert."
  (interactive "P")
  (ll-debug-insert (if arg nil 1)))

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

(defun my-kill-this-buffer (arg)
  "Kill buffer and delete window if there is more than one."
  (interactive "P")
  (kill-buffer nil)
  (when (and (not arg) (> (count-windows) 1))
    (delete-window)))

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

(defun my-minibuffer-insert-word-after-point ()
  "Insert the word after point into the minibuffer."
  (interactive)
  (let (word beg)
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (save-excursion
        (setq beg (point))
        (skip-syntax-forward "w_")
        (setq word (buffer-substring-no-properties beg (point)))))
    (when word
      (insert word))))

(defun my-narrow-nested-dwim (&optional arg)
  "narrow-nested-dwim, but do something special if there is a prefix arg."
  (interactive "P")
  (if arg
      (cond ((equal major-mode 'sv-mode)
             (sv-mode-narrow-to-scope))
            (t
             (narrow-nested-dwim)))
    (narrow-nested-dwim)))

(defun my-open-line-above ()
  "Open a line above the current one."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1))

(defun my-open-line-below ()
  "Open a line below the current one."
  (interactive)
  (end-of-line)
  (newline))

(defun my-other-frame ()
  "Switch to other frame."
  (interactive)
  (when (> (length (frame-list)) 1)
    (let ((frame (get-other-frame)))
      (if (eq (frame-visible-p frame) 'icon)
          (make-frame-visible frame)
        (other-frame 1)))))

(defun my-pop-tag-mark-kill-buffer ()
  "Pop tag mark and kill previous buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (pop-tag-mark)
    (unless (equal buf (current-buffer))
      (kill-buffer buf))))

(defun my-put-file-name-on-clipboard (&optional arg)
  "Put the current file name on the clipboard"
  (interactive "P")
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (when arg
        (setq filename (file-name-nondirectory filename)))
      (let ((x-select-enable-clipboard t))
        (kill-new filename)
        (message filename)))))

(defun my-prettify ()
  "Remove trailing space, untabify, reindent."
  (interactive)
  (my-delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defun my-query-replace (&optional arg)
  "query-replace ... take from-string from region if it is active.
With prefix arg, call the standard query-replace (good for repeating
previous replacement)."
  (interactive "*P")
  (if (or arg (not (region-active-p)))
      (let ((current-prefix-arg nil))
        (call-interactively 'query-replace))
    (let (from to)
      (setq from (buffer-substring (region-beginning) (region-end))
            to (read-from-minibuffer
                (format "Query replace %s with: " from) nil nil nil
                'query-replace-history))
      (goto-char (region-beginning))
      (setq mark-active nil)
      (query-replace from to)
      (setq query-replace-defaults (cons from to)))))

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
           (let ((pos (window-start)))
             (recenter)
             (when (= pos (window-start))
               (recenter (/ (window-text-height) 5))
               (setq my-recenter-count 1))))
          ((= my-recenter-count 1)
           (recenter (/ (window-text-height) 5)))
          (t
           (recenter (/ (* (window-text-height) 4) 5))))
    (setq my-recenter-count (1+ my-recenter-count))
    (when (> my-recenter-count 2)
      (setq my-recenter-count 0))))

(defun my-rectangle-number-lines (start end start-at &optional format)
  "Like `rectangle-number-lines' but with better defaults.

START-AT, if non-nil, should be a number from which to begin
counting.  FORMAT, if non-nil, should be a format string to pass
to `format' along with the line count.  When called interactively
with a prefix argument, prompt for START-AT and FORMAT."
  (interactive
   (if current-prefix-arg
       (let* ((start (region-beginning))
              (end   (region-end))
              (start-at (read-number "Number to count from: " 0)))
         (list start end start-at
               (read-string "Format string: " "%d")))
     (list (region-beginning) (region-end) 0 "%d")))
  (delete-extract-rectangle (region-beginning) (region-end))
  (setq end (point))
  (when (< end start)
    (let ((tmp start))
      (setq start end
            end tmp)))
  (let ((rectangle-number-line-counter start-at))
    (apply-on-rectangle 'rectangle-number-line-callback
                        start end format)))

(defun my-regexp-backward (regexp)
  "Skip lines backward containing a regexp."
  (interactive "sSkip lines backward containing regexp: ")
  (beginning-of-line)
  (forward-line -1)
  (while (and (not (bobp)) (re-search-forward regexp (line-end-position) t))
    (beginning-of-line)
    (forward-line -1)))

(defun my-regexp-forward (regexp)
  "Skip lines containing a regexp."
  (interactive "sSkip lines containing regexp: ")
  (beginning-of-line)
  (while (and (not (eobp)) (re-search-forward regexp (line-end-position) t))
    (beginning-of-line)
    (forward-line 1)))

(defvar my-rotate-case-direction nil
  "nil => capitalize, uppercase, lowercase,
 t => lowercase, uppercase, capitalize.")

(defun my-rotate-case (&optional beg end)
  "Rotate case to capitalized, uppercase, lowercase."
  (interactive)
  (let ((case-fold-search nil))
    (save-excursion
      (if (region-active-p)
          (if (< (region-beginning) (region-end))
              (setq beg (region-beginning)
                    end (region-end))
            (setq end (region-beginning)
                  beg (region-end)))
        (skip-syntax-forward "w_")
        (setq end (point))
        (skip-syntax-backward "w_")
        (skip-chars-forward "^a-zA-Z")
        (setq beg (point)))
      (goto-char beg)
      (if (< (- end beg) 2)
          (if (looking-at "[A-Z]")
              (downcase-region beg end)
            (upcase-region beg end))
        (let ((current-state
               (if (looking-at "[a-z]") 'lowercase
                 (forward-char)
                 (if (re-search-forward "[a-z]" end t) 'capitalized
                   'uppercase)))
              next-state)
          (setq next-state
                (if (equal last-command 'my-rotate-case)
                    (if my-rotate-case-direction
                        (cond ((eq current-state 'lowercase) 'uppercase)
                              ((eq current-state 'uppercase) 'capitalized)
                              (t 'lowercase))
                      (cond ((eq current-state 'capitalized) 'uppercase)
                            ((eq current-state 'uppercase) 'lowercase)
                            (t 'capitalized)))
                  (cond ((eq current-state 'lowercase)
                         (setq my-rotate-case-direction nil)
                         'capitalized)
                        ((eq current-state 'capitalized)
                         (setq my-rotate-case-direction t)
                         'lowercase)
                        (t
                         (setq my-rotate-case-direction nil)
                         'lowercase))))
          (cond ((eq next-state 'lowercase) (downcase-region beg end))
                ((eq next-state 'uppercase) (upcase-region beg end))
                (t (upcase-region beg (1+ beg))
                   (downcase-region (1+ beg) end))))))))

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

(defun my-sort-fields (field)
  "Sort region or following paragraph."
  (interactive "p")
  (save-excursion
    (if (and transient-mark-mode mark-active)
        (sort-fields field (region-beginning) (region-end))
      (sort-fields field (point-at-bol)
                   (save-excursion
                     (forward-paragraph)
                     (point-at-eol))))))

(defun my-sort-lines ()
  "Sort region or following paragraph."
  (interactive)
  (save-excursion
    (if (and transient-mark-mode mark-active)
        (sort-lines nil (region-beginning) (region-end))
      (sort-lines nil (point-at-bol)
                  (save-excursion
                    (forward-paragraph)
                    (point-at-eol))))))

(defun my-sort-lines ()
  "Sort region or following paragraph."
  (interactive)
  (save-excursion
    (if (and transient-mark-mode mark-active)
        (sort-lines nil (region-beginning) (region-end))
      (sort-lines nil (point-at-bol)
                  (save-excursion
                    (forward-paragraph)
                    (point-at-eol))))))

(defun my-tidy-lines ()
  "Tidy up lines."
  (interactive)
  (let* ((start (if (region-active-p) (region-beginning) (point-at-bol)))
         (end  (if (region-active-p) (region-end) (point-at-eol)))
         (num-lines (count-lines start end)))
    (save-excursion
      (goto-char start)
      (dotimes (idx num-lines)
        (back-to-indentation)
        (while (re-search-forward "[ \t]\\{2,\\}" (point-at-eol) t)
          (unless (my-inside-string-or-comment-p (match-beginning 0))
            (replace-match " ")))
        (dolist (regexp (list " \\([[(,;]\\)" "\\([[(]\\) " " \\([])]\\)"))
          (back-to-indentation)
          (while (re-search-forward regexp (point-at-eol) t)
            (unless (my-inside-string-or-comment-p (match-beginning 0))
              (replace-match "\\1"))))
        (back-to-indentation)
        (while (re-search-forward ",\\([^ \n]\\)" (point-at-eol) t)
          (unless (my-inside-string-or-comment-p (match-beginning 0))
            (replace-match ", \\1")))
        (forward-line 1)))))

(defun my-inside-string-or-comment-p (pos)
  "Is POS inside a string or comment?"
  (save-excursion
    (let ((ppss (syntax-ppss pos)))
      (or (nth 4 ppss) (nth 3 ppss)))))

(defun my-toggle-buffer-modified ()
  "Toggle buffer modified/unmodified."
  (interactive)
  (set-buffer-modified-p (not (buffer-modified-p))))

(defun my-toggle-quotes ()
  "Toggle between single/double quotes."
  (interactive)
  (let ((pos (point))
        (char (if (= (char-after) ?') ?\" ?')))
    (forward-sexp)
    (delete-char -1)
    (insert char)
    (goto-char pos)
    (delete-char 1)
    (insert char)
    (backward-char)))

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

(defun my-untabity ()
  "Untabify entire buffer."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(defun unpop-to-mark-command ()
  "Unpop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (interactive)
  (let ((num-times (if (equal last-command 'pop-to-mark-command) 2
                     (if (equal last-command 'unpop-to-mark-command) 1
                       (error "Previous command was not a (un)pop-to-mark-command")))))
    (dotimes (x num-times)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (+ 0 (car (last mark-ring))) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (mark t)))
      (deactivate-mark))))

(defvar my-push-marker (make-marker))
(defun my-push-mark-and-marker (&optional arg)
  "Push mark and create/move a marker.  With ARG, pop to mark/marker."
  (interactive "P")
  (if arg
      (when (and (marker-buffer my-push-marker)
                 (marker-position my-push-marker))
        (switch-to-buffer (marker-buffer my-push-marker))
        (goto-char (marker-position my-push-marker)))
    (push-mark)
    (setq my-push-marker (point-marker))
    (set-marker-insertion-type my-push-marker t)
    (message "Mark set")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice

(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (char-equal (char-syntax (char-before (point))) ?\))
    (let ((matching-text (blink-matching-open)))
      (when matching-text
        (message matching-text)))))

(defadvice kill-buffer (around my-kill-buffer-advice activate)
  "Don't kill the *scratch* buffer."
  (if (equal (ad-get-arg 0) "*scratch*")
      (bury-buffer)
    ad-do-it))

;; (defadvice quit-window (before advise-quit-window activate)
;;   (when (called-interactively-p 'any)
;;     (ad-set-arg 0 (not (ad-get-arg 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(defun my-after-save-hook ()
  (executable-make-buffer-file-executable-if-script-p))

(defun my-diff-mode-hook ()
  (define-key diff-mode-map "q" 'my-kill-this-buffer)
  (define-key diff-mode-map "n" 'diff-hunk-next)
  (define-key diff-mode-map "p" 'diff-hunk-prev))

(defun my-doxymacs-font-lock-hook ()
  (when (member major-mode (list 'c-mode 'c++-mode 'sv-mode))
    (doxymacs-font-lock)))

(defun my-emacs-lisp-mode-hook ()
  (setq comment-column 0))

(defun my-etags-select-hook ()
  (hl-line-mode 1))

(defun my-grep-mode-hook ()
  (define-key grep-mode-map "s" 'scf-mode))

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0)
  (local-set-key (kbd "C-/") 'dabbrev-expand)
  (local-set-key (kbd "C-w") 'my-minibuffer-insert-word-after-point)
  (local-set-key (kbd "C-z") 'undo)
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
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'etags-select-mode-hook 'my-etags-select-hook)
(add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(add-hook 'grep-mode-hook 'my-grep-mode-hook)
(add-hook 'midnight-hook 'recentf-cleanup)
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'sv-mode-hook 'doxymacs-mode)
(add-hook 'task-after-load-hook 'my-task-after-load-hook)
(add-hook 'uvm-log-mode-hook 'my-whitespace-off-hook)
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
       (set (make-local-variable 'compile-command) "cat $work/test/results/specman.elog"))
     (add-hook 'e-mode-hook 'my-e-mode-hook)))

(eval-after-load "elog-mode"
  '(progn
     (defun my-elog-mode-hook ()
       (my-whitespace-off-hook)
       (setq truncate-lines 'one-line-each))
     (add-hook 'elog-mode-hook 'my-elog-mode-hook)))

(eval-after-load "file-template"
  '(progn
     (add-to-list 'file-template-mapping-alist '("\\.e$" . "template.e"))
     (add-to-list 'file-template-mapping-alist '("\\.s$" . "template.s"))
     (add-to-list 'file-template-mapping-alist '("\\.v$" . "template.v"))
     (add-to-list 'file-template-mapping-alist '("\\.sv$" . "template.sv"))
     (add-to-list 'file-template-mapping-alist '("\\.svh$" . "template.svh"))
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
     (speedbar-add-supported-extension ".v")
     (speedbar-add-supported-extension ".sv")
     (speedbar-add-supported-extension ".svh")
     (speedbar-add-supported-extension ".aop")))

(eval-after-load "verilog-mode"
  '(require 'my-verilog))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode

(setq-default org-directory "~/Dropbox/org"
              org-default-notes-file (concat org-directory "/Notes.org"))

(setq org-capture-templates
      '(("t" "Task" entry (function my-org-capture-task-location)
         "* %?" :empty-lines 1 :kill-buffer t)
        ("w" "Work" entry (file+headline "Work.org" "Capture")
         "* TODO %?" :empty-lines 1 :kill-buffer t)
        ("n" "Note" entry (file+headline "Notes.org" "Capture")
         "* %?" :empty-lines 1 :kill-buffer t)
        ("i" "Idea" entry (file+headline "Ideas.org" "Capture")
         "* %?" :empty-lines 1 :kill-buffer t)
        ("h" "Home" entry (file+headline "Home.org" "Capture")
         "* TODO %?" :empty-lines 1 :kill-buffer t)
        ("b" "Buy" entry (file+headline "Buy.org" "Capture")
         "* TODO %?" :empty-lines 1 :kill-buffer t)))

(defvar my-org-kill-task-notes nil)

(defun my-org-capture-task-location ()
  "Put point at the end of the current task's notes."
  (if (null task-current-name)
      (error "No task loaded")
    (if (get-buffer task-notes-filename)
        (set-buffer task-notes-filename)
      (setq my-org-kill-task-notes t)
      (set-buffer
       (find-file-noselect (concat task-top-dir task-current-name "/" task-notes-filename))))
    (goto-char (point-max))))

(defun my-org-capture-after-finalize-hook ()
  "Kill task notes buffer if it wasn't loaded before capture."
  (when my-org-kill-task-notes
    (setq my-org-kill-task-notes nil)
    (let ((buf (get-buffer task-notes-filename)))
      (when buf
        (kill-buffer buf)))))

(add-hook 'org-capture-after-finalize-hook 'my-org-capture-after-finalize-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(defmacro my-keys-define (key fn)
  (list 'define-key 'my-keys-minor-mode-map (list 'kbd key) fn))

(my-keys-define "<delete>" 'delete-char)
(my-keys-define "C-/" 'dabbrev-expand)
(my-keys-define "C-M-h" 'backward-sexp)
(my-keys-define "C-M-j" 'qe-unit-copy)
(my-keys-define "C-M-k" 'qe-unit-kill)
(my-keys-define "C-M-l" 'forward-sexp)
(my-keys-define "C-M-n" 'my-edit-scroll-down)
(my-keys-define "C-M-p" 'my-edit-scroll-up)
(my-keys-define "C-M-y" 'my-edit-yank-pop)
(my-keys-define "C-\\" 'my-expand-yasnippet-or-abbrev)
(my-keys-define "C-c $" 'my-delete-trailing-whitespace)
(my-keys-define "C-c '" 'my-toggle-quotes)
(my-keys-define "C-c +" 'my-inc-num)
(my-keys-define "C-c ," 'my-reformat-comma-delimited-items)
(my-keys-define "C-c ." 'my-kill-results-buffer)
(my-keys-define "C-c /" 'my-ido-insert-bookmark-dir)
(my-keys-define "C-c ;" 'my-insert-comment-line)
(my-keys-define "C-c A" 'align-regexp)
(my-keys-define "C-c C" 'my-comment-region)
(my-keys-define "C-c C-c" 'my-comment-region-toggle)
(my-keys-define "C-c C-f" 'my-ido-recentf-file)
(my-keys-define "C-c C-o" (lambda () (interactive) (call-interactively (if (equal major-mode 'sv-mode) 'sv-mode-other-file 'ff-get-other-file))))
(my-keys-define "C-c G" 'rgrep)
(my-keys-define "C-c P" 'my-pair-delete-backward)
(my-keys-define "C-c R" 'revbufs)
(my-keys-define "C-c TAB" 'indent-region)
(my-keys-define "C-c a" 'my-align)
(my-keys-define "C-c b" 'buffer-face-mode)
(my-keys-define "C-c c" 'my-comment-region-after-copy)
(my-keys-define "C-c f" 'my-ffap)
(my-keys-define "C-c g" 'lgrep)
(my-keys-define "C-c j" 'my-edit-join-line-with-next)
(my-keys-define "C-c l d" 'll-debug-revert)
(my-keys-define "C-c l i" 'my-ll-debug-insert)
(my-keys-define "C-c l r" 'll-debug-renumber)
(my-keys-define "C-c m" 'compile)
(my-keys-define "C-c n" 'my-narrow-nested-dwim)
(my-keys-define "C-c p" 'my-pair-delete-forward)
(my-keys-define "C-c r" 'revert-buffer)
(my-keys-define "C-c s" 'my-rotate-window-buffers)
(my-keys-define "C-c t" 'my-tidy-lines)
(my-keys-define "C-c v" 'toggle-truncate-lines)
(my-keys-define "C-h" 'backward-char)
(my-keys-define "C-j" 'qe-unit-move)
(my-keys-define "C-k" 'my-edit-kill-line)
(my-keys-define "C-l" 'forward-char)
(my-keys-define "C-o" 'my-bs-toggle)
(my-keys-define "C-w" 'qe-unit-kill)
(my-keys-define "C-x *" 'calculator)
(my-keys-define "C-x -" 'my-fit-window)
(my-keys-define "C-x 2" 'my-bs-split-window-vertically)
(my-keys-define "C-x 3" 'my-bs-split-window-horizontally)
(my-keys-define "C-x C-c" 'my-kill-frame-or-emacs)
(my-keys-define "C-x C-n" 'other-window)
(my-keys-define "C-x C-p" (lambda () (interactive (other-window -1))))
(my-keys-define "C-x C-z" (lambda () (interactive) (ding)))
(my-keys-define "C-x E" 'my-apply-macro-to-region-lines)
(my-keys-define "C-x K" 'kill-buffer)
(my-keys-define "C-x M-q" 'my-toggle-buffer-modified)
(my-keys-define "C-x RET" 'my-open-line-below)
(my-keys-define "C-x SPC" 'fixup-whitespace)
(my-keys-define "C-x _" (lambda () (interactive) (my-fit-window t)))
(my-keys-define "C-x `" 'my-flymake-goto-next-error)
(my-keys-define "C-x a" 'kmacro-start-macro-or-insert-counter)
(my-keys-define "C-x e" 'kmacro-end-or-call-macro)
(my-keys-define "C-x k" 'my-kill-buffer)
(my-keys-define "C-x m" 'my-magit-status)
(my-keys-define "C-x t" 'task-map)
(my-keys-define "C-x w" 'my-clone-file)
(my-keys-define "C-x |" 'my-toggle-window-split)
(my-keys-define "C-x ~" 'my-flymake-goto-prev-error)
(my-keys-define "C-y" 'my-edit-yank)
(my-keys-define "C-z" 'undo)
(my-keys-define "M-!" 'my-shell-command-on-current-file)
(my-keys-define "M-#" 'task-bmk-toggle)
(my-keys-define "M-%" 'my-query-replace)
(my-keys-define "M-&" 'my-pop-tag-mark-kill-buffer)
(my-keys-define "M-(" 'task-bmk-buf-prev)
(my-keys-define "M-)" 'task-bmk-buf-next)
(my-keys-define "M-*" 'pop-tag-mark)
(my-keys-define "M-," 'iflipb-previous-buffer)
(my-keys-define "M-." 'iflipb-next-buffer)
(my-keys-define "M-/" 'mdabbrev-expand)
(my-keys-define "M-;" 'comment-indent)
(my-keys-define "M-;" (lambda (&optional arg) (interactive "P") (if arg (qe-backward-word-end) (qe-forward-word-end))))
(my-keys-define "M-=" 'my-count-lines)
(my-keys-define "M-?" (lambda (&optional arg) (interactive "P") (if arg (etags-select-find-tag) (etags-select-find-tag-at-point))))
(my-keys-define "M-G" (lambda (&optional arg) (interactive "P") (if arg (my-pop-back-imenu) (my-ido-imenu-goto-symbol))))
(my-keys-define "M-H" 'qe-backward-word-section)
(my-keys-define "M-J" 'qe-backward-kill-section)
(my-keys-define "M-K" 'qe-forward-kill-section)
(my-keys-define "M-L" 'qe-forward-word-section)
(my-keys-define "M-N" 'my-edit-page-down)
(my-keys-define "M-P" 'my-edit-page-up)
(my-keys-define "M-Q" 'my-unfill)
(my-keys-define "M-RET" 'my-open-line-above)
(my-keys-define "M-SPC" 'my-push-mark-and-marker)
(my-keys-define "M-[" 'my-backward-paragraph-rect)
(my-keys-define "M-]" 'my-forward-paragraph-rect)
(my-keys-define "M-^" 'etags-stack-show)
(my-keys-define "M-`" 'next-error)
(my-keys-define "M-b" 'jump-to-prev-pos)
(my-keys-define "M-c" 'my-rotate-case)
(my-keys-define "M-d" 'my-dired-pop-to-or-create)
(my-keys-define "M-g" 'my-goto-line-column)
(my-keys-define "M-h" 'qe-backward-word)
(my-keys-define "M-i" 'ido-switch-buffer)
(my-keys-define "M-j" 'qe-backward-kill)
(my-keys-define "M-k" 'qe-forward-kill)
(my-keys-define "M-l" 'qe-forward-word)
(my-keys-define "M-n" 'qe-forward-paragraph)
(my-keys-define "M-o" 'bs-show)
(my-keys-define "M-p" 'qe-backward-paragraph)
(my-keys-define "M-q" 'my-fill)
(my-keys-define "M-r c" (lambda () (interactive) (call-interactively 'copy-to-register) (deactivate-mark)))
(my-keys-define "M-r k" 'kill-rectangle)
(my-keys-define "M-r n" 'my-rectangle-number-lines)
(my-keys-define "M-r p" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively 'insert-register))))
(my-keys-define "M-r t" 'string-rectangle)
(my-keys-define "M-s o" 'my-occur)
(my-keys-define "M-u" 'my-recenter)
(my-keys-define "M-w" 'qe-unit-copy)
(my-keys-define "M-z" 'redo)
(my-keys-define "M-~" 'previous-error)

;; (my-keys-define "C-x c" 'clone-indirect-buffer-other-window)

;; These have to be in this order

(my-keys-define "C-c h" 'help-command)
(my-keys-define "C-c h a" 'apropos)
(my-keys-define "C-c h I" 'info-apropos)

;; Keybinding minor mode

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(global-set-key (kbd "<mouse-2>") 'mouse-yank-primary)

(global-set-key (kbd "<S-down-mouse-3>") 'imenu)
(global-set-key (kbd "<C-down-mouse-3>") 'mouse-popup-menubar)

(global-set-key (kbd "<mouse-4>") (lambda () "Scroll up." (interactive) (my-edit-scroll-up 5)))
(global-set-key (kbd "<mouse-5>") (lambda () "Scroll down." (interactive) (my-edit-scroll-down 5)))

;; Translations

(define-key key-translation-map (kbd "C-c N") (kbd "C-x n s"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cisco setup

(global-set-key (kbd "C-x v") clearcase-prefix-map)
(define-key clearcase-mode-map (kbd "C-v") nil)
(define-key clearcase-dired-mode-map (kbd "C-v") nil)

(setq etags-table-alist
      (list
       '("/vob/sse/asic/shared/ver/.*\\.svh?$" "/auto/antares_user/scfrazer/tags/sv/TAGS")
       '("/vob/sse/asic/shared/models/PCIE/expertio_PCIE/PCIE/.*" "/auto/antares_user/scfrazer/tags/sv/TAGS")
       '("/vob/sse/asic/luke/.*\\.svh?$" "/nfs/luke_scratch/tags/sv/TAGS")
       '("/vob/sse/asic/luke/.*\\.aop$" "/nfs/luke_scratch/tags/sv/TAGS")
       '("/vob/sse/asic/luke/.*\\.s$" "/nfs/luke_scratch/tags/rtl/TAGS")
       '("/vob/sse/asic/luke/.*\\.v?$" "/nfs/luke_scratch/tags/rtl/TAGS")
       '("/vob/sse/asic/luke/.*\\.e$" "/nfs/luke_scratch/tags/e/TAGS")
       '("/vob/sse/asic/luke/.*\\.[ch]pp$" "/nfs/luke_scratch/tags/sc/TAGS")
       '("/vob/sse/asic/luke/.*\\.c$" "/nfs/luke_scratch/tags/sc/TAGS")
       ))

(unless (getenv "SV_PATH")
  (setenv "SV_PATH"
          ".:/vob/sse/asic/shared/ver/lib/sv:/vob/cpp/ver/lib/sv:/vob/cpp/ver/shared/sv:/vob/cpp/asic/yoda/rtl/blk:/vob/cpp/asic/yoda/ver/chipdv/env/sv"))

(eval-after-load "sv-mode"
  '(progn
     (setq compilation-error-regexp-alist
           (add-to-list 'compilation-error-regexp-alist
                        '("^Error-.+\n\\(.+\\),\\s-+\\([0-9]+\\)" 1 2)))
     ))

(add-to-list 'auto-mode-alist '("\\.macro$" . cperl-mode))

(add-to-list 'my-bs-never-show-regexps "breakpoint.tcl")

(eval-after-load 'll-debug
  '(ll-debug-register-mode 'c++-mode
                           "vpi_printf(" ");"
                           '(nil "\"" (ll-debug-create-next-debug-string) "\\n\")")
                           '(nil "\"" (ll-debug-create-next-debug-string) " (" (ll-debug-get-c++-function-name) ")"
                                 ("Variable name: "
                                  "  " str "="
                                  '(progn
                                     (if v1
                                         (setq v1 (concat v1 ", " str))
                                       (setq v1 str))
                                     nil)
                                  (let ((fmt (read-string "Format: ")))
                                    (cond
                                     ((string= (downcase fmt) "x")
                                      (concat "0x%" fmt))
                                     (t
                                      (concat "%" fmt)))))
                                 (if v1 "\\n\", " "\\n\"") v1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases

(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'bc 'emacs-lisp-byte-compile)
(defalias 'colors 'list-colors-display)
(defalias 'dec 'my-hex-to-dec)
(defalias 'diff-b 'ediff-buffers)
(defalias 'diff-f 'my-ediff-buffer-with-file)
(defalias 'edbg 'edebug-defun)
(defalias 'file 'my-put-file-name-on-clipboard)
(defalias 'fl 'font-lock-fontify-buffer)
(defalias 'fly 'flymake-mode)
(defalias 'fnd 'my-find-name-dired)
(defalias 'gb 'grep-buffers)
(defalias 'hex 'my-dec-to-hex)
(defalias 'ind 'my-indent)
(defalias 'kr 'browse-kill-ring)
(defalias 'med 'my-font-medium)
(defalias 'mf 'make-frame-on-display)
(defalias 'qrr 'query-replace-regexp)
(defalias 'regb 'my-regexp-backward)
(defalias 'regf 'my-regexp-forward)
(defalias 'sf 'my-sort-fields)
(defalias 'sl 'my-sort-lines)
(defalias 'small 'my-font-small)
(defalias 'tail 'auto-revert-tail-mode)
(defalias 'tdoe 'toggle-debug-on-error)
(defalias 'unt 'my-untabity)
(defalias 'ws 'my-font-lock-show-whitespace)

(mode-fn-map 'html 'org-mode 'org-export-as-html)
(mode-fn-map 'tidy 'cperl-mode 'my-perl-tidy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System setup

(unless window-system
  (my-keys-define "C-M-z" 'suspend-emacs)
  (my-keys-define "C-_" 'dabbrev-expand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS-specific setup

(let ((extra-config (concat "~/.emacs.d/" (symbol-name system-type) ".el")))
  (when (file-exists-p extra-config)
    (load-file extra-config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finish up

(require 'btext-mode)

;; Turn off tramp

(let (elm)
  (dolist (handler (list 'tramp-completion-file-name-handler 'tramp-file-name-handler))
    (when (setq elm (rassq handler file-name-handler-alist))
      (setq file-name-handler-alist (delq elm file-name-handler-alist)))))

;; Terminal server in a Clearcase view

(when (and (not window-system) clearcase-servers-online clearcase-setview-viewtag)
  (require 'server)
  (setq-default server-name clearcase-setview-viewtag)
  (server-start))

;; Command frequency

;; (require 'command-frequency)
;; (command-frequency-mode 1)

;; Time emacs load time

(message "~/.emacs.d/init.el load time = %.3f s" (my-get-load-time))

;; Disabled commands

(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
