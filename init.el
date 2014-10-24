;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode 0))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/org"))

(require 'package)
(package-initialize)

;; Need these first to avoid font-lock/dired issues

(require 'my-font-lock)
(require 'my-dired)

(require 'ag2)
(require 'bm)
(require 'csh-mode)
(require 'etags)
(require 'etags-select)
(require 'etags-table)
(require 'hide-region)
(require 'hl-line)
(require 'iflipb)
(require 'jump-to-prev-pos)
(require 'mdabbrev)
(require 'midnight)
(require 'mode-fn)
(require 'quick-edit)
(require 'rect)
(require 'redo+)
(require 'revbufs)
(require 'sb-imenu)
(require 'scf-mode)
(require 'show-mark)
(require 'sr-speedbar)
(require 'uniquify)
(require 'yank-target)

(require 'my-abbrev)
(require 'my-ace-jump-mode)
(require 'my-bookmark)
(require 'my-buf)
(require 'my-calculator)
(require 'my-cc-mode)
(require 'my-clearcase)
(require 'my-debug)
(require 'my-doxymacs)
(require 'my-ediff)
(require 'my-edit)
(require 'my-ffap)
(require 'my-grep)
(require 'my-grep-ed)
(require 'my-ibuffer)
(require 'my-ido)
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
(require 'my-sgml-xml)
(require 'my-shell)
(require 'my-sort-lines)
(require 'my-sql)
(require 'my-sv-mode)
(require 'my-task)
(require 'my-theme)
(require 'my-tmux)
(require 'my-vc)

(autoload 'align "align" nil t)
(autoload 'align-regexp "align" nil t)
(autoload 'browse-kill-ring "browse-kill-ring" nil t)
(autoload 'compile "compile" nil t)
(autoload 'e-mode "e-mode" "Specman 'e' code editing mode" t)
(autoload 'elog-mode "elog-mode" nil t)
(autoload 'expand-abbrev "abbrev" nil t)
(autoload 'file-template-auto-insert "file-template" nil t)
(autoload 'file-template-find-file-not-found-hook "file-template" nil t)
(autoload 'file-template-insert "file-template" nil t)
(autoload 'highlight-indentation-mode "highlight-indentation" nil t)
(autoload 'htmlize-region "htmlize" nil t)
(autoload 'js2-mode "js2-mode" nil t)
(autoload 'json-mode "json-mode" nil t)
(autoload 'makefile-mode "make-mode" nil t)
(autoload 'php-mode "php-mode" nil t)
(autoload 'rdl-mode "rdl-mode" nil t)
(autoload 'regman "regman" nil t)
(autoload 'rst-mode "rst" "reStructured Text Mode" t)
(autoload 'specterx-mode "specterx-mode" "SpecterX mode" t)
(autoload 'sqlplus "sqlplus" nil t)
(autoload 'sqlplus-mode "sqlplus" nil t)
(autoload 'sse-log-mode "sse-log-mode" nil t)
(autoload 'sv-mode "sv-mode" "SystemVerilog mode" t)
(autoload 'uvm-log-mode "uvm-log-mode" nil t)
(autoload 'vsif-mode "vsif-mode" "VSIF mode" t)

(show-paren-mode t)
(delete-selection-mode t)
(transient-mark-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(global-show-mark-mode 1)
(winner-mode 1)
(when (fboundp 'electric-indent-mode)
  (electric-indent-mode -1))
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode -1))

(setq-default Man-notify-method 'bully
              ace-jump-mode-case-fold nil
              ace-jump-mode-gray-background nil
              ace-jump-mode-scope 'window
              ace-jump-mode-submode-list '(ace-jump-word-mode ace-jump-char-mode ace-jump-line-mode)
              ag2-default-literal t
              backup-inhibited t
              blink-matching-paren-distance nil
              bm-goto-position nil
              bm-recenter t
              bm-wrap-immediately nil
              browse-kill-ring-display-duplicates nil
              browse-kill-ring-highlight-current-entry nil
              browse-kill-ring-maximum-display-length 400
              browse-kill-ring-no-duplicates t
              browse-kill-ring-quit-action (quote kill-and-delete-window)
              browse-kill-ring-separator "---"
              browse-kill-ring-separator-face 'font-lock-function-name-face
              browse-kill-ring-show-preview nil
              browse-kill-ring-use-fontification t
              case-fold-search t
              column-number-mode t
              compare-ignore-whitespace t
              compilation-scroll-output 'first-error
              completions-format 'vertical
              confirm-kill-emacs 'y-or-n-p
              cursor-in-non-selected-windows nil
              cursor-type 'box
              dabbrev-case-fold-search nil
              desktop-restore-frames nil
              diff-switches "-b -u"
              dired-auto-revert-buffer t
              echo-keystrokes 0.1
              etags-select-use-short-name-completion t
              etags-table-search-up-depth 10
              eval-expression-print-length nil
              eval-expression-print-level nil
              even-window-heights nil
              file-template-insert-automatically 'ask
              file-template-paths (list (concat user-emacs-directory "templates/"))
              fill-column 78
              flyspell-mode-map nil
              font-lock-verbose nil
              hi-lock-auto-select-face t
              highlight-changes-active-string " Chg+"
              highlight-changes-global-modes nil
              highlight-changes-passive-string " Chg-"
              hscroll-step 1
              htmlize-output-type 'font
              iflipb-ignore-buffers 'my-buf-ignore-buffer
              indent-tabs-mode nil
              indicate-buffer-boundaries t
              inhibit-startup-message t
              isearch-lazy-highlight-initial-delay 0
              js2-basic-offset 4
              kill-do-not-save-duplicates t
              kill-whole-line t
              large-file-warning-threshold nil
              line-move-visual t
              line-number-display-limit-width 1000
              line-number-mode t
              lpr-command "lpr"
              lpr-lp-system t
              lpr-switches ""
              magit-repo-dirs (list "~/.emacs.d" "~/Projects")
              magit-repo-dirs-depth 2
              make-backup-files nil
              mouse-autoselect-window t
              mouse-highlight 1
              mouse-yank-at-point t
              parens-require-spaces nil
              redisplay-dont-pause t
              rst-mode-lazy nil
              save-abbrevs nil
              scroll-conservatively 10000
              scroll-error-top-bottom t
              scroll-preserve-screen-position t
              shift-select-mode nil
              show-paren-delay 0
              speedbar-indentation-width 2
              speedbar-initial-expansion-list-name "sb-imenu"
              speedbar-use-images nil
              split-width-threshold nil
              tags-revert-without-query t
              truncate-partial-width-windows nil
              uniquify-buffer-name-style 'post-forward-angle-brackets
              user-mail-address (concat "<" (getenv "USER") "@cisco.com>")
              vc-handled-backends nil ;; maybe '(Hg) later
              verilog-auto-endcomments nil
              verilog-auto-indent-on-newline nil
              verilog-auto-lineup '(all)
              verilog-auto-newline nil
              verilog-case-indent 4
              verilog-imenu-flatten t
              verilog-imenu-qualify-names nil
              verilog-indent-begin-after-if nil
              verilog-indent-level 4
              verilog-indent-level-behavioral 0
              verilog-indent-level-declaration 0
              verilog-indent-level-directive 0
              verilog-indent-level-module 0
              verilog-minimum-comment-distance 40
              verilog-tab-always-indent t
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
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.aop$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.cron$" . crontab-mode))
(add-to-list 'auto-mode-alist '("\\.csh$" . csh-mode))
(add-to-list 'auto-mode-alist '("\\.cshrc$" . csh-mode))
(add-to-list 'auto-mode-alist '("\\.e$" . e-mode))
(add-to-list 'auto-mode-alist '("\\.elog$" . elog-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.rdlh?$" . rdl-mode))
(add-to-list 'auto-mode-alist '("\\.s$" . specterx-mode))
(add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))
(add-to-list 'auto-mode-alist '("\\.sv$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.sva$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.svh$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.v$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.vh$" . sv-mode))
(add-to-list 'auto-mode-alist '("\\.vsif$" . vsif-mode))
(add-to-list 'auto-mode-alist '("dve_gui.log$" . uvm-log-mode))
(add-to-list 'auto-mode-alist '("run.log$" . uvm-log-mode))
(add-to-list 'auto-mode-alist '("very.*\\.log$" . elog-mode))

(defun major-mode-from-name ()
  "Choose proper mode for buffers created by switch-to-buffer."
  (let ((buffer-file-name (or buffer-file-name (buffer-name))))
    (set-auto-mode)))
(setq-default major-mode 'major-mode-from-name)

;; Don't use sh-mode for csh files

(dolist (elt interpreter-mode-alist)
  (when (member (car elt) (list "csh" "tcsh"))
    (setcdr elt 'csh-mode)))

;; Comments

(add-to-list 'comment-styles '(my-style t nil t nil))
(setq-default comment-column 0
              comment-empty-lines t
              comment-fill-column 120
              comment-style 'my-style)

;; isearch scroll

(setq isearch-allow-scroll t)
(put 'my-recenter 'isearch-scroll t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun my-align ()
  "Align to an entered char."
  (interactive "*")
  (let ((regexp (concat "\\(\\s-*\\)" (char-to-string (read-char "Align to char:")))))
    (save-excursion
      (align-regexp (region-beginning) (region-end) regexp 1 align-default-spacing))))

(defun my-ansi-color ()
  "ANSI colorize the buffer."
  (interactive)
  (save-restriction
    (widen)
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun my-apply-macro-to-region-lines (top bottom)
  "Like `apply-macro-to-region-lines' but works with offset into line."
  (interactive "r")
  (when (null last-kbd-macro)
    (error "No keyboard macro has been defined"))
  (save-excursion
    (let ((end-marker (copy-marker bottom))
          next-line-marker column)
      (goto-char top)
      (setq column (current-column))
      (beginning-of-line)
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
        (goto-char next-line-marker)
        (save-excursion
          (forward-line 1)
          (set-marker next-line-marker (point)))
        (save-excursion
          (let ((mark-active nil))
            (forward-char column)
            (execute-kbd-macro last-kbd-macro))))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))))

(defun my-ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
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
        (setq i (- i 96)))))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

(defun my-backward-paragraph-rect ()
  "Move backward over what looks like a similar set of lines."
  (interactive)
  (let* ((col (current-column))
         (look-fwd (looking-back "^\\s-*" (point-at-bol)))
         (regexp (regexp-quote
                  (buffer-substring-no-properties
                   (point)
                   (save-excursion
                     (if look-fwd
                         (progn
                           (skip-syntax-forward " ")
                           (unless (eolp) (forward-char 1)))
                       (skip-syntax-backward " ")
                       (unless (bolp) (backward-char 1)))
                     (point)))))
         (again t))
    (while again
      (forward-line -1)
      (setq again (not (bobp)))
      (unless (and (= (move-to-column col) col)
                   (if look-fwd
                       (looking-at regexp)
                     (looking-back regexp (point-at-bol))))
        (forward-line 1)
        (move-to-column col)
        (setq again nil)))))

(defun my-backward-regexp (regexp)
  "Skip lines backward containing a regexp."
  (interactive "sSkip lines backward containing regexp: ")
  (beginning-of-line)
  (forward-line -1)
  (while (and (not (bobp)) (re-search-forward regexp (line-end-position) t))
    (beginning-of-line)
    (forward-line -1)))

(defun my-case-symbol (mode)
  "Change case of symbol.  MODE is 'upcase 'downcase or 'capitalize."
  (interactive "*S")
  (let (beg end)
    (save-excursion
      (unless (looking-at "\\sw\\|\\s_")
        (skip-syntax-backward "^w_"))
      (skip-syntax-forward "w_")
      (setq end (point))
      (skip-syntax-backward "w_")
      (skip-syntax-forward "^w")
      (setq beg (point)))
    (case mode
      ('upcase (upcase-region beg end))
      ('downcase (downcase-region beg end))
      ('capitalize (capitalize-region beg (1+ beg))))))

(defun my-clone-file (filename)
  "Clone the current buffer and write it into FILENAME."
  (interactive "FClone to file: ")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename nil nil nil 'confirm)))

(defun my-color-translate ()
  "Translate between tty color and rgb color."
  (interactive "*")
  (skip-chars-backward "-a-zA-Z0-9#")
  (let ((start (point)) orig)
    (skip-chars-forward "-a-zA-Z0-9#")
    (setq orig (buffer-substring-no-properties start (point)))
    (delete-region start (point))
    (if (= (elt orig 0) ?#)
        (insert "color-" (number-to-string (tty-color-translate orig)))
      (let* ((color-values (tty-color-values orig))
             (red (/ (nth 0 color-values) 256))
             (green (/ (nth 1 color-values) 256))
             (blue (/ (nth 2 color-values) 256)))
        (insert (format "#%02X%02X%02X" red green blue))))))

(defvar my-compile-command (list compile-command))
(defun my-compile ()
  "Call `compile' with selection of commands."
  (interactive)
  (setq compile-command
        (ido-completing-read "Compile command: " my-compile-command))
  (call-interactively 'compile)
  (setq my-compile-command (delq compile-command my-compile-command))
  (add-to-list 'my-compile-command compile-command))

(defun my-comment-or-uncomment-region ()
  "Like `comment-or-uncomment-region', but always uses lines."
  (interactive "*")
  (let ((beg (save-excursion (goto-char (region-beginning)) (point-at-bol)))
        (end (save-excursion (goto-char (region-end)) (point-at-bol))))
    (when (= beg end)
      (setq end (point-at-bol 2)))
    (comment-or-uncomment-region beg end)))

(defun my-comment-region-after-copy ()
  "Insert a copy of the region and comment the original."
  (interactive "*")
  (let ((beg (save-excursion (goto-char (region-beginning)) (point-at-bol)))
        (end (save-excursion (goto-char (region-end)) (point-at-bol))))
    (when (= beg end)
      (setq end (point-at-bol 2)))
    (kill-ring-save beg end)
    (goto-char end)
    (save-excursion (yank))
    (comment-region beg end)))

(defun my-convert-to-ascii ()
  "Change extended chars to ascii equivalents."
  (interactive "*")
  (let ((repls '(("\205" "...")
                 ("\221" "'")
                 ("\222" "'")
                 ("\223" "\"")
                 ("\224" "\"")
                 ("\225" "*")
                 ("\226" "-")
                 ("\227" "--")
                 ("\240" " ")
                 ("\261" "+/-")
                 )))
    (dolist (repl repls)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward (car repl) nil t)
          (replace match (cdr repl) nil t))))))

(defun my-convert-to-base (arg)
  "Convert to decimal, or with prefix arg to hex."
  (interactive "*P")
  (if arg
      (my-dec-to-hex)
    (my-hex-to-dec)))

(defun my-count-lines (&optional arg)
  "Count lines to next blank line, or with prefix arg count lines in region."
  (interactive "P")
  (if arg
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

(defun my-delete-duplicate-lines (&optional arg)
  "Like `delete-duplicate-lines', but operates on the following paragraph,
or the region with prefix arg."
  (interactive "*P")
  (save-excursion
    (if arg
        (delete-duplicate-lines (region-beginning) (region-end) nil nil nil t)
      (delete-duplicate-lines (point-at-bol)
                              (save-excursion (forward-paragraph) (point-at-eol))
                              nil nil nil t))))

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

(defun my-dos2unix ()
  "Remove ^M's from file."
  (interactive "*")
  (set-buffer-file-coding-system 'unix t)
  (goto-char (point-min))
  (while (search-forward (string ?\C-m) nil t)
    (replace-match ""))
  (goto-char (point-min)))

(defun my-fill (&optional arg)
  "Fill paragraph, or region with prefix arg."
  (interactive "*P")
  (if arg
      (fill-region (region-beginning) (region-end))
    (fill-paragraph nil)))

(defun my-forward-paragraph ()
  "Move to next blank line after some text."
  (interactive)
  (beginning-of-line)
  (while (and (looking-at "^\\s-*$") (not (eobp)))
    (forward-line))
  (re-search-forward "^\\s-*$" nil 'go))

(defun my-forward-paragraph-rect ()
  "Move forward over what looks like a similar set of lines."
  (interactive)
  (let* ((col (current-column))
         (look-fwd (looking-back "^\\s-*" (point-at-bol)))
         (regexp (regexp-quote
                  (buffer-substring-no-properties
                   (point)
                   (save-excursion
                     (if look-fwd
                         (progn
                           (skip-syntax-forward " ")
                           (unless (eolp) (forward-char 1)))
                       (skip-syntax-backward " ")
                       (unless (bolp) (backward-char 1)))
                     (point)))))
         (again t))
    (while again
      (forward-line 1)
      (setq again (not (eobp)))
      (unless (and (= (move-to-column col) col)
                   (if look-fwd
                       (looking-at regexp)
                     (looking-back regexp (point-at-bol))))
        (forward-line -1)
        (move-to-column col)
        (setq again nil)))))

(defun my-forward-regexp (regexp)
  "Skip lines containing a regexp."
  (interactive "sSkip lines containing regexp: ")
  (beginning-of-line)
  (while (and (not (eobp)) (re-search-forward regexp (line-end-position) t))
    (beginning-of-line)
    (forward-line 1)))

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

(defun my-indent ()
  "Indent entire buffer."
  (interactive "*")
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun my-line-comment ()
  "Goto a line comment if one exists, or insert a comment at the
end of a non-blank line, or insert an 80-column comment line"
  (interactive "*")
  (let ((pos (point))
        (cs (replace-regexp-in-string "\\s-+$" "" comment-start)))
    (beginning-of-line)
    (if (re-search-forward cs (point-at-eol) t)
        (backward-char (length cs))
      (if (looking-at "\\s-*$")
          (let (char)
            (goto-char pos)
            (insert comment-start)
            (delete-horizontal-space)
            (setq char (char-before))
            (insert-char char (- 80 (- (point) (point-at-bol)))))
        (end-of-line)
        (insert "  " comment-start)
        (delete-horizontal-space)
        (insert" ")))))

(defun my-kill-buffer (arg buffer)
  "Kill buffer and delete window if there is more than one."
  (interactive "P\nbKill buffer: ")
  (kill-buffer buffer)
  (when (> (count-windows) 1)
    (delete-window)))

(defun my-kill-frame-or-emacs ()
  "Kill a frame or emacs"
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-emacs)))

(defun my-kill-results-buffer ()
  "Kill a compliation/grep/*whatever*/cloned buffer in a second window."
  (interactive)
  (when (> (count-windows) 1)
    (other-window 1)
    (unless (and (posix-string-match "[ *]+.*[ *]+\\|.+<[0-9]+>$" (buffer-name))
                 (not (string= "*scratch*" (buffer-name))))
      (other-window 1))
    (when (and (posix-string-match "[ *]+.*[ *]+\\|.+<[0-9]+>$" (buffer-name))
               (not (string= "*scratch*" (buffer-name))))
      (kill-buffer nil)
      (delete-window))))

(defun my-kill-this-buffer (arg)
  "Kill buffer and delete window if there is more than one."
  (interactive "P")
  (kill-buffer nil)
  (when (and (not arg) (> (count-windows) 1))
    (delete-window)))

(defvar my-layout-window-configuration nil)

(defun my-layout-load ()
  "Load a saved window configuration."
  (interactive)
  (when my-layout-window-configuration
    (set-window-configuration my-layout-window-configuration)
    (message "Layout loaded")))

(defun my-layout-save ()
  "Save current window configuration."
  (interactive)
  (setq my-layout-window-configuration (current-window-configuration))
  (message "Layout saved"))

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

(defun my-minibuffer-insert-region ()
  "Insert the region into the minibuffer."
  (interactive)
  (let (region)
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (let ((m (mark t)))
        (when m
          (setq region (buffer-substring-no-properties (region-beginning) (region-end))))))
    (insert region)))

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

(defun my-narrow (&optional arg)
  "Narrow to region, or widen if already narrowed, or with prefix
arg do something special."
  (interactive "P")
  (if (and arg (equal major-mode 'sv-mode))
      (sv-mode-narrow-to-scope)
    (if (/= (buffer-size) (- (point-max) (point-min)))
        (widen)
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min)))))

(defun my-pop-tag-mark-kill-buffer ()
  "Pop tag mark and kill previous buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (pop-tag-mark)
    (unless (equal buf (current-buffer))
      (kill-buffer buf))))

(defun my-put-file-name-on-clipboard (&optional arg)
  "Put the current file name in the kill-ring.
If running inside a tmux session, it will also be put in a tmux copy-buffer.
With ARG and inside tmux, also copy through iTerm2 to clipboard."
  (interactive "P")
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (when (getenv "TMUX")
        (my-tmux-copy-text filename)
        (when arg
          (my-tmux-iterm-copy-text filename)))
      (message filename))))

(defun my-prettify ()
  "Remove trailing space, untabify, reindent."
  (interactive "*")
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

(defun my-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer is not visiting a file")
      (let ((new-name (read-file-name "Rename file to: " filename)))
        (rename-file filename new-name t)
        (set-visited-file-name new-name t t)))))

(defun my-query-replace (&optional arg)
  "Same as `query-replace', but C-u means take from-string from region,
and a number prefix means replace in region."
  (interactive "*P")
  (if (not arg)
      (call-interactively 'query-replace)
    (if (numberp arg)
        (let ((common (query-replace-read-args "Query replace in region" nil))
              (start (region-beginning))
              (end (region-end)))
          (goto-char start)
          (query-replace (nth 0 common) (nth 1 common) nil start end))
      (let (from to)
        (setq from (buffer-substring (region-beginning) (region-end))
              to (read-from-minibuffer
                  (format "Query replace %s with: " from) nil nil nil
                  'query-replace-history))
        (goto-char (region-beginning))
        (query-replace from to)
        (setq query-replace-defaults (cons from to))))))

(defun my-rectangle-number-lines (start end start-at &optional format)
  "Like `rectangle-number-lines' but with better defaults.

START-AT, if non-nil, should be a number from which to begin
counting.  FORMAT, if non-nil, should be a format string to pass
to `format' along with the line count.  When called interactively
with a prefix argument, prompt for START-AT and FORMAT."
  (interactive
   (cond ((equal current-prefix-arg '(4))
          (let* ((start (region-beginning))
                 (end   (region-end))
                 (start-at (read-string "Start at: "))
                 (format "%d"))
            (if (string-match "[a-zA-Z]" start-at)
                (setq start-at (string-to-char start-at)
                      format "%c")
              (setq start-at (string-to-number start-at)))
            (list start end start-at format)))
         ((equal current-prefix-arg '(16))
          (let* ((start (region-beginning))
                 (end   (region-end))
                 (start-at (read-number "Start at: ")))
            (list start end start-at
                  (read-string "Format string: " "%d"))))
         (t
          (list (region-beginning) (region-end) 0 "%d"))))
  (delete-extract-rectangle (region-beginning) (region-end))
  (setq end (point))
  (when (< end start)
    (let ((tmp start))
      (setq start end
            end tmp)))
  (let ((rectangle-number-line-counter start-at))
    (apply-on-rectangle 'rectangle-number-line-callback
                        start end format)))

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

(defun my-server()
  "Start an Emacs server in a ClearCase view."
  (interactive)
  (when (and (not window-system) clearcase-servers-online clearcase-setview-viewtag)
    (require 'server)
    (setq-default server-name clearcase-setview-viewtag)
    (server-start)))

(defun my-set-register (&optional arg)
  "Copy last kill, or with prefix arg region, to a register."
  (interactive "P")
  (if arg
      (set-register (register-read-with-preview "(Region) Set register:")
                  (buffer-substring (region-beginning) (region-end)))
    (set-register (register-read-with-preview "(Last kill) Set register:") (current-kill 0 t))))

(defun my-term ()
  "Like `term', but use default shell without prompting."
  (interactive)
  (set-buffer
   (make-term "terminal" (or explicit-shell-file-name
                             (getenv "ESHELL")
                             (getenv "SHELL")
                             "/bin/sh")))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*terminal*"))

(defun my-tidy-lines ()
  "Tidy up lines in region."
  (interactive "*")
  (let ((beg (save-excursion (goto-char (region-beginning)) (point-at-bol)))
        (end (save-excursion (goto-char (region-end)) (point-at-bol)))
        num-lines)
    (when (= beg end)
      (setq end (point-at-bol 2)))
    (setq num-lines (count-lines beg end))
    (save-excursion
      (goto-char beg)
      (dotimes (idx num-lines)
        (back-to-indentation)
        ;; Multiple spaces
        (while (re-search-forward "[ \t]\\{2,\\}" (point-at-eol) t)
          (unless (my-inside-string-or-comment-p (match-beginning 0))
            (replace-match " ")))
        ;; Space before, space after, space before
        (dolist (regexp (list " \\([[(,;]\\)" "\\([[({]\\) " " \\([])}]\\)"))
          (back-to-indentation)
          (while (re-search-forward regexp (point-at-eol) t)
            (unless (my-inside-string-or-comment-p (match-beginning 0))
              (replace-match "\\1"))))
        (back-to-indentation)
        ;; No space between
        (while (re-search-forward "\\([,;]\\)\\([^ \n]\\)" (point-at-eol) t)
          (unless (my-inside-string-or-comment-p (match-beginning 0))
            (replace-match "\\1 \\2")))
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
  (interactive "*")
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

(defun my-unfill (&optional arg)
  "Unfill paragraph, or region with prefix arg."
  (interactive "*P")
  (let ((fill-column (point-max)))
    (if arg
        (fill-region (region-beginning) (region-end))
      (fill-paragraph 1))))

(defun my-untabity ()
  "Untabify entire buffer."
  (interactive "*")
  (save-excursion
    (untabify (point-min) (point-max))))

(defun my-window-resize (&optional arg)
  "Resize window to `frame-height' / 4.
Prefix with C-u to resize the `next-window'."
  (interactive "P")
  (let ((win (if arg (next-window) (get-buffer-window))))
    (window-resize win (- (/ (frame-height) 4) (window-height win)))))

(defun my-x-color-to-tty-color ()
  (interactive)
  (let (color-num end)
    (skip-chars-backward "a-zA-Z0-9")
    (setq end (save-excursion (skip-chars-forward "a-zA-Z0-9") (point)))
    (setq color-num (tty-color-translate (buffer-substring-no-properties (point) end)))
    (when (> color-num 15)
      (kill-region (point) end)
      (insert "color-" (number-to-string color-num)))))

(defun my-yank-target-jump (&optional arg)
  "Set yank target, or with prefix arg go to yank target, or with numeric arg go to yank source."
  (interactive "P")
  (if (and arg (listp arg))
      (yank-target-go-target)
    (if arg
        (yank-target-go-source)
      (yank-target-set))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice

;; (defadvice show-paren-function (after my-echo-paren-matching-line activate)
;;   "If a matching paren is off-screen, echo the matching line."
;;   (when (char-equal (char-syntax (char-before (point))) ?\))
;;     (let ((matching-text (blink-matching-open)))
;;       (when matching-text
;;         (message matching-text)))))

(defadvice kill-buffer (around my-kill-buffer-advice activate)
  "Don't kill the *scratch* buffer."
  (if (equal (ad-get-arg 0) "*scratch*")
      (bury-buffer)
    ad-do-it))

;; (defadvice quit-window (before advise-quit-window activate)
;;   (when (called-interactively-p 'any)
;;     (ad-set-arg 0 (not (ad-get-arg 0)))))

(defadvice term-handle-exit (after my-term-handle-exit activate)
  "Kill terminal buffer after exit."
  (kill-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(defun my-after-save-hook ()
  (executable-make-buffer-file-executable-if-script-p))

(defun my-ag2-mode-hook ()
  (define-key ag2-mode-map "\C-x\C-q" 'grep-ed-start))

(defun my-diff-mode-hook ()
  (define-key diff-mode-map "q" 'my-kill-this-buffer)
  (define-key diff-mode-map "n" 'diff-hunk-next)
  (define-key diff-mode-map "p" 'diff-hunk-prev))

(defun my-doxymacs-font-lock-hook ()
  (when (member major-mode (list 'c-mode 'c++-mode 'sv-mode))
    (doxymacs-font-lock)))

(defun my-emacs-lisp-mode-hook ()
  (setq comment-column 0))

(defun my-find-file-hook ()
  (when (or (equal (buffer-name) "config_tree.txt")
            (equal (buffer-name) "topology.txt"))
    (my-whitespace-off-hook)
    (my-word-wrap-on-hook)))

(defun my-grep-mode-hook ()
  (define-key grep-mode-map "s" 'scf-mode))

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0)
  (show-mark-mode 0)
  (local-set-key (kbd "C-_") 'dabbrev-expand)
  (local-set-key (kbd "C-/") 'dabbrev-expand)
  (local-set-key (kbd "C-w") 'my-minibuffer-insert-word-after-point)
  (local-set-key (kbd "C-z") 'undo)
  (local-set-key (kbd "M-h") 'my-minibuffer-backward)
  (local-set-key (kbd "M-j") 'my-minibuffer-backward-kill)
  (local-set-key (kbd "M-k") 'my-minibuffer-forward-kill)
  (local-set-key (kbd "M-l") 'my-minibuffer-forward)
  (local-set-key (kbd "M-w") 'my-minibuffer-insert-region)
  (local-set-key (kbd "M-$") (lambda (&optional arg)
                               (interactive "P")
                               (let* ((enable-recursive-minibuffers t)
                                      (dir (my-ido-get-bookmark-dir)))
                                 (when dir
                                   (when arg
                                     (setq dir (concat "/view/CPPDVTOOLS.view" dir)))
                                   (delete-minibuffer-contents)
                                   (insert dir))))))

(defun my-sh-mode-hook ()
  (use-local-map nil))

(defun my-whitespace-off-hook ()
  (my-font-lock-show-whitespace -1))

(defun my-word-wrap-on-hook ()
  (setq truncate-lines nil)
  (setq word-wrap t))

(defun my-verilog-hook ()
  (define-key verilog-mode-map "`" nil)
  (define-key verilog-mode-map (kbd "RET") nil)
  (define-key verilog-mode-map ";" nil))

(add-hook 'Info-mode-hook 'my-whitespace-off-hook)
(add-hook 'after-save-hook 'my-after-save-hook)
(add-hook 'ag2-mode-hook 'my-ag2-mode-hook)
(add-hook 'diff-mode-hook 'my-diff-mode-hook)
(add-hook 'dired-mode-hook 'my-whitespace-off-hook)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'find-file-hook 'my-find-file-hook)
(add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(add-hook 'grep-mode-hook 'my-grep-mode-hook)
(add-hook 'midnight-hook 'recentf-cleanup)
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(add-hook 'sv-mode-hook 'doxymacs-mode)
(add-hook 'uvm-log-mode-hook 'my-whitespace-off-hook)
(add-hook 'uvm-log-mode-hook 'my-word-wrap-on-hook)
(add-hook 'verilog-mode-hook 'my-verilog-hook)

(remove-hook 'midnight-hook 'clean-buffer-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; After loads

(eval-after-load "compile"
  '(progn
     (defun my-compilation-mode-hook ()
       (setq truncate-lines 'one-line-each)
       (goto-char (point-max)))
;;      (defun my-compilation-process-setup-function ()
;;        (setq compilation-window-height (/ (frame-height) 4)))
;;      (setq compilation-process-setup-function 'my-compilation-process-setup-function)
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
     (add-to-list 'file-template-mapping-alist '("\\.csh$" . "template.csh"))
     (add-to-list 'file-template-mapping-alist '("\\.sh$" . "template.sh"))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(defmacro my-keys-define (key fn)
  (list 'define-key 'my-keys-minor-mode-map (list 'kbd key) fn))

(my-keys-define "<delete>" 'delete-char)
(my-keys-define "C-/" 'dabbrev-expand)
(my-keys-define "C-M-h" 'backward-sexp)
(my-keys-define "C-M-k" 'delete-region)
(my-keys-define "C-M-l" 'forward-sexp)
(my-keys-define "C-M-n" 'my-edit-scroll-down)
(my-keys-define "C-M-p" 'my-edit-scroll-up)
(my-keys-define "C-M-y" (lambda () (interactive) (yank-target-go-target) (yank)))
(my-keys-define "C-\\" 'expand-abbrev)
(my-keys-define "C-c #" 'hl-line-mode)
(my-keys-define "C-c $" 'my-delete-trailing-whitespace)
(my-keys-define "C-c '" 'my-toggle-quotes)
(my-keys-define "C-c ," 'my-reformat-comma-delimited-items)
(my-keys-define "C-c ." 'my-kill-results-buffer)
(my-keys-define "C-c ;" 'my-line-comment)
(my-keys-define "C-c =" 'my-ediff-dwim)
(my-keys-define "C-c A" 'align-regexp)
(my-keys-define "C-c C" 'my-comment-region-after-copy)
(my-keys-define "C-c G" 'ag2)
(my-keys-define "C-c N" 'narrow-to-defun)
(my-keys-define "C-c P" 'my-pair-delete-backward)
(my-keys-define "C-c R" 'revbufs)
(my-keys-define "C-c TAB" 'indent-region)
(my-keys-define "C-c U" (lambda () (interactive) (my-case-symbol 'upcase)))
(my-keys-define "C-c a" 'my-align)
(my-keys-define "C-c b" 'my-ido-insert-bookmark-dir)
(my-keys-define "C-c c" 'my-comment-or-uncomment-region)
(my-keys-define "C-c d" 'my-debug-map)
(my-keys-define "C-c e" 'my-term)
(my-keys-define "C-c f" 'my-ffap)
(my-keys-define "C-c g" 'ag2-local)
(my-keys-define "C-c i" (lambda () "Insert register" (interactive) (let ((current-prefix-arg '(4))) (call-interactively 'insert-register))))
(my-keys-define "C-c j" 'my-edit-join-line-with-next)
(my-keys-define "C-c l" (lambda () (interactive) (my-case-symbol 'downcase)))
(my-keys-define "C-c m" 'my-compile)
(my-keys-define "C-c n" 'my-narrow)
(my-keys-define "C-c o" (lambda () (interactive) (call-interactively (if (equal major-mode 'sv-mode) 'sv-mode-other-file 'ff-get-other-file))))
(my-keys-define "C-c p" 'my-pair-delete-forward)
(my-keys-define "C-c r" 'revert-buffer)
(my-keys-define "C-c s" 'my-set-register)
(my-keys-define "C-c t" 'my-tidy-lines)
(my-keys-define "C-c u" (lambda () (interactive) (my-case-symbol 'capitalize)))
(my-keys-define "C-c v" 'toggle-truncate-lines)
(my-keys-define "C-c w" (lambda (&optional arg) (interactive "P") (if arg (winner-redo) (winner-undo))))
(my-keys-define "C-c y" 'yank-target-map)
(my-keys-define "C-d" 'delete-forward-char)
(my-keys-define "C-f" 'qe-unit-move)
(my-keys-define "C-h" 'backward-char)
(my-keys-define "C-j" 'ace-jump-mode)
(my-keys-define "C-k" 'my-edit-kill-line)
(my-keys-define "C-l" 'forward-char)
(my-keys-define "C-o" 'my-buf-toggle)
(my-keys-define "C-r" 'my-isearch-backward)
(my-keys-define "C-s" 'my-isearch-forward)
(my-keys-define "C-v" 'my-edit-newline-and-indent)
(my-keys-define "C-w" 'qe-unit-kill)
(my-keys-define "C-x 2" 'my-buf-split-window-vertically)
(my-keys-define "C-x 3" 'my-buf-split-window-horizontally)
(my-keys-define "C-x (" 'kmacro-start-macro-or-insert-counter)
(my-keys-define "C-x *" 'calculator)
(my-keys-define "C-x -" 'my-window-resize)
(my-keys-define "C-x C-c" 'my-kill-frame-or-emacs)
(my-keys-define "C-x C-h" 'hide-region-toggle)
(my-keys-define "C-x C-n" 'other-window)
(my-keys-define "C-x C-p" (lambda () (interactive (other-window -1))))
(my-keys-define "C-x C-r" 'my-ido-recentf-file)
(my-keys-define "C-x C-z" (lambda () (interactive) (ding)))
(my-keys-define "C-x E" 'my-apply-macro-to-region-lines)
(my-keys-define "C-x K" 'my-kill-buffer)
(my-keys-define "C-x M" 'my-magit-history)
(my-keys-define "C-x M-q" 'my-toggle-buffer-modified)
(my-keys-define "C-x SPC" 'fixup-whitespace)
(my-keys-define "C-x _" (lambda () (interactive) (my-window-resize t)))
(my-keys-define "C-x `" 'my-flymake-goto-next-error)
(my-keys-define "C-x c" 'clone-indirect-buffer-other-window)
(my-keys-define "C-x d" 'doxymacs-mode-map)
(my-keys-define "C-x f" 'flymake-start-syntax-check)
(my-keys-define "C-x k" 'kill-buffer)
(my-keys-define "C-x m" 'magit-status)
(my-keys-define "C-x t" 'task-map)
(my-keys-define "C-x w" 'my-clone-file)
(my-keys-define "C-x |" 'my-toggle-window-split)
(my-keys-define "C-x ~" 'my-flymake-goto-prev-error)
(my-keys-define "C-y" 'qe-yank)
(my-keys-define "C-z" 'undo)
(my-keys-define "M-!" 'my-shell-command-on-current-file)
(my-keys-define "M-#" (lambda (&optional arg) (interactive "P") (if arg (bm-show-all) (bm-toggle))))
(my-keys-define "M-%" 'my-query-replace)
(my-keys-define "M-&" 'my-pop-tag-mark-kill-buffer)
(my-keys-define "M-'" 'qe-backward-word-end)
(my-keys-define "M-(" 'bm-previous)
(my-keys-define "M-)" 'bm-next)
(my-keys-define "M-*" 'pop-tag-mark)
(my-keys-define "M-," 'iflipb-previous-buffer)
(my-keys-define "M-." 'iflipb-next-buffer)
(my-keys-define "M-/" 'mdabbrev-expand)
(my-keys-define "M-;" 'qe-forward-word-end)
(my-keys-define "M-=" 'my-count-lines)
(my-keys-define "M-?" (lambda (&optional arg) (interactive "P") (if arg (etags-select-find-tag) (etags-select-find-tag-at-point))))
(my-keys-define "M-G" (lambda (&optional arg) (interactive "P") (if arg (my-pop-back-imenu) (my-ido-imenu-goto-symbol))))
(my-keys-define "M-H" 'qe-backward-word-section)
(my-keys-define "M-J" 'qe-backward-kill-section)
(my-keys-define "M-K" 'qe-forward-kill-section)
(my-keys-define "M-L" 'qe-forward-word-section)
(my-keys-define "M-N" 'scroll-up-command)
(my-keys-define "M-P" 'scroll-down-command)
(my-keys-define "M-Q" 'my-unfill)
(my-keys-define "M-RET" 'my-edit-newline-and-indent-above)
(my-keys-define "M-SPC" 'my-yank-target-jump)
(my-keys-define "M-\"" (lambda () (interactive) (forward-comment (point-max))))
(my-keys-define "M-]" (lambda (&optional arg) (interactive "P") (if arg (my-backward-paragraph-rect) (my-forward-paragraph-rect))))
(my-keys-define "M-`" 'next-error)
(my-keys-define "M-b" 'jump-to-prev-pos)
(my-keys-define "M-c" 'my-tmux-iterm-copy)
(my-keys-define "M-d" 'my-dired-pop-to-or-create)
(my-keys-define "M-g" 'my-goto-line-column)
(my-keys-define "M-h" 'qe-backward-word)
(my-keys-define "M-i" 'ido-switch-buffer)
(my-keys-define "M-j" 'qe-backward-kill)
(my-keys-define "M-k" 'qe-forward-kill)
(my-keys-define "M-l" 'qe-forward-word)
(my-keys-define "M-n" 'qe-forward-paragraph)
(my-keys-define "M-o" 'my-ibuffer)
(my-keys-define "M-p" 'qe-backward-paragraph)
(my-keys-define "M-q" 'my-fill)
(my-keys-define "M-r SPC" 'rectangle-mark-mode)
(my-keys-define "M-r j" 'jump-to-register)
(my-keys-define "M-r k" 'kill-rectangle)
(my-keys-define "M-r n" 'my-rectangle-number-lines)
(my-keys-define "M-r t" 'string-rectangle)
(my-keys-define "M-r w" 'window-configuration-to-register)
(my-keys-define "M-s G" 'my-rgrep)
(my-keys-define "M-s O" 'my-multi-occur)
(my-keys-define "M-s g" 'my-lgrep)
(my-keys-define "M-s o" 'my-occur)
(my-keys-define "M-t" 'my-tmux-copy)
(my-keys-define "M-u" 'my-recenter)
(my-keys-define "M-w" 'qe-unit-copy)
(my-keys-define "M-z" 'redo)
(my-keys-define "M-}" 'my-forward-paragraph)
(my-keys-define "M-~" 'previous-error)

;; These have to be in this order

(my-keys-define "C-c h" 'help-command)
(my-keys-define "C-c h a" 'apropos)
(my-keys-define "C-c h I" 'info-apropos)

;; Keybinding minor mode

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cisco setup

(global-set-key (kbd "C-x v") clearcase-prefix-map)
(define-key clearcase-mode-map (kbd "C-v") nil)
(define-key clearcase-dired-mode-map (kbd "C-v") nil)

(setq ag2-files-aliases-alist '(("dv" . "\\.(sv|svh|cpp|hpp)$")
                                ("rtl" . "\\.(s|v|vh)$")
                                ("vtt" . "\\.(java|php|json|html)$")))

(setq etags-table-alist
      (list
       '("/vob/sse/asic/.*\\.svh?$" "/auto/luke_user5/scfrazer/tags/sv/TAGS")
       '("/vob/sse/asic/shared/models/PCIE/expertio_PCIE/PCIE/.*" "/auto/luke_user5/scfrazer/tags/sv/TAGS")
       '("/vob/sse/asic/.*\\.[ch]pp$" "/auto/luke_user5/scfrazer/tags/cpp/TAGS")
       ))

(when clearcase-servers-online
  (ll-debug-register-mode 'c++-mode
                          "dvc_info(" ");"
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

(unless (getenv "SV_PATH")
  (setenv "SV_PATH"
          ".:/vob/sse/asic/shared/ver/lib/sv:/vob/cpp/ver/lib/sv:/vob/cpp/ver/shared/sv:/vob/cpp/asic/yoda/rtl/blk:/vob/cpp/asic/yoda/ver/chipdv/env/sv"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases

(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'bc 'emacs-lisp-byte-compile)
(defalias 'bre 'my-backward-regexp)
(defalias 'colors 'list-colors-display)
(defalias 'dec 'my-hex-to-dec)
(defalias 'edbg 'edebug-defun)
(defalias 'eb 'ediff-buffers)
(defalias 'file 'my-put-file-name-on-clipboard)
(defalias 'fl 'font-lock-fontify-buffer)
(defalias 'fly 'flymake-mode)
(defalias 'fnd 'my-find-name-dired)
(defalias 'fre 'my-forward-regexp)
(defalias 'hex 'my-dec-to-hex)
(defalias 'hli 'highlight-indentation-mode)
(defalias 'ind 'my-indent)
(defalias 'init (lambda () (interactive) (find-file user-init-file)))
(defalias 'kr 'browse-kill-ring)
(defalias 'qrr 'query-replace-regexp)
(defalias 'ren 'rename-buffer)
(defalias 'rl 'register-list)
(defalias 'rot 'my-rotate-window-buffers)
(defalias 'sb 'sr-speedbar-toggle)
(defalias 'serve 'my-server)
(defalias 'sf 'my-sort-fields)
(defalias 'sl 'my-sort-lines)
(defalias 'tail 'auto-revert-tail-mode)
(defalias 'tdoe 'toggle-debug-on-error)
(defalias 'uniq 'my-delete-duplicate-lines)
(defalias 'unt 'my-untabity)
(defalias 'vc_gen (lambda () (interactive) (require 'vc_gen)))
(defalias 'vtt (lambda () (interactive) (require 'vtt)))
(defalias 'work (lambda () (interactive) (find-file (expand-file-name "~/Documents/Org/Work.org"))))
(defalias 'ws 'my-font-lock-show-whitespace)

(mode-fn-map 'html 'org-mode 'org-export-as-html)
(mode-fn-map 'tidy 'cperl-mode 'my-perl-tidy)
(mode-fn-map 'tidy 'c++-mode 'my-cc-mode-uncrustify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System setup

(unless window-system

  (set-terminal-coding-system 'utf-8)
  ;; (set-keyboard-coding-system 'utf-8)
  ;; (prefer-coding-system 'utf-8)

  (defface my-display-table-face
    '((t (:foreground "black" :background "#FFFF00")))
    "Face for terminal truncation/wrapping glyphs."
    :group 'faces)

  (let ((truncation-glyph (make-glyph-code ?\$ 'my-display-table-face))
        (wrap-glyph (make-glyph-code ?\\ 'my-display-table-face))
        (escape-glyph (make-glyph-code ?\\ 'my-display-table-face))
        (control-glyph (make-glyph-code ?\^ 'my-display-table-face)))
    (set-display-table-slot standard-display-table 'truncation truncation-glyph)
    (set-display-table-slot standard-display-table 'wrap wrap-glyph)
    (set-display-table-slot standard-display-table 'escape escape-glyph)
    (set-display-table-slot standard-display-table 'control control-glyph))

  (my-keys-define "<f1>" 'xterm-mouse-mode)
  (my-keys-define "C-M-z" 'suspend-emacs)
  (my-keys-define "C-_" 'dabbrev-expand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS-specific setup

(let ((extra-config (concat user-emacs-directory (symbol-name system-type) ".el")))
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

;; Disabled commands

(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
