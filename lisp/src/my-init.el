;; my-init.el

(setq vc-follow-symlinks t)

;; Need these first

(require 'cl-lib)

(require 'my-font-lock)
(setq-default my-font-lock-auto-whitespace t)
(setq minor-mode-alist (remove (assq 'whitespace-mode minor-mode-alist) minor-mode-alist))

(require 'highlight-indent-guides)
(defun my-highlight-indent-guides-auto-set-faces (orig-fun)
  (if (equal frame-background-mode 'light)
      (set-face-foreground 'highlight-indent-guides-character-face
                           (color-lighten-name (face-foreground 'font-lock-comment-face) 25))
    (set-face-foreground 'highlight-indent-guides-character-face
                           (color-darken-name (face-foreground 'font-lock-comment-face) 10))))
(advice-add #'highlight-indent-guides-auto-set-faces :around #'my-highlight-indent-guides-auto-set-faces)
(setq minor-mode-alist (remove (assq 'highlight-indent-guides-mode minor-mode-alist) minor-mode-alist))

(require 'bind-key)
(require 'bind-remind)

(require 'my-dired)
(bind-key* "M-d" 'my-dired-pop-to-or-create)

;; Required packages

(require 'my-abbrev)
(bind-keys* ("C-\\" . expand-abbrev))
(setq save-abbrevs nil)

(require 'my-avy)
(bind-keys*("C-j" . my-avy-goto))

(require 'my-bookmark)
(my-bookmark-reload)

(require 'my-buf)
(bind-keys* ("C-o". my-buf-toggle))

(require 'compile)

(require 'my-edit)
(bind-keys* ("C-M-n" . my-edit-scroll-down)
            ("C-M-p" . my-edit-scroll-up)
            ("C-M-v" . my-edit-newline-and-indent-around)
            ("C-c j" . my-edit-join-line-with-next)
            ("C-k"   . my-edit-kill-line)
            ("C-v"   . my-edit-newline-and-indent)
            ("M-RET" . my-edit-newline-and-indent-above))

(require 'my-ediff)
(bind-keys* ("C-c =" . my-ediff-dwim))

(require 'my-electric)

(require 'filladapt)
(setq filladapt-mode-line-string nil)
(setq-default filladapt-mode t)

(require 'my-fzf)
(bind-keys* ("C-x w" . my-fzf-project-file)
            ("C-x W" . my-fzf-any-project-file)
            ("C-x f" . my-fzf-local-file)
            ("C-x F" . my-fzf-any-local-file)
            ("C-x d" . my-fzf-any-project-directory)
            ("C-x D" . my-fzf-any-local-directory))

(require 'goto-chg)
(bind-keys* ("C-M-@" . goto-last-change))

(require 'my-ido)
(bind-keys* ("C-c b"   . my-ido-insert-bookmark-dir)
            ("C-x C-r" . my-ido-recentf-file)
            ("M-i"     . ido-switch-buffer))
(progn
  (defun my-ido-fzf ()
    "Fuzzy find file under directory being input"
    (interactive)
    (setq ido-exit 'fallback)
    (setq ido-fallback 'my-ido-fzf-start)
    (exit-minibuffer))
  (defun my-ido-fzf-start ()
    "Start fzf from ido"
    (interactive)
    (let ((process-environment
           (cons "FZF_DEFAULT_COMMAND=fd --type f --hidden --exclude .git --no-ignore" process-environment)))
      (fzf/start ido-current-directory)))
  (define-key ido-file-completion-map (kbd "C-s") 'my-ido-fzf))

(require 'my-isearch)
;; Use global-set-key so minor modes can override
(global-set-key (kbd "C-f") 'my-isearch-search-forward-line)
(global-set-key (kbd "C-r") 'my-isearch-backward)
(global-set-key (kbd "C-s") 'my-isearch-forward)
(setq isearch-allow-scroll t
      lazy-highlight-initial-delay 0)
(put 'my-recenter 'isearch-scroll t)

(require 'jump-to-prev-pos)
(bind-keys* ("M-b" . jump-to-prev-pos))

(require 'mode-fn)
(mode-fn-map 'tidy 'c++-mode 'my-cc-mode-tidy)
(mode-fn-map 'tidy 'c-mode 'my-cc-mode-tidy)
(mode-fn-map 'tidy 'php-mode 'my-php-tidy)
(mode-fn-map 'tidy 'web-mode 'my-web-mode-beautify)

(require 'my-mode-line)

(require 'my-occur)
(bind-keys* ("M-s O" . my-multi-occur)
            ("M-s o" . my-occur))

(require 'my-recentf)

(require 'redo+)

(require 'sh-script)

(require 'show-mark)
(global-show-mark-mode 1)
(setq minor-mode-alist (remove (assq 'show-mark-mode minor-mode-alist) minor-mode-alist))

(require 'my-task)
(require 'my-undo)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'use-package)
(setq use-package-verbose t)

(require 'my-win)
(bind-keys* ("C-x 2" . my-win-split-vertically)
            ("C-x 3" . my-win-split-horizontally)
            ("C-x ^" . my-win-resize-up-start)
            ("C-x V" . my-win-resize-down-start)
            ("C-x {" . my-win-resize-left-start)
            ("C-x }" . my-win-resize-right-start))

(require 'zsh-edit-mode)

;; Deferred packages

(use-package align
  :bind* (("C-c A" . align-regexp)
          ("C-c a" . align)
          ("C-x a" . my-align))
  :config
  (progn
    (defun my-align ()
      "Align to an entered char."
      (interactive "*")
      (let ((regexp (concat "\\(\\s-*\\)" (regexp-quote (char-to-string (read-char "Align to char:"))))))
        (save-excursion
          (align-regexp (region-beginning) (region-end) regexp 1 align-default-spacing))))))

(use-package as-mode
  :mode (("\\.s\\'" . as-mode)))

(use-package asic-compile
  :commands (asic-compile))

(use-package bm
  :bind* (("M-^" . my-bm-toggle-or-show)
          ("M-(" . bm-previous)
          ("M-)" . bm-next))
  :config
  (progn
    (setq bm-goto-position nil
          bm-recenter t
          bm-wrap-immediately nil)
    (defun my-bm-toggle-or-show (&optional arg)
      "Toggle or show bookmarks"
      (interactive "P")
      (if arg (bm-show-all) (bm-toggle)))))

(use-package calculator
  :bind* ("C-x *" . calculator)
  :config
  (require 'my-calculator))

(use-package cc-mode
  :defer t
  :config
  (require 'my-cc-mode))

(use-package deft
  :bind* ("<f8>" . deft)
  :commands (deft)
  :config
  (progn
    (setq-default deft-auto-save-interval 0
                  deft-current-sort-method 'title
                  deft-directory "~/notes"
                  deft-extensions '("txt" "md" "org")
                  deft-strip-summary-regexp "\\([\n\t]\\|^[^#].*$\\)")
    (defun my-deft-parse-summary (orig-fun contents title)
      (replace-regexp-in-string "[ ]+" " " (apply orig-fun (list contents title))))
    (advice-add #'deft-parse-summary :around #'my-deft-parse-summary)
    (defun my-deft-open-file-hook ()
      (when (get-buffer deft-buffer)
        (kill-buffer deft-buffer)))
    (add-hook 'deft-open-file-hook 'my-deft-open-file-hook)))

(use-package diff-mode
  :defer t
  :defines (diff-mode-shared-map)
  :config
  (progn
    (setq diff-default-read-only t
          diff-switches "-b -u")
    (bind-keys :map diff-mode-shared-map
               ("q" . kill-this-buffer)
               ("n" . diff-hunk-next)
               ("p" . diff-hunk-prev))
    (defun my-diff-mode-hook ()
      (diff-auto-refine-mode 1))
    (add-hook 'diff-mode-hook 'my-diff-mode-hook)))

(use-package doxymacs
  :defer t
  :config
  (progn
    (require 'my-doxymacs)
    (defun my-doxymacs-font-lock-hook ()
      (when (member major-mode (list 'c-mode 'c++-mode 'sv-mode))
        (doxymacs-font-lock)))
    (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
    (add-hook 'sv-mode-hook 'doxymacs-mode)))

(use-package my-fd
  :bind* (("M-s F" . my-fd-project)
          ("M-s f" . my-fd-current)))
(defalias 'fd 'my-fd-current)
(defalias 'fd-p 'my-fd-project)

(use-package ffap
  :bind* (("C-c f" . my-ffap)
          ("C-c o" . my-other-file))
  :init
  (defun my-other-file ()
    (interactive)
    (call-interactively
     (cl-case major-mode
       ('sv-mode 'sv-mode-other-file)
       ('c++-mode 'my-cc-other-file)
       (t 'ff-get-other-file))))
  :config
  (require 'my-pop-back)
  (require 'my-ffap))

(use-package fill-function-arguments
  :bind* ("C-c ," . fill-function-arguments-dwim)
  :config
  (progn
    (setq-default fill-function-arguments-first-argument-same-line t
                  fill-function-arguments-last-argument-same-line t
                  fill-function-arguments-indent-after-fill t)))

(use-package flymake
  :commands (flymake-goto-next-error
             flymake-goto-prev-error)
  :init
  (defalias 'fly 'flymake-mode)
  :config
  (require 'my-flymake))

(use-package flyspell
  ;; :bind* (;; TODO ("<f3>" . my-flyspell-goto-prev-error)
  ;;         ("<f4>" . flyspell-goto-next-error))
  :commands (flyspell-prog-mode
             my-flyspell-prog-mode)
  :init
  (progn
    (defun my-flyspell-prog-mode ()
      "Turn on `flyspell-prog-mode' and check the buffer"
      (interactive)
      (flyspell-prog-mode)
      (flyspell-buffer))
    (defalias 'spell 'my-flyspell-prog-mode))
  :config
  (progn
    (defun my-flyspell-generic-progmode-verify (orig-fun)
      (and (apply orig-fun '())
           (save-excursion
             (skip-syntax-backward "w")
             (not (looking-at ".*[A-Z].*[A-Z]")))))
    (advice-add #'flyspell-generic-progmode-verify :around #'my-flyspell-generic-progmode-verify)))

(use-package git-simple
  :bind-keymap (("C-x g" . git-simple-global-map)))

(use-package git-timemachine
  :defer t
  :config
  (setq git-timemachine-abbreviation-length 7))

(use-package grep
  :bind* (("M-s G" . my-rgrep)
          ("M-s g" . my-lgrep))
  :config
  (progn
    (require 'my-grep)
    (require 'my-grep-ed)
    (bind-key "q" 'my-kill-results-buffer grep-mode-map)))

(use-package hl-line
  :bind* ("C-c #" . hl-line-mode))

(use-package ibuffer
  :bind* ("M-o" . my-ibuffer)
  :config
  (require 'my-ibuffer))

(use-package iflipb
  :bind* (("M-," . iflipb-previous-buffer)
          ("M-." . iflipb-next-buffer))
  :config
  (progn
    (setq iflipb-ignore-buffers 'my-buf-ignore-buffer)))

(use-package my-imenu
  :bind* ("M-G" . my-ido-imenu-nav)
  :commands (my-ido-imenu-goto-symbol)
  :init
  (defun my-ido-imenu-nav (&optional arg)
    (interactive "P")
    (if arg (my-pop-back-imenu) (my-ido-imenu-goto-symbol)))
  :config
  (require 'my-pop-back))

(use-package my-increment-number
  :commands (my-dec-to-hex my-hex-to-dec))

(use-package ll-debug
  :bind* (("C-c d C"   . my-debug-comment-region-after-copy)
          ("C-c d C-r" . my-debug-isearch-backward)
          ("C-c d C-s" . my-debug-isearch-forward)
          ("C-c d O"   . my-debug-multi-occur)
          ("C-c d R"   . ll-debug-revert)
          ("C-c d c"   . my-debug-comment-region)
          ("C-c d d"   . my-debug-insert-line)
          ("C-c d i"   . my-debug-insert-ll)
          ("C-c d n"   . my-debug-next)
          ("C-c d o"   . my-debug-occur)
          ("C-c d p"   . my-debug-previous)
          ("C-c d r"   . ll-debug-renumber))
  :config
  (require 'my-debug))

;; (use-package magit
;;   :commands (magit-status)
;;   :config
;;   (progn
;;     (require 'my-magit)
;;     (setq magit-auto-revert-mode nil
;;           magit-backup-mode nil
;;           magit-delete-by-moving-to-trash nil
;;           magit-diff-auto-show nil
;;           magit-popup-show-help-echo nil
;;           magit-popup-show-help-section nil
;;           magit-repository-directories (list "~/.emacs.d" "~/Projects")
;;           magit-repository-directories-depth 2)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (require 'my-markdown))

(use-package mdabbrev
  :bind* ("M-/" . mdabbrev-expand))

(use-package nxml-mode
  :defer t
  :config
  (progn
    (defun my-nxml-forward-balanced ()
      (interactive)
      (let ((nxml-sexp-element-flag t)) (nxml-forward-balanced-item)))
    (defun my-nxml-backward-balanced ()
      (interactive)
      (let ((nxml-sexp-element-flag t)) (nxml-forward-balanced-item -1)))
    (bind-keys :map nxml-mode-map
               ("C-c >" . my-nxml-forward-balanced)
               ("C-c <" . my-nxml-backward-balanced)
               ("C-c &" . nxml-insert-named-char))))

(use-package package
  :config
  (progn
    (setq-default package-check-signature nil)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))))

(use-package my-pair
  :bind* (("C-c ("  . my-pair-open-paren-dwim)
          ("C-c )"  . my-pair-close-paren-dwim)
          ("C-c ["  . my-pair-open-paren-dwim)
          ("C-c ]"  . my-pair-close-paren-dwim)
          ("C-c {"  . my-pair-open-paren-dwim)
          ("C-c }"  . my-pair-close-paren-dwim)
          ("C-c '"  . my-pair-quotes-dwim)
          ("C-c \"" . my-pair-quotes-dwim)
          ("C-c `"  . my-pair-quotes-dwim)
          ("C-c -"  . my-pair-delete)
          ("C-c e"  . my-pair-close-all)
          ("M-a"    . my-pair-step-out-backward)
          ("M-e"    . my-pair-step-out-forward)))

(use-package perl-mode
  :mode (("\\.pl\\'" . perl-mode))
  :config
  (require 'my-perl))

(use-package python
  :mode (("\\.py\\'" . python-mode))
  :config
  (require 'my-python))

(use-package quick-edit
  :bind* (("C-w" . qe-unit-kill)
          ("C-y" . qe-yank)
          ("M-'" . qe-backward-word-end)
          ("M-;" . qe-forward-word-end)
          ("M-H" . qe-backward-word-section)
          ("M-J" . qe-backward-kill-section)
          ("M-K" . qe-forward-kill-section)
          ("M-L" . qe-forward-word-section)
          ("M-h" . qe-backward-word)
          ("M-j" . qe-backward-kill)
          ("M-k" . qe-forward-kill)
          ("M-l" . qe-forward-word)
          ("M-n" . qe-forward-paragraph)
          ("M-p" . qe-backward-paragraph)
          ("M-w" . qe-unit-copy)
          ("M-{" . qe-backward-curly-block)
          ("M-}" . qe-forward-curly-block))
  :config
  (progn
    (defun my-qe-unit-copy-1 (orig-fun key-seq)
      (if (= (aref key-seq 0) ?o)
          (let* ((pos (point))
                 (at-bol (= (point-at-bol) pos))
                 str)
            (goto-char (if at-bol
                           (avy--line)
                         (car (avy-process (avy--regex-candidates "^\\s-*\\([^ \t\n]\\)" nil nil nil 1)))))
            (setq str (buffer-substring-no-properties (point) (point-at-eol)))
            (goto-char pos)
            (insert str)
            (when at-bol
              (insert "\n"))
            (goto-char pos))
        (apply orig-fun (list key-seq))))
    (advice-add 'qe-unit-copy-1 :around #'my-qe-unit-copy-1)
    (defun my-qe-unit-bounds (orig-fun key-seq)
      (if (= (aref key-seq 0) 12) ;; C-l
          (cons (point)
                (save-excursion
                  (goto-char (avy--line))
                  (point)))
        (apply orig-fun (list key-seq))))
    (advice-add 'qe-unit-bounds :around #'my-qe-unit-bounds)))

(use-package my-rect
  :bind* (("M-SPC" . my-rect-push-mark)
          ("M-r" . my-rect/body)))

(use-package revbufs
  :bind* ("C-c R" . revbufs))

(use-package my-rg
  :bind* (("C-c G" . my-rg-project)
          ("C-c g" . my-rg-current)))

(use-package sgml-mode
  :mode (("\\.\\(xml\\|xsl\\|rng\\)\\'" . sgml-mode))
  :config
  (require 'my-sgml-mode))

(use-package smerge-mode
  :commands (smerge-mode
             smerge-ediff)
  :config
  (progn
    (defhydra smerge-hydra
      (:color pink :hint nil :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _o_urs (upper)       _<_: base/upper        _k_ill current
_p_rev       _b_ase (middle)      _=_: upper/lower
^^           _t_heirs (lower)     _>_: base/lower
^^           _a_ll                _r_efine
^^           _RET_: current       _e_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("o" smerge-keep-upper)
      ("b" smerge-keep-base)
      ("t" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("r" smerge-refine)
      ("e" smerge-ediff)
      ("k" smerge-kill-current)
      ("q" nil "cancel" :color blue))))

(use-package my-sort-lines
  :commands (my-sort-lines)
  :init
  (defalias 'sl 'my-sort-lines))

(use-package sr-speedbar
  :bind* ("C-c I" . sr-speedbar-toggle)
  :config
  (require 'my-speedbar))

(use-package sv-mode
  :mode (("\\.sv\\'" . sv-mode)
         ("\\.svh\\'" . sv-mode)
         ("\\.sva\\'" . sv-mode)
         ("\\.v\\'" . sv-mode)
         ("\\.vh\\'" . sv-mode))
  :config
  (progn
    (setq-default verilog-error-regexp-emacs-alist nil)
    (require 'verilog-mode)
    (require 'my-sv-mode)))

(use-package my-tmux
  :bind* (("C-t" . my-tmux-copy)))

(use-package vc
  :commands (vc-dir)
  :init
  (setq vc-handled-backends '(Git))
  :config
  (require 'my-vc))

(use-package visual-regexp
  :commands (vr/query-replace)
  :config
  (setq vr/auto-show-help nil
        vr/default-replace-preview t))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.css\\'" . web-mode))
  :config
  (require 'my-web-mode))

(use-package xref
  :bind* (("M-&" . my-xref-pop-marker-stack-kill-buffer)
          ("M-*" . xref-pop-marker-stack)
          ("M-?" . xref-find-definitions))
  :config
  (require 'my-xref))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode))
  :config
  (require 'my-yaml-mode))

(use-package yank-target
  :bind-keymap (("C-c y" . yank-target-map))
  :config
  (progn
    (defun my-yank-target-go-yank ()
      "Go to target and yank"
      (interactive)
      (yank-target-go-target)
      (yank))
    (defun my-yank-target-jump (&optional arg)
      "Set yank target, or with prefix arg go to yank target, or with numeric arg go to yank source."
      (interactive "P")
      (if (and arg (listp arg))
          (yank-target-go-target)
        (if arg
            (yank-target-go-source)
          (yank-target-set))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
(autoload 'browse-kill-ring "browse-kill-ring" nil t)
(autoload 'compile "compile" nil t)
(autoload 'file-template-auto-insert "file-template" nil t)
(autoload 'file-template-find-file-not-found-hook "file-template" nil t)
(autoload 'file-template-insert "file-template" nil t)
(autoload 'htmlize-region "htmlize" nil t)
(autoload 'json-mode "json-mode" nil t)
(autoload 'makefile-mode "make-mode" nil t)

(show-paren-mode t)
(delete-selection-mode t)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(winner-mode 1)

(setq-default Man-notify-method 'bully
              auto-hscroll-mode t ;;'current-line
              backup-inhibited t
              bidi-display-reordering nil
              blink-matching-paren-distance nil
              browse-kill-ring-display-duplicates nil
              browse-kill-ring-highlight-current-entry nil
              browse-kill-ring-maximum-display-length 400
              browse-kill-ring-no-duplicates t
              browse-kill-ring-separator "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
              browse-kill-ring-separator-face 'my-browse-kill-ring-separator-face
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
              custom-theme-directory (concat user-emacs-directory "themes")
              dabbrev-case-fold-search nil
              desktop-restore-frames nil
              display-line-numbers-grow-only t
              display-line-numbers-widen t
              display-line-numbers-width 3
              echo-keystrokes 0.1
              enable-local-eval t
              eval-expression-print-length nil
              eval-expression-print-level nil
              even-window-heights nil
              extended-command-suggest-shorter nil
              file-template-insert-automatically 'ask
              file-template-paths (list (concat user-emacs-directory "templates/"))
              fill-column 78
              flyspell-mode-map nil
              font-lock-maximum-decoration '((c++-mode . 2) (t . t))
              hi-lock-auto-select-face t
              highlight-changes-active-string " Chg+"
              highlight-changes-global-modes nil
              highlight-changes-passive-string " Chg-"
              hscroll-step 1
              htmlize-output-type 'font
              indent-tabs-mode nil
              indicate-buffer-boundaries t
              inhibit-startup-message t
              initial-major-mode 'emacs-lisp-mode
              initial-scratch-message ";; Scratch elisp buffer\n\n"
              kill-do-not-save-duplicates t
              kill-whole-line t
              large-file-warning-threshold nil
              line-move-visual t
              line-number-display-limit-width 1000
              line-number-mode t
              log-edit-hook nil
              lpr-command "lpr"
              lpr-lp-system t
              lpr-switches ""
              make-backup-files nil
              mouse-autoselect-window t
              mouse-highlight 1
              mouse-yank-at-point t
              parens-require-spaces nil
              pulse-flag t
              redisplay-dont-pause t
              scroll-conservatively 10000
              scroll-error-top-bottom t
              scroll-margin 3
              scroll-preserve-screen-position t
              shift-select-mode nil
              show-paren-delay 0
              split-width-threshold nil
              switch-to-buffer-preserve-window-point 'already-displayed
              truncate-lines t
              truncate-partial-width-windows nil
              user-mail-address (concat "<" (getenv "USER") "@microsoft.com>")
              visible-bell t
              warning-suppress-types (list '(undo discard-info))
              winner-boring-buffers (list "*Completions*" "*Help*" "*Apropos*" "*buffer-selection*")
              winner-ring-size 50)

(setq-default select-active-regions t ;; nil
              mouse-drag-copy-region t
              x-select-enable-primary t
              x-select-enable-clipboard nil)

(setq completion-ignored-extensions (delete ".bin" completion-ignored-extensions))
(add-to-list 'completion-ignored-extensions ".d")

(add-to-list 'auto-mode-alist '("Makefile.*\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.cfg\\'" . btext-mode))
(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.zsh.*\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))

;; Comments

(add-to-list 'comment-styles '(my-style t nil t t "My style"))
(setq-default comment-column 0
              comment-empty-lines t
              comment-fill-column 120
              comment-style 'my-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun my-ansi-color ()
  "ANSI colorize the buffer."
  (interactive)
  (save-restriction
    (widen)
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun my-apply-macro-to-region-lines (top bottom)
  "Like `apply-macro-to-region-lines' but works with offset into line, and
undoable all at once."
  (interactive "r")
  (when (null last-kbd-macro)
    (error "No keyboard macro has been defined"))
  (when (= top bottom)
    (setq bottom (save-excursion (re-search-forward "^\\s-*$" nil 'go) (point))))
  (save-excursion
    (with-no-undo-boundaries
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
        (set-marker next-line-marker nil)))))

(defun my-ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (save-excursion
    (let ((i -1))
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

(defun my-backward-regexp (regexp)
  "Skip lines backward containing a regexp."
  (interactive "sSkip lines backward containing regexp: ")
  (beginning-of-line)
  (forward-line -1)
  (while (and (not (bobp)) (re-search-forward regexp (line-end-position) t))
    (beginning-of-line)
    (forward-line -1)))

(defun my-backup ()
  "Create a backup (.keep) of a file."
  (interactive)
  (let* ((filename (buffer-file-name))
         (backups
          (directory-files
           (file-name-directory filename) t
          (concat (file-name-nondirectory filename) ".keep.[0-9]+")))
        (extension ".keep"))
    (when backups
      (setq extension (format ".keep.%d" (1+ (length backups)))))
    (copy-file filename (concat filename extension) t)
    (message
     (format "Backed up file to %s" (concat (file-name-nondirectory filename) extension)))))

(defun my-backup-set-mode ()
  "Set the mode of backup files to the mode of the original."
  (interactive)
  (let ((backup-regexp "\\.\\(keep\\|contrib\\)\\(\\.[0-9]+\\)?$") mode)
    (when (and (buffer-file-name) (string-match backup-regexp (buffer-file-name)))
      (let ((name (replace-regexp-in-string backup-regexp "" (buffer-file-name))))
        (setq mode (let ((case-fold-search t))
                     (assoc-default name auto-mode-alist 'string-match)))
        (when mode
          (set-auto-mode-0 mode))))))

(defun my-bounds-of-current-symbol ()
  "Return bounds of current symbol."
  (let (beg end)
    (save-excursion
      (unless (looking-at "\\sw\\|\\s_")
        (skip-syntax-backward "^w_"))
      (skip-syntax-forward "w_")
      (setq end (point))
      (skip-syntax-backward "w_")
      (skip-syntax-forward "^w")
      (setq beg (point)))
    (cons beg end)))

(defface my-browse-kill-ring-separator-face
  '((t :foreground "deeppink"))
  "Face to highlight kill-ring separators."
  :group 'faces)

(defvar my-kbd-macro-transient-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'my-call-last-kbd-macro)
    map)
  "Keymap used for keyboard macros.")

(defun my-call-last-kbd-macro (&optional rpt)
  "Like `call-last-kbd-macro', but undoable all at once."
  (interactive "P")
  (with-no-undo-boundaries
    (call-last-kbd-macro rpt))
  (set-transient-map my-kbd-macro-transient-map t))

(defun my-case-symbol (mode)
  "Change case of symbol.  MODE is 'upcase 'downcase or 'capitalize."
  (interactive "*S")
  (let* ((bounds (my-bounds-of-current-symbol))
         (beg (car bounds))
         (end (cdr bounds)))
    (cl-case mode
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

(defun my-compile ()
  "DTRT to 'compile' this file."
  (interactive)
  (cond ((and (boundp 'smerge-mode) smerge-mode)
         (call-interactively 'smerge-hydra/body))
        ((and (boundp 'flymake-mode) flymake-mode)
         (flymake-start))
        ((equal major-mode 'emacs-lisp-mode)
         (emacs-lisp-byte-compile))
        ((equal major-mode 'sv-mode)
         (call-interactively 'asic-compile))
        (t
         (call-interactively 'compile))))

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
          (replace-match (cdr repl) nil t))))))

(defun my-count-lines (&optional arg)
  "Count lines to next blank line, or with prefix arg count lines in region."
  (interactive "P")
  (if arg
      (let ((current-prefix-arg nil))
        (call-interactively 'count-words-region))
    (message "%d lines to end of paragraph"
             (count-lines (point-at-bol)
                          (save-excursion (forward-paragraph) (point-at-eol))))))

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

(defun my-display-line-numbers (&optional arg)
  "Turn on line numbers, with ARG do it globally."
  (interactive "P")
  (if arg
      (let ((current-prefix-arg nil))
        (call-interactively 'global-display-line-numbers-mode))
    (let ((current-prefix-arg nil))
      (call-interactively 'display-line-numbers-mode))))

(defun my-dos-coding-system ()
  "Change buffer coding system to DOS."
  (interactive)
  (let ((coding-system-for-read 'utf-8-dos))
    (revert-buffer nil t)))

(defun my-dos2unix ()
  "Remove ^M's from file."
  (interactive "*")
  (set-buffer-file-coding-system 'unix t)
  (goto-char (point-min))
  (while (search-forward (string ?\C-m) nil t)
    (replace-match ""))
  (goto-char (point-min)))

;; (defun my-edebug-remove-instrumentation ()
;;   "Remove Edebug instrumentation from all functions."
;;   (interactive)
;;   (let ((functions nil))
;;     (mapatoms
;;      (lambda (symbol)
;;        (when (and (functionp symbol)
;;                   (get symbol 'edebug))
;;          (let ((unwrapped (edebug-unwrap* (symbol-function symbol))))
;;            (unless (equal unwrapped (symbol-function symbol))
;;              (push symbol functions)
;;              (setf (symbol-function symbol) unwrapped)))))
;;      obarray)
;;     (if (not functions)
;;         (message "Found no functions to remove instrumentation from")
;;       (message "Remove edebug instrumentation from %s"
;;                (mapconcat #'symbol-name functions ", ")))))

(defun my-editor-mode ()
  "For use when invoked as $EDITOR."
  (setq-local mode-name "EDITOR")
  (defun my-editor-mode-done ()
    "Save and exit."
    (interactive)
    (let ((confirm-kill-emacs nil))
      (save-buffers-kill-emacs t)))
  (local-set-key (kbd "C-c C-c") 'my-editor-mode-done))

(defun my-edit-region-in-mode (start end mode-name)
  "Edit region in some other mode."
  (interactive "r\nsMode? ")
  (let* ((buf (clone-indirect-buffer nil nil))
         (mode (intern-soft (concat mode-name "-mode"))))
    (unless mode
      (error (concat "No mode named '" mode-name "-mode.")))
    (with-current-buffer buf
      (narrow-to-region start end)
      (goto-char (point-min))
      (funcall mode)
      (switch-to-buffer buf))))

(defun my-fill (&optional arg)
  "Fill paragraph, or region with prefix arg."
  (interactive "*P")
  (if arg
      (fill-region (region-beginning) (region-end))
    (fill-paragraph nil)))

(defun my-fixme-next (&optional arg)
  "Go to next FIXME, or previous with prefix arg."
  (interactive "P")
  (let ((case-fold-search nil) (pos (point)))
    (if arg
        (unless (re-search-backward "\\bFIXME\\b" nil t)
          (message "No previous FIXMEs"))
      (when (looking-at "\\bFIXME\\b")
        (forward-char 5))
      (if (re-search-forward "\\bFIXME\\b" nil t)
          (backward-char 5)
        (goto-char pos)
        (message "No more FIXMEs")))))

  (defun my-forward-paragraph ()
  "Move to next blank line after some text."
  (interactive)
  (beginning-of-line)
  (while (and (looking-at "^\\s-*$") (not (eobp)))
    (forward-line))
  (re-search-forward "^\\s-*$" nil 'go))

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
          (goto-char (point-min))
          (forward-line (1- (string-to-number line))))
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
            (insert-char char (- 80 (- (point) (point-at-bol)) (length comment-end)))
            (insert comment-end))
        (end-of-line)
        (insert "  " comment-start)
        (delete-horizontal-space)
        (insert " ")
        (when (> (length comment-end) 0)
          (save-excursion
            (insert " " comment-end)))))))

(defun my-kill-frame-or-emacs ()
  "Kill a frame or emacs"
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-emacs)))

(defun my-kill-results-buffer ()
  "Kill a *special* or cloned buffer in a second window."
  (interactive)
  (when (> (count-windows) 1)
    (let ((regexp (concat "\\`" (regexp-opt '("*scratch" "*SPEEDBAR" "*sqlplus"))))
          (windows (window-list-1 (next-window)))
          buf)
      (catch 'done
        (dolist (win windows)
          (setq buf (window-buffer win))
          (when (and (string-match "\\`\\*\\|.+<[0-9]+>\\'" (buffer-name buf))
                     (not (string-match regexp (buffer-name buf))))
            (delete-windows-on buf)
            (kill-buffer buf)
            (throw 'done t)))))))

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

(defun my-macroexpand (sexp)
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

(defun my-minibuffer-backward ()
  "Move backward words or path elements in the minibuffer."
  (interactive)
  (unless (looking-back "[a-zA-Z0-9_.-]" (point-at-bol))
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

(defun my-minibuffer-regexp-quote ()
  "Regexp quote the current minibuffer contents."
  (interactive)
  (let ((new-input (regexp-quote (minibuffer-contents))))
    (delete-minibuffer-contents)
    (insert new-input)))

(defun my-narrow (&optional arg)
  "Narrow to region, or widen if already narrowed, or with prefix
arg do something special."
  (interactive "P")
  (if (/= (buffer-size) (- (point-max) (point-min)))
      (widen)
    (if (null arg)
        (narrow-to-region (region-beginning) (region-end))
      (cond ((equal major-mode 'sv-mode)
             (sv-mode-narrow-to-scope))
            (t
             (narrow-to-defun))))
    (my-mode-line-count-lines)
    (goto-char (point-min))))

(defvar my-paste-mode nil)
(add-to-list 'minor-mode-alist `(my-paste-mode ,(propertize " PASTE " 'face 'warning)))
(defun my-paste-mode ()
  (interactive)
  (set-mark (point))
  (let ((buf (current-buffer))
        (my-paste-mode t))
    (with-temp-buffer
      (let ((stay t)
            (text (current-buffer)))
        (redisplay)
        (while stay
          (let ((char (let ((inhibit-redisplay t)) (read-event nil t 0.1))))
            (unless char
              (with-current-buffer buf (insert-buffer-substring text))
              (erase-buffer)
              (redisplay)
              (setq char (read-event nil t)))
            (cond
             ((or (eq char ?\C-j)
                  (eq char ?\C-m))
              (insert ?\n))
             ((eq char ?\C-i)
              (insert ?\t))
             ((< 31 char 127)
              (insert char))
             ((> char 127)
              nil)
             (t
              (push char unread-command-events)
              (setq stay nil)))))
        (insert-buffer-substring text)))))

(defun my-put-file-name-on-clipboard (&optional arg)
  "Put the current file name in the kill-ring.
If running inside a tmux session, it will also be put in a tmux copy-buffer
and copied through iTerm2 to clipboard."
  (interactive "P")
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (when arg
        (setq filename (file-name-nondirectory filename)))
      (kill-new filename)
      (when (getenv "TMUX")
        (require 'my-tmux)
        (my-tmux-copy-text filename)
        (my-tmux-term-copy-text filename))
      (message filename))))

(defun my-prettify ()
  "Remove trailing space, untabify, reindent."
  (interactive "*")
  (my-delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defvar my-recenter-count nil)
(defun my-recenter (&optional arg)
  "Recenter middle/high/low."
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
               (recenter 0)
               (setq my-recenter-count 1))))
          ((= my-recenter-count 1)
           (recenter 0))
          (t
           (recenter -1)))
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
          (deactivate-mark)
          (query-replace (nth 0 common) (nth 1 common) nil start end))
      (let (from to)
        (setq from (buffer-substring-no-properties (region-beginning) (region-end))
              to (read-from-minibuffer
                  (format "Query replace %s with: " from) nil nil nil
                  'query-replace-history))
        (goto-char (region-beginning))
        (deactivate-mark)
        (query-replace from to)
        (push (cons from to) query-replace-defaults)))))

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

(defun my-scratch-create ()
  "Recreate the *scratch* buffer."
  (interactive)
  (get-buffer-create "*scratch*")
  (switch-to-buffer "*scratch*")
  (erase-buffer)
  (insert initial-scratch-message)
  (emacs-lisp-mode)
  (set-buffer-modified-p nil))

(defun my-set-register (&optional arg)
  "Copy last kill, or with prefix arg region, to a register."
  (interactive "P")
  (if arg
      (set-register (register-read-with-preview "(Region) Set register:")
                    (buffer-substring (region-beginning) (region-end)))
    (set-register (register-read-with-preview "(Last kill) Set register:") (current-kill 0 t))))

;; (defun my-style-symbol-toggle ()
;;   "Toggle style of symbol between camelCase and snake_case."
;;   (interactive "*")
;;   (save-excursion
;;     (let* ((bounds (my-bounds-of-current-symbol))
;;            (beg (car bounds))
;;            (end (cdr bounds)))
;;       (with-no-undo-boundaries
;;         (if (progn (goto-char beg) (search-forward "_" end t))
;;             (progn
;;               (upcase-initials-region beg end)
;;               (replace-string "_" "" nil beg end)
;;               (downcase-region beg (1+ beg)))
;;           (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ beg) end)
;;           (setq bounds (my-bounds-of-current-symbol))
;;           (downcase-region (car bounds) (cdr bounds)))))))

(defun my-suspend-emacs (&optional arg)
  "Suspend emacs.  With prefix arg, cd to current-directory."
  (interactive "P")
  (let (dir)
    (when arg
      (setq dir (if (eq major-mode 'dired-mode)
                    (dired-current-directory)
                  (and (buffer-file-name)
                       (file-name-directory (buffer-file-name))))))
    (suspend-emacs (and dir (concat "cd " dir)))))

(defun my-tab ()
  "Special TAB key behavior"
  (interactive)
  (let ((prev-eol (point-at-eol)))
    (call-interactively 'indent-according-to-mode)
    (when (and (= prev-eol (point-at-eol))
               (member (following-char) (list ?\" ?\' ?\` ?\) ?\] ?\} ?\>)))
      (skip-chars-forward "])}>\"'`"))))
;; Use global-set-key so minor modes can override
(global-set-key (kbd "TAB") 'my-tab)

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

(defun my-theme ()
  "Load a theme."
  (interactive)
  (let (themes name choice)
    (dolist (theme (custom-available-themes))
      (setq name (symbol-name theme))
      (when (and (string-match "^smf-" name)
                 (not (string-match "^smf-base" name)))
        (push name themes)))
    (setq choice (intern (ido-completing-read "Theme: " (sort themes 'string<) nil t)))
    (my-theme-disable-all)
    (load-theme choice t)))

(defun my-theme-disable-all ()
  "Disable all custom themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun my-theme-dark ()
  "Load my dark theme."
  (interactive)
  (my-theme-disable-all)
  (load-theme 'smf-misterioso t))

(defun my-theme-light ()
  "Load my light theme."
  (interactive)
  (my-theme-disable-all)
  (load-theme 'smf-light t))

(defun my-todo-next (&optional arg)
  "Go to next TODO, or previous with prefix arg."
  (interactive "P")
  (let ((case-fold-search nil) (pos (point)))
    (if arg
        (unless (re-search-backward "\\bTODO\\b" nil t)
          (message "No previous TODOs"))
      (when (looking-at "\\bTODO\\b")
        (forward-char 4))
      (if (re-search-forward "\\bTODO\\b" nil t)
          (backward-char 4)
        (goto-char pos)
        (message "No more TODOs")))))

(defun my-toggle-buffer-modified ()
  "Toggle buffer modified/unmodified."
  (interactive)
  (set-buffer-modified-p (not (buffer-modified-p))))

(defun my-toggle-window-split ()
  "Toggle between horizontal/vertical split.
Only works if there are exactly two windows."
  (interactive)
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
    (delete-window (next-window))
    (let ((first-win (selected-window)))
      (funcall splitter)
      (when this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (when this-win-2nd (other-window 1)))))

(defun my-transpose-sexps ()
  "If point is after certain chars transpose chunks around that.
Otherwise transpose sexps."
  (interactive "*")
  (if (not (looking-back "[,]\\s-*" (point-at-bol)))
      (progn (transpose-sexps 1) (forward-sexp -1))
    (let ((beg (point)) end rhs lhs)
      (while (and (not (eobp))
                  (not (looking-at "\\s-*\\([,]\\|\\s)\\)")))
        (forward-sexp 1))
      (setq rhs (buffer-substring beg (point)))
      (delete-region beg (point))
      (re-search-backward "[,]\\s-*" nil t)
      (setq beg (point))
      (while (and (not (bobp))
                  (not (looking-back "\\([,]\\|\\s(\\)\\s-*" (point-at-bol))))
        (forward-sexp -1))
      (setq lhs (buffer-substring beg (point)))
      (delete-region beg (point))
      (insert rhs)
      (re-search-forward "[,]\\s-*" nil t)
      (save-excursion
        (insert lhs)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice

(defadvice kill-buffer (around my-kill-buffer-advice activate)
  "Don't kill the *scratch* buffer."
  (if (equal (ad-get-arg 0) "*scratch*")
      (bury-buffer)
    ad-do-it))

(eval-when-compile (require 'pulse))
(defun my-pulse-momentary-highlight-region (orig-fun start end &optional face)
  "If pulsing enabled but not available, blink it instead."
  (if (and pulse-flag (not (pulse-available-p)))
      (let ((ov (make-overlay start end))
            (idx 0))
        (while (< idx 3)
          (overlay-put ov 'face face)
          (sit-for 0.3)
          (overlay-put ov 'face nil)
          (sit-for 0.2)
          (setq idx (1+ idx)))
        (delete-overlay ov))
    (apply orig-fun start end face)))

(advice-add 'pulse-momentary-highlight-region :around #'my-pulse-momentary-highlight-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(defun my-after-save-hook ()
  (executable-make-buffer-file-executable-if-script-p))

(defun my-emacs-lisp-mode-hook ()
  (setq comment-column 0)
  (local-set-key (kbd "C-x M-e") 'pp-macroexpand-last-sexp)
  (add-to-list 'imenu-generic-expression
               '("Require"
                 "^(require +'\\(\\_<.+\\_>\\)" 1))
  (add-to-list 'imenu-generic-expression
               '("Use Package"
                 "^(use-package +\\(\\_<.+\\_>\\)" 1)))

(defun my-find-file-hook ()
  (when (or (equal (buffer-name) "sim.log")
            (equal (buffer-name) "topology.txt"))
    (my-word-wrap-on-hook))
  (my-backup-set-mode))

(defun my-minibuffer-ido-insert-bookmark-dir (&optional arg)
  "Insert a bookmarked dir using ido."
  (interactive "P")
  (let* ((enable-recursive-minibuffers t)
         (dir (my-ido-get-bookmark-dir)))
    (when dir
      (insert dir))))

(defun my-minibuffer-setup-hook ()
  (override-global-mode -1)
  (show-mark-mode 0)
  (local-set-key (kbd "C-/") 'dabbrev-expand)
  (local-set-key (kbd "C-\\") 'expand-abbrev)
  (local-set-key (kbd "C-_") 'dabbrev-expand)
  (local-set-key (kbd "C-w") 'my-minibuffer-insert-word-after-point)
  (local-set-key (kbd "C-x D") 'my-fzf-any-local-directory)
  (local-set-key (kbd "C-x F") 'my-fzf-any-local-file)
  (local-set-key (kbd "C-x W") 'my-fzf-any-project-file)
  (local-set-key (kbd "C-x d") 'my-fzf-any-project-directory)
  (local-set-key (kbd "C-x f") 'my-fzf-local-file)
  (local-set-key (kbd "C-x w") 'my-fzf-project-file)
  (local-set-key (kbd "C-z") 'undo)
  (local-set-key (kbd "M-$") 'my-minibuffer-ido-insert-bookmark-dir)
  (local-set-key (kbd "M-\\") 'completion-at-point)
  (local-set-key (kbd "M-h") 'my-minibuffer-backward)
  (local-set-key (kbd "M-j") 'my-minibuffer-backward-kill)
  (local-set-key (kbd "M-k") 'my-minibuffer-forward-kill)
  (local-set-key (kbd "M-l") 'my-minibuffer-forward)
  (local-set-key (kbd "M-r") 'my-minibuffer-regexp-quote)
  (local-set-key (kbd "M-w") 'my-minibuffer-insert-region)
  (local-set-key (kbd "M-z") (lambda () (interactive) nil))
  (local-set-key (kbd "M-~") 'my-minibuffer-ido-insert-bookmark-dir))

(defface my-next-error-face
  '((t (:underline t)))
  "Face to highlight current 'error'."
  :group 'faces)
(defvar my-next-error-overlay nil)
(defun my-next-error-hook ()
  (when next-error-last-buffer
    (with-current-buffer next-error-last-buffer
      (when my-next-error-overlay
        (delete-overlay my-next-error-overlay))
      (setq my-next-error-overlay (make-overlay (point-at-bol) (point-at-eol)))
      (overlay-put my-next-error-overlay 'face 'my-next-error-face))))

(defun my-prog-mode-hook ()
  (highlight-indent-guides-mode 1))

(defun my-sh-mode-hook ()
  (when (and buffer-file-name (string-match "\\.zsh.*\\'" buffer-file-name))
    (sh-set-shell "zsh")))

(defun my-word-wrap-on-hook ()
  (setq truncate-lines nil)
  (setq word-wrap t))

(add-hook 'after-save-hook 'my-after-save-hook)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'find-file-hook 'my-find-file-hook)
(add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
(add-hook 'next-error-hook 'my-next-error-hook)
(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(add-hook 'sh-mode-hook 'my-sh-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; After loads

(eval-after-load "compile"
  '(progn
     (defun my-compilation-mode-hook ()
       (define-key compilation-mode-map "{" 'compilation-previous-file)
       (define-key compilation-mode-map "}" 'compilation-next-file)
       (setq truncate-lines nil)
       (goto-char (point-max)))
     (add-hook 'compilation-mode-hook 'my-compilation-mode-hook)
     (setq-default compilation-mode-line-errors
                   '(" ["
                     (:eval (if (> compilation-num-errors-found 0)
                                (list (int-to-string compilation-num-errors-found) 'face 'error)
                              (list "0" 'face 'default)))
                     " "
                     (:eval (if (> compilation-num-warnings-found 0)
                                (list (int-to-string compilation-num-warnings-found) 'face 'warning)
                              (list "0" 'face 'default)))
                     " "
                     (:eval (if (> compilation-num-infos-found 0)
                                (list (int-to-string compilation-num-infos-found) 'face 'caution)
                              (list "0" 'face 'default)))
                     "]"))
     ))

(defvar file-template-mapping-alist)
(eval-after-load "file-template"
  '(progn
     (add-to-list 'file-template-mapping-alist '("\\.html?$" . "template.html"))
     (add-to-list 'file-template-mapping-alist '("\\.sh$" . "template.sh"))
     (add-to-list 'file-template-mapping-alist '("\\.sv$" . "template.sv"))
     (add-to-list 'file-template-mapping-alist '("\\.svh$" . "template.svh"))
     (add-to-list 'file-template-mapping-alist '("\\.v$" . "template.v"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(bind-keys*
 ("<delete>"    . delete-char)
 ("<f1>"        . flymake-goto-prev-error)
 ("<f2>"        . flymake-goto-next-error)
 ("<f3>"        . my-fixme-next)
 ("<f4>"        . my-todo-next)
 ("C-/"         . dabbrev-expand)
 ("C-M-h"       . backward-sexp)
 ("C-M-k"       . delete-region)
 ("C-M-l"       . forward-sexp)
 ("C-M-t"       . my-transpose-sexps)
 ("C-M-y"       . browse-kill-ring)
 ("C-c $"       . my-delete-trailing-whitespace)
 ("C-c ."       . my-kill-results-buffer)
 ("C-c ;"       . my-line-comment)
 ("C-c C"       . my-comment-region-after-copy)
 ("C-c N"       . narrow-to-defun)
 ("C-c T"       . tidy)
 ("C-c TAB"     . indent-region)
 ("C-c U"       . (lambda () (interactive) (my-case-symbol 'upcase)))
 ("C-c W"       . winner-redo)
 ("C-c c"       . my-comment-or-uncomment-region)
 ("C-c i"       . (lambda () "Insert register" (interactive) (let ((current-prefix-arg '(4))) (call-interactively 'insert-register))))
 ("C-c l"       . (lambda () (interactive) (my-case-symbol 'downcase)))
 ("C-c m"       . my-compile)
 ("C-c n"       . my-narrow)
 ("C-c p"       . my-paste-mode)
 ("C-c q"       . bury-buffer)
 ("C-c r"       . revert-buffer)
 ("C-c s"       . my-set-register)
 ("C-c t"       . my-tidy-lines)
 ("C-c u"       . (lambda () (interactive) (my-case-symbol 'capitalize)))
 ("C-c v"       . toggle-truncate-lines)
 ("C-c w"       . winner-undo)
 ("C-d"         . delete-forward-char)
 ("C-h"         . backward-char)
 ("C-l"         . forward-char)
 ("C-x 5 n"     . set-frame-name)
 ("C-x #"       . my-display-line-numbers)
 ("C-x %"       . my-toggle-window-split)
 ("C-x ("       . kmacro-start-macro-or-insert-counter)
 ("C-x -"       . my-win-split-vertically)
 ("C-x C-c"     . my-kill-frame-or-emacs)
 ("C-x C-n"     . other-window)
 ("C-x C-p"     . (lambda () (interactive (other-window -1))))
 ("C-x C-z"     . (lambda () (interactive) (ding)))
 ("C-x E"       . my-apply-macro-to-region-lines)
 ("C-x K"       . kill-buffer-and-window)
 ("C-x M-q"     . my-toggle-buffer-modified)
 ("C-x S"       . (lambda () "Shrink other window." (interactive) (shrink-window-if-larger-than-buffer (next-window))))
 ("C-x SPC"     . fixup-whitespace)
 ("C-x _"       . my-win-split-vertically-small)
 ("C-x e"       . my-call-last-kbd-macro)
 ("C-x k"       . kill-buffer)
 ("C-x s"       . shrink-window-if-larger-than-buffer)
 ("C-x t"       . task-map)
 ("C-x |"       . my-win-split-horizontally)
 ("C-z"         . undo)
 ("ESC <left>"  . (lambda () "Select previous frame." (interactive) (other-frame 1)))
 ("ESC <right>" . (lambda () "Select next frame." (interactive) (other-frame -1)))
 ("M-!"         . my-shell-command-on-current-file)
 ("M-%"         . my-query-replace)
 ("M-="         . my-count-lines)
 ("M-N"         . scroll-up-command)
 ("M-P"         . scroll-down-command)
 ("M-Q"         . my-unfill)
 ("M-]"         . my-forward-paragraph)
 ("M-`"         . next-error)
 ("M-g"         . my-goto-line-column)
 ("M-q"         . my-fill)
 ("M-u"         . my-recenter)
 ("M-z"         . redo)
 ("M-~"         . previous-error))

;; These have to be in this order

(bind-key* "C-c h" 'help-command)
(bind-key* "C-c h a" 'apropos)
(bind-key* "C-c h I" 'info-apropos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Work setup

(require 'project)
(defun my-project-find (dir)
  (when-let ((ws (getenv "WORKSPACE")))
    (cons 'vc ws)))
(add-to-list 'project-find-functions #'my-project-find)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases

(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'bre 'my-backward-regexp)
(defalias 'colors 'list-colors-display)
(defalias 'dark 'my-theme-dark)
(defalias 'dos 'my-dos-coding-system)
(defalias 'edbg 'edebug-defun)
(defalias 'file 'my-put-file-name-on-clipboard)
(defalias 'fl 'font-lock-fontify-buffer)
(defalias 'fnd 'my-dired-find-name-dired)
(defalias 'fre 'my-forward-regexp)
(defalias 'ind 'my-indent)
(defalias 'init (lambda () (interactive) (require 'use-package) (find-file (concat user-emacs-directory "lisp/src/my-init.el"))))
(defalias 'light 'my-theme-light)
(defalias 'qrr 'vr/query-replace)
(defalias 'ren 'rename-buffer)
(defalias 'rot 'my-rotate-window-buffers)
(defalias 'sb 'sr-speedbar-toggle)
(defalias 'serve 'my-server)
(defalias 'tail 'auto-revert-tail-mode)
(defalias 'tdoe 'toggle-debug-on-error)
(defalias 'theme 'my-theme)
(defalias 'theme-off 'my-theme-disable-all)
(defalias 'tmux (lambda () (interactive) (find-file (expand-file-name "~/.tmux.history"))))
(defalias 'uniq 'my-delete-duplicate-lines)
(defalias 'unt 'my-untabity)
(defalias 'work (lambda () (interactive) (find-file (expand-file-name "/home/scfrazer/notes/todo.md"))))
(defalias 'ws 'whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System setup

(unless window-system

  (set-terminal-coding-system 'utf-8)

  (defface my-display-table-face
    '((((class color) (background dark)) :foreground "yellow2" :background "gray33")
      (((class color) (background light)) :foreground "violetred" :background "whitesmoke"))
    "Face for terminal truncation/wrapping glyphs."
    :group 'faces)

  (let ((truncation-glyph (make-glyph-code ?⇄ 'my-display-table-face))
        (wrap-glyph (make-glyph-code ?⤸ 'my-display-table-face))
        (escape-glyph (make-glyph-code ?\\ 'my-display-table-face))
        (control-glyph (make-glyph-code ?^ 'my-display-table-face))
        (vertical-border-glyph (make-glyph-code ?│ 'my-display-table-face)))
    (set-display-table-slot standard-display-table 'truncation truncation-glyph)
    (set-display-table-slot standard-display-table 'wrap wrap-glyph)
    (set-display-table-slot standard-display-table 'escape escape-glyph)
    (set-display-table-slot standard-display-table 'control control-glyph)
    (set-display-table-slot standard-display-table 'vertical-border vertical-border-glyph))

  (bind-keys* ("C-M-z" . my-suspend-emacs)
              ("C-_"   . dabbrev-expand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS-specific setup

(let ((extra-config (concat user-emacs-directory (symbol-name system-type) ".el")))
  (when (file-exists-p extra-config)
    (load-file extra-config)))

(let ((confidential (concat user-emacs-directory "lisp/confidential.elc")))
  (when (file-exists-p confidential)
    (load-file confidential)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finish up

(require 'btext-mode)

;; Disabled commands

(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;; Done

(provide 'my-init)
