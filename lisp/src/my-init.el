;; my-init.el

(setq vc-follow-symlinks t)

(defvar my-location
  (let ((hostname (or (getenv "HOSTNAME")
                      (getenv "ABBREV_HOSTNAME"))))
    (and (stringp hostname)
         (cond
          ((string-match "^asic-vm-rtp" hostname) 'RTP)
          ((string-match "^asic-vm-sjc" hostname) 'SJC)
          ((string-match "^asic-vm-bgl" hostname) 'BGL)
          ((string-match "^SCFRAZER" hostname) 'Work)))))

;; Need these first

(require 'cl-lib)

(require 'my-font-lock)
(defalias 'fl 'font-lock-fontify-buffer)
(defalias 'ws 'whitespace-mode)
(setq-default my-font-lock-auto-whitespace t)

(require 'bind-key)
(require 'bind-remind)

(require 'my-dired)
(require 'dired-narrow)
(bind-key* "M-d" 'my-dired-pop-to-or-create)
(unbind-key "C-o" dired-mode-map)
(unbind-key "s"   dired-mode-map)
(bind-keys :map dired-mode-map
           ("/"        . dired-narrow)
           ("SPC"      . my-dired-toggle-mark)
           ("<return>" . my-dired-open)
           ("J"        . my-dired-jump-to-prev-dir)
           ("M-<"      . my-dired-beginning-of-buffer)
           ("M->"      . my-dired-end-of-buffer)
           ("RET"      . my-dired-open)
           ("b"        . my-dired-toggle-path)
           ("j"        . my-dired-jump-to-dir)
           ("n"        . my-dired-next-line)
           ("o"        . my-dired-do-find-file)
           ("p"        . my-dired-previous-line)
           ("s h"      . dired-hide-subdir)
           ("s i"      . dired-maybe-insert-subdir)
           ("s k"      . dired-kill-subdir)
           ("u"        . my-dired-up-dir))

;; Required packages

(require 'my-abbrev)
(bind-keys* ("C-\\" . expand-abbrev))
(setq save-abbrevs nil)

(require 'my-bookmark)

(require 'my-buf)
(bind-keys* ("C-o". my-buf-toggle))

(require 'my-clearcase)
(when (boundp 'clearcase-prefix-map)
  (bind-key "e" 'my-clearcase-edcs-edit clearcase-prefix-map))

(require 'my-edit)
(bind-keys* ("C-M-n" . my-edit-scroll-down)
            ("C-M-p" . my-edit-scroll-up)
            ("C-M-v" . my-edit-newline-and-indent-around)
            ("C-c j" . my-edit-join-line-with-next)
            ("C-k"   . my-edit-kill-line)
            ("C-v"   . my-edit-newline-and-indent)
            ("M-RET" . my-edit-newline-and-indent-above))

(require 'my-ido)
(bind-keys* ("C-c b"   . my-ido-insert-bookmark-dir)
            ("C-x C-r" . my-ido-recentf-file)
            ("M-i"     . ido-switch-buffer))

(require 'my-jump-marker)
;; (bind-keys* ("M-SPC" . my-jump-marker))

(require 'jump-to-prev-pos)
(bind-keys* ("M-b" . jump-to-prev-pos))

(require 'goto-chg)
(bind-keys* ("M-SPC" . goto-last-change))

(require 'mode-fn)
(mode-fn-map 'html 'org-mode 'org-export-as-html)
(mode-fn-map 'tidy 'c++-mode 'my-cc-mode-uncrustify)
(mode-fn-map 'tidy 'cperl-mode 'my-perl-tidy)
(mode-fn-map 'tidy 'java-mode 'my-java-mode-uncrustify)
(mode-fn-map 'tidy 'js2-mode 'web-beautify-js)
(mode-fn-map 'tidy 'php-mode 'my-php-tidy)
(mode-fn-map 'tidy 'web-mode 'web-beautify-html)

(require 'my-mode-line)

(require 'my-occur)
(bind-keys* ("M-s O" . my-multi-occur)
            ("M-s o" . my-occur))

(require 'my-recentf)

(require 'redo+)

(require 'show-mark)
(global-show-mark-mode 1)

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
          ("C-c a" . my-align))
  :config
  (progn
    (defun my-align ()
      "Align to an entered char."
      (interactive "*")
      (let ((regexp (concat "\\(\\s-*\\)" (regexp-quote (char-to-string (read-char "Align to char:"))))))
        (save-excursion
          (align-regexp (region-beginning) (region-end) regexp 1 align-default-spacing))))))

(use-package avy
  :bind* (("C-j" . my-avy-goto))
  :config
  (progn
    (setq avy-keys (nconc (number-sequence ?a ?z) (number-sequence ?A ?Z))
          avy-all-windows nil
          avy-case-fold-search nil
          avy-style 'at)
    (defun my-avy-goto (char)
      "Jump to CHAR at a word start, or any char if C-k, or BOL if C-l, or EOL if C-m."
      (interactive (list (read-char "Char: ")))
      (if (= 11 char)
          (call-interactively 'avy-goto-char)
        (if (= 12 char)
            (call-interactively 'avy-goto-line)
          (if (and (not (< 31 char 127))
                   (not (= 13 char)))
              (error "Unknown char")
            (avy-with avy-goto-word-1
              (let* ((str (string char))
                     (regex (cond ((= 13 char)
                                   "\n")
                                  ((= 32 char)
                                   "\\s-+")
                                  ((string= str ".")
                                   "\\.")
                                  ((and avy-word-punc-regexp
                                        (string-match avy-word-punc-regexp str))
                                   (regexp-quote str))
                                  ((< 64 char 91)
                                   (concat "\\b" str "\\|" str "[a-z0-9]"))
                                  (t
                                   (concat "\\b" str)))))
                (if (eq major-mode 'php-mode)
                    (let ((table (copy-syntax-table (syntax-table))))
                      (modify-syntax-entry ?$ "." table)
                      (with-syntax-table table
                        (avy--generic-jump regex nil avy-style)))
                  (avy--generic-jump regex nil avy-style))))))))))

(use-package ag2
  :bind* (("M-s G" . ag2)
          ("M-s g" . ag2-local))
  :config
  (progn
    (require 'my-grep-ed)
    (setq ag2-default-literal t
          ag2-files-aliases-alist '(("dv" . "\\.(sv|svh|cpp|hpp)$")
                                    ("rtl" . "\\.(s|v|vh)$")
                                    ("vtt" . "\\.(java|php|json|html|js)$")))
    (bind-key "C-x C-q" 'grep-ed-start ag2-mode-map)))

(use-package bm
  :bind* (("M-#" . my-bm-toggle-or-show)
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

(use-package cperl-mode
  :defer t
  :init
  (defalias 'perl-mode 'cperl-mode)
  :config
  (require 'my-perl))

(use-package csh-mode
  :mode (("\\.csh\\'" . csh-mode)
         ("\\.cshrc\\'" . csh-mode))
  :init
  ;; Add csh to magic interpreter modes
 (add-to-list 'interpreter-mode-alist '("csh" . csh-mode)))

(use-package diff-mode
  :defer t
  :defines (diff-mode-shared-map)
  :config
  (progn
    (setq diff-default-read-only t
          diff-switches "-b -u")
    (bind-keys :map diff-mode-shared-map
               ("q" . my-kill-this-buffer)
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

(use-package ediff
  :bind* ("C-c =" . my-ediff-dwim)
  :commands (ediff-buffers ediff-files)
  :init
  (defalias 'eb 'ediff-buffers)
  :config
  (require 'my-ediff))

(use-package etags
  :bind* (("M-?" . my-find-tag)
          ("M-&" . my-pop-tag-mark-kill-buffer)
          ("M-*" . my-pop-tag-mark))
  :config
  (progn
    (require 'etags-select)
    (require 'etags-table)
    (let ((tags-base-dir (cond
                          ((eq my-location 'RTP) "/auto/luke_user5/scfrazer/tags")
                          ((eq my-location 'SJC) "/auto/cppfs3a/scfrazer/tags")
                          ((eq my-location 'BGL) "/auto/sse-dump-blr/scfrazer/tags"))))
      (setq etags-select-use-short-name-completion t
            etags-table-alist
            (list
             `(".*\\.svh?$" ,(concat tags-base-dir "/sv/TAGS"))
             `("/vob/sse/asic/shared/models/PCIE/expertio_PCIE/PCIE/.*" ,(concat tags-base-dir "/sv/TAGS"))
             `("/vob/sse/asic/.*\\.[ch]pp$" ,(concat tags-base-dir "/cpp/TAGS")))
            etags-table-search-up-depth 10
            tags-revert-without-query t))
    (defun my-find-tag (&optional arg)
      "Find tag at point or plain find tag"
      (interactive "P")
      (if arg (etags-select-find-tag) (etags-select-find-tag-at-point)))
    (defun my-pop-tag-mark ()
      "Pop tag mark."
      (interactive)
      (pop-tag-mark))
    (defun my-pop-tag-mark-kill-buffer ()
      "Pop tag mark and kill previous buffer."
      (interactive)
      (let ((buf (current-buffer)))
        (my-pop-tag-mark)
        (unless (equal buf (current-buffer))
          (kill-buffer buf))))))

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

(use-package flymake
 :bind* (("C-x f" . flymake-start-syntax-check))
 :commands (my-flymake-goto-next-error my-flymake-goto-prev-error)
 :init
 (defalias 'fly 'flymake-mode)
 :config
 (require 'my-flymake))

(use-package git-timemachine
  :config
  (setq git-timemachine-abbreviation-length 7))

(use-package grep
  :bind* (("C-c G" . my-rgrep)
          ("C-c g" . my-lgrep))
  :config
  (progn
    (require 'my-grep)
    (require 'my-grep-ed)
    (bind-key "q" 'my-kill-results-buffer grep-mode-map)))

(use-package hide-region
  :bind* ("C-x C-h" . hide-region-toggle))

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

(use-package my-isearch
  :bind* (("C-r" . my-isearch-backward)
          ("C-s" . my-isearch-forward))
  :config
  (progn
    (setq isearch-allow-scroll t
          lazy-highlight-initial-delay 0)
    (put 'my-recenter 'isearch-scroll t)))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :config
  (require 'my-js2-mode))

(use-package less-css-mode
  :mode (("\\.less\\'" . less-css-mode))
  :config
  (progn
    (require 'flymake-easy)
    (require 'flymake-less)
    (setq-default less-css-compile-at-save t)
    (add-hook 'less-css-mode-hook 'flymake-less-load)))

(use-package linum
  :commands (linum-mode)
  :config
  (progn
    (defvar my-linum-format "%4d ")
    (defun my-linum-before-numbering-hook ()
      "Set the linum format."
      (setq-local my-linum-format
                  (let ((width (length (number-to-string
                                        (count-lines (point-min) (point-max))))))
                    (concat "%" (number-to-string width) "d "))))
    (add-hook 'linum-before-numbering-hook 'my-linum-before-numbering-hook)
    (defun my-linum-format-func (line)
      "Format the line number."
      (propertize (format my-linum-format line) 'face 'linum))
    (setq linum-format 'my-linum-format-func)))

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
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

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

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)))

(use-package quick-edit
  :bind* (("C-f" . qe-unit-move)
          ("C-w" . qe-unit-kill)
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
          ("M-w" . qe-unit-copy)))

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :config
  (require 'my-org))

(use-package my-pair
  :bind* (("C-c ("  . my-pair-open-paren-dwim)
          ("C-c )"  . my-pair-close-paren-dwim)
          ("C-c ["  . my-pair-open-paren-dwim)
          ("C-c ]"  . my-pair-close-paren-dwim)
          ("C-c {"  . my-pair-open-paren-dwim)
          ("C-c }"  . my-pair-close-paren-dwim)
          ("C-c <"  . my-pair-open-paren-dwim)
          ("C-c >"  . my-pair-close-paren-dwim)
          ("C-c '"  . my-pair-quotes-dwim)
          ("C-c \"" . my-pair-quotes-dwim)
          ("C-c -"  . my-pair-delete)
          ("C-c e"  . my-pair-close-all)
          ("M-a"    . my-pair-step-out-backward)
          ("M-e"    . my-pair-step-out-forward)))

(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :config
  (progn
    (require 'flymake)
    (defvar my-php-bin-dir
      (cond
       ((eq my-location 'RTP) "/nfs/ibunobackup2/scfrazer/local/php/bin")
       ((eq my-location 'SJC) "/auto/vtt/www/prod/dev/local/bin")))
    (defun flymake-php-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list (concat my-php-bin-dir "/php-lint") (list local-file))))
    (defun my-php-tidy ()
      "Run phpcbf on buffer."
      (interactive "*")
      (call-process-shell-command (concat my-php-bin-dir "/phpcbf --standard=VTT " (buffer-file-name)))
      (revert-buffer nil t t))
    (defun my-php-mode-hook ()
      (font-lock-add-keywords nil '(("default" (0 'font-lock-keyword-face prepend))) 'add-to-end)
      (when my-php-bin-dir
        (flymake-mode 1)))
    (add-hook 'php-mode-hook 'my-php-mode-hook)))

(use-package python
  :defer t
  :config
  (progn
    (require 'my-python)
    (defun my-python-mode-hook ()
      (setq forward-sexp-function nil))
    (add-hook 'python-mode-hook 'my-python-mode-hook)))

(use-package my-reformat
  :bind* ("C-c ," . my-reformat-comma-delimited-items))

(use-package register-list
  :defer t
  :init
  (defalias 'rl 'register-list)
  :config
  (require 'my-register-list))

(use-package revbufs
  :bind* ("C-c R" . revbufs))

(use-package sgml-mode
  :mode (("\\.\\(xml\\|xsl\\|rng\\)\\'" . sgml-mode))
  :config
  (require 'my-sgml-mode))

(use-package simple-git
  :config
  :bind-keymap (("C-x g" . simple-git-global-map)))

(use-package my-sort-lines
  :commands (my-sort-lines)
  :init
  (defalias 'sl 'my-sort-lines))

(use-package speedbar
  :bind* ("C-c I" . sr-speedbar-toggle)
  :config
  (progn
    (require 'sr-speedbar)
    (require 'sb-imenu)
    (setq speedbar-indentation-width 2
          speedbar-initial-expansion-list-name "sb-imenu"
          speedbar-use-images nil)
    (speedbar-add-supported-extension ".v")
    (speedbar-add-supported-extension ".sv")
    (speedbar-add-supported-extension ".svh")
    (speedbar-add-supported-extension ".aop")))

(use-package sv-mode
  :mode (("\\.aop\\'" . sv-mode)
         ("\\.sv\\'" . sv-mode)
         ("\\.sva\\'" . sv-mode)
         ("\\.svh\\'" . sv-mode)
         ("\\.v\\'" . sv-mode)
         ("\\.vh\\'" . sv-mode))
  :config
  (require 'my-sv-mode))

(use-package sqlplus
  :mode (("\\.sqp\\'" . sqlplus-mode))
  :config
  (require 'my-sql))

(use-package term
  :commands (my-term)
  :config
  (progn
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
    (defadvice term-handle-exit (after my-term-handle-exit activate)
      "Kill terminal buffer after exit."
      (kill-buffer))))

(use-package my-tmux
  :bind* (("M-c" . my-tmux-iterm-copy)
          ("M-t" . my-tmux-copy)))

(use-package vc
  :commands (vc-dir)
  :init
  (setq vc-handled-backends '(Git))
  :config
  (require 'my-vc))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :config
  (progn
    (setq web-mode-auto-close-style 1
          web-mode-enable-auto-closing t
          web-mode-enable-auto-indentation t
          web-mode-enable-current-element-highlight t
          web-mode-enable-html-entities-fontification t)
    (defun my-web-mode-comment-insert ()
      (interactive)
      (web-mode-comment-insert))
    (bind-keys :map web-mode-map
               ("C-c C-o" . my-web-mode-comment-insert))))

(use-package my-xclip
  :bind* (("C-c X" . my-xclip-yank)
          ("C-c x" . my-xclip-copy)))

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
(autoload 'highlight-indentation-mode "highlight-indentation" nil t)
(autoload 'htmlize-region "htmlize" nil t)
(autoload 'json-mode "json-mode" nil t)
(autoload 'makefile-mode "make-mode" nil t)
(autoload 'rdl-mode "rdl-mode" nil t)
(autoload 'regman "regman" nil t)
(autoload 'rst-mode "rst" "reStructured Text Mode" t)
(autoload 'specterx-mode "specterx-mode" "SpecterX mode" t)
(autoload 'sse-log-mode "sse-log-mode" nil t)
(autoload 'uvm-log-mode "uvm-log-mode" nil t)
(autoload 'vsif-mode "vsif-mode" "VSIF mode" t)

(show-paren-mode t)
(delete-selection-mode t)
(transient-mark-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(winner-mode 1)
(when (fboundp 'electric-indent-mode)
  (electric-indent-mode -1))

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode -1))

(setq-default Man-notify-method 'bully
              backup-inhibited t
              bidi-display-reordering nil
              blink-matching-paren-distance nil
              browse-kill-ring-display-duplicates nil
              browse-kill-ring-highlight-current-entry nil
              browse-kill-ring-maximum-display-length 400
              browse-kill-ring-no-duplicates t
;;               browse-kill-ring-separator "                                                                                "
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
              echo-keystrokes 0.1
              eval-expression-print-length nil
              eval-expression-print-level nil
              even-window-heights nil
              file-template-insert-automatically 'ask
              file-template-paths (list (concat user-emacs-directory "templates/"))
              fill-column 78
              flyspell-mode-map nil
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
              redisplay-dont-pause t
              rst-mode-lazy nil
              scroll-conservatively 10000
              scroll-error-top-bottom t
              scroll-margin 3
              scroll-preserve-screen-position t
              shift-select-mode nil
              show-paren-delay 0
              split-width-threshold nil
              truncate-lines nil
              truncate-partial-width-windows t
              user-mail-address (concat "<" (getenv "USER") "@cisco.com>")
              visible-bell t
              warning-suppress-types (list '(undo discard-info))
              winner-boring-buffers (list "*Completions*" "*Help*" "*Apropos*" "*buffer-selection*")
              winner-ring-size 50)

(setq-default select-active-regions t ;; nil
              mouse-drag-copy-region t
              x-select-enable-primary t
              x-select-enable-clipboard nil)

(add-to-list 'auto-mode-alist '("Makefile.*\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.rdlh?\\'" . rdl-mode))
(add-to-list 'auto-mode-alist '("\\.s\\'" . specterx-mode))
(add-to-list 'auto-mode-alist '("\\.vsif\\'" . vsif-mode))
(add-to-list 'auto-mode-alist '("\\.zsh.*\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))
(add-to-list 'auto-mode-alist '("dve_gui.log\\'" . uvm-log-mode))
(add-to-list 'auto-mode-alist '("run.log\\'" . uvm-log-mode))

;; Comments

(add-to-list 'comment-styles '(my-style t nil t nil))
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

(defface my-browse-kill-ring-separator-face
  '((t :foreground "color-197"))
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
  (let (beg end)
    (save-excursion
      (unless (looking-at "\\sw\\|\\s_")
        (skip-syntax-backward "^w_"))
      (skip-syntax-forward "w_")
      (setq end (point))
      (skip-syntax-backward "w_")
      (skip-syntax-forward "^w")
      (setq beg (point)))
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
          (replace-match (cdr repl) nil t))))))

(defun my-count-lines (&optional arg)
  "Count lines to next blank line, or with prefix arg count lines in region."
  (interactive "P")
  (if arg
      (message "%d lines in region"
               (count-lines (region-beginning) (region-end)))
    (message "%d lines to end of paragraph"
             (count-lines (point-at-bol)
                          (save-excursion (forward-paragraph) (point-at-eol))))))

(defun cpl ()
  "Turn on cpl-mode."
  (interactive)
  (require 'cpl-mode)
  (cpl-mode))

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
    (let ((regexp (concat "\\`" (regexp-opt '("*scratch" "*sqlplus"))))
          (windows (window-list-1 (next-window)))
          buf)
      (catch 'done
        (dolist (win windows)
          (setq buf (window-buffer win))
          (when (and (string-match "\\`*\\|.+<[0-9]+>\\'" (buffer-name buf))
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

(defun my-narrow (&optional arg)
  "Narrow to region, or widen if already narrowed, or with prefix
arg do something special."
  (interactive "P")
  (if (and arg (equal major-mode 'sv-mode))
      (sv-mode-narrow-to-scope)
    (if (/= (buffer-size) (- (point-max) (point-min)))
        (widen)
      (narrow-to-region (region-beginning) (region-end))
      (my-mode-line-count-lines)
      (goto-char (point-min)))))

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
        (my-tmux-iterm-copy-text filename))
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
          (query-replace (nth 0 common) (nth 1 common) nil start end))
      (let (from to)
        (setq from (buffer-substring-no-properties (region-beginning) (region-end))
              to (read-from-minibuffer
                  (format "Query replace %s with: " from) nil nil nil
                  'query-replace-history))
        (goto-char (region-beginning))
        (query-replace from to)
        (push (cons from to) query-replace-defaults)))))

(defun my-rectangle-number-lines (&optional arg)
  "Like `rectangle-number-lines' but with better defaults.

When called with one prefix arg, prompt for starting point.  When
called with a different prefix arg, prompt for starting point and
format."
  (interactive "*P")
  (let ((start-at 0)
        (format "%d"))
    (when arg
      (setq start-at (read-string "Start at: "))
      (if (string-match "[a-zA-Z]" start-at)
          (setq start-at (string-to-char start-at)
                format "%c")
        (setq start-at (string-to-number start-at))
        (unless (equal current-prefix-arg '(4))
          (setq format (read-string "Format string: " "%d")))))
    (delete-extract-rectangle (region-beginning) (region-end))
    (let ((start (mark))
          (end (point)))
      (if (< end start)
          (progn
            (setq rectangle-number-line-counter (+ (count-lines end start) start-at -1))
            (apply-on-rectangle 'my-rectangle-reverse-number-line-callback end start format))
        (setq rectangle-number-line-counter start-at)
        (apply-on-rectangle 'rectangle-number-line-callback start end format)))))

(defun my-rectangle-reverse-number-line-callback (start _end format-string)
  (move-to-column start t)
  (insert (format format-string rectangle-number-line-counter))
  (setq rectangle-number-line-counter
        (1- rectangle-number-line-counter)))

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
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme choice t)))

(defun my-toggle-buffer-modified ()
  "Toggle buffer modified/unmodified."
  (interactive)
  (set-buffer-modified-p (not (buffer-modified-p))))

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
  (when (or (equal (buffer-name) "config_tree.txt")
            (equal (buffer-name) "topology.txt"))
    (my-word-wrap-on-hook)))

(defun my-minibuffer-ido-insert-bookmark-dir (&optional arg)
  "Insert a bookmarked dir using ido."
  (interactive "P")
  (let* ((enable-recursive-minibuffers t)
         (dir (my-ido-get-bookmark-dir)))
    (when dir
      (when arg
        (setq dir (concat "/view/CPPDVTOOLS.view" dir)))
      (insert dir))))

(defun my-minibuffer-setup-hook ()
  (override-global-mode -1)
  (show-mark-mode 0)
  (local-set-key (kbd "C-/") 'dabbrev-expand)
  (local-set-key (kbd "C-\\") 'expand-abbrev)
  (local-set-key (kbd "C-_") 'dabbrev-expand)
  (local-set-key (kbd "C-w") 'my-minibuffer-insert-word-after-point)
  (local-set-key (kbd "C-z") 'undo)
  (local-set-key (kbd "M-$") 'my-minibuffer-ido-insert-bookmark-dir)
  (local-set-key (kbd "M-\\") 'completion-at-point)
  (local-set-key (kbd "M-h") 'my-minibuffer-backward)
  (local-set-key (kbd "M-j") 'my-minibuffer-backward-kill)
  (local-set-key (kbd "M-k") 'my-minibuffer-forward-kill)
  (local-set-key (kbd "M-l") 'my-minibuffer-forward)
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
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(add-hook 'uvm-log-mode-hook 'my-word-wrap-on-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; After loads

(eval-after-load "compile"
  '(progn
     (defun my-compilation-mode-hook ()
       (define-key compilation-mode-map "{" 'compilation-previous-file)
       (define-key compilation-mode-map "}" 'compilation-next-file)
       (setq truncate-lines nil)
       (goto-char (point-max)))
     (add-hook 'compilation-mode-hook 'my-compilation-mode-hook)))

(eval-after-load "file-template"
  '(progn
     (add-to-list 'file-template-mapping-alist '("\\.csh$" . "template.csh"))
     (add-to-list 'file-template-mapping-alist '("\\.e$" . "template.e"))
     (add-to-list 'file-template-mapping-alist '("\\.html?$" . "template.html"))
     (add-to-list 'file-template-mapping-alist '("\\.js$" . "template.js"))
     (add-to-list 'file-template-mapping-alist '("\\.php$" . "template.php"))
     (add-to-list 'file-template-mapping-alist '("\\.s$" . "template.s"))
     (add-to-list 'file-template-mapping-alist '("\\.sh$" . "template.sh"))
     (add-to-list 'file-template-mapping-alist '("\\.sv$" . "template.sv"))
     (add-to-list 'file-template-mapping-alist '("\\.svh$" . "template.svh"))
     (add-to-list 'file-template-mapping-alist '("\\.v$" . "template.v"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(bind-keys*
 ("<delete>"    . delete-char)
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
 ("C-c M"       . recompile)
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
 ("C-x #"       . linum-mode)
 ("C-x ("       . kmacro-start-macro-or-insert-counter)
 ("C-x -"       . my-window-resize)
 ("C-x C-c"     . my-kill-frame-or-emacs)
 ("C-x C-n"     . other-window)
 ("C-x C-p"     . (lambda () (interactive (other-window -1))))
 ("C-x C-z"     . (lambda () (interactive) (ding)))
 ("C-x E"       . my-apply-macro-to-region-lines)
 ("C-x K"       . kill-buffer-and-window)
 ("C-x M-q"     . my-toggle-buffer-modified)
 ("C-x S"       . (lambda () "Shrink other window." (interactive) (shrink-window-if-larger-than-buffer (next-window))))
 ("C-x SPC"     . fixup-whitespace)
 ("C-x _"       . (lambda () (interactive) (my-window-resize t)))
 ("C-x `"       . my-flymake-goto-next-error)
 ("C-x c"       . clone-indirect-buffer-other-window)
 ("C-x e"       . my-call-last-kbd-macro)
 ("C-x k"       . kill-buffer)
 ("C-x s"       . shrink-window-if-larger-than-buffer)
 ("C-x t"       . task-map)
 ("C-x w"       . my-clone-file)
 ("C-x |"       . my-toggle-window-split)
 ("C-x ~"       . my-flymake-goto-prev-error)
 ("C-z"         . undo)
 ("ESC <left>"  . (lambda () "Select previous frame." (interactive) (other-frame 1)))
 ("ESC <right>" . (lambda () "Select next frame." (interactive) (other-frame -1)))
 ("M-!"         . my-shell-command-on-current-file)
 ("M-%"         . my-query-replace)
 ("M-="         . my-count-lines)
 ("M-N"         . scroll-up-command)
 ("M-P"         . scroll-down-command)
 ("M-Q"         . my-unfill)
 ("M-`"         . next-error)
 ("M-g"         . my-goto-line-column)
 ("M-q"         . my-fill)
 ("M-r M-n"     . my-forward-paragraph-rect)
 ("M-r M-p"     . my-backward-paragraph-rect)
 ("M-r SPC"     . rectangle-mark-mode)
 ("M-r k"       . kill-rectangle)
 ("M-r n"       . my-rectangle-number-lines)
 ("M-r t"       . string-rectangle)
 ("M-u"         . my-recenter)
 ("M-z"         . redo)
 ("M-}"         . my-forward-paragraph)
 ("M-~"         . previous-error))

;; These have to be in this order

(bind-key* "C-c h" 'help-command)
(bind-key* "C-c h a" 'apropos)
(bind-key* "C-c h I" 'info-apropos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cisco setup

(when (or (eq my-location 'RTP)
          (eq my-location 'SJC)
          (eq my-location 'BGL))

  (require 'vcs-compile)
  (add-to-list 'vcs-compile-command-list "q lsq_targ_buildpb_chipMl2infMpciMpaMfabMhbmGMPP64 /build_test/")

  (add-to-list 'my-compile-command "q lsq_compile_src_chipdv")

  (require 'ur-log-mode)

  (defun dv-lint ()
    (interactive)
    (compilation-mode)
    (hl-line-mode 1)
    (set (make-local-variable 'compilation-error-regexp-alist)
         (list '("^.+\\s-+line:\\s-+\\([0-9]+\\)\\s-+in file:\\s-+\\([^ \t\n]+\\)" 2 1))))

  (eval-after-load "ll-debug"
    '(progn
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
                                     (if v1 "\\n\", " "\\n\"") v1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases

(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'bc 'emacs-lisp-byte-compile)
(defalias 'bcl 'emacs-lisp-byte-compile-and-load)
(defalias 'bre 'my-backward-regexp)
(defalias 'colors 'list-colors-display)
(defalias 'edbg 'edebug-defun)
(defalias 'file 'my-put-file-name-on-clipboard)
(defalias 'fnd 'my-dired-find-name-dired)
(defalias 'fre 'my-forward-regexp)
(defalias 'hli 'highlight-indentation-mode)
(defalias 'ind 'my-indent)
(defalias 'init (lambda () (interactive) (require 'use-package) (find-file (concat user-emacs-directory "lisp/src/my-init.el"))))
(defalias 'qrr 'query-replace-regexp)
(defalias 'ren 'rename-buffer)
(defalias 'rot 'my-rotate-window-buffers)
(defalias 'sb 'sr-speedbar-toggle)
(defalias 'serve 'my-server)
(defalias 'tail 'auto-revert-tail-mode)
(defalias 'tdoe 'toggle-debug-on-error)
(defalias 'theme 'my-theme)
(defalias 'uniq 'my-delete-duplicate-lines)
(defalias 'unt 'my-untabity)
(defalias 'vc_gen (lambda () (interactive) (require 'vc_gen)))
(defalias 'vtt (lambda () (interactive) (require 'vtt)))
(defalias 'work (lambda () (interactive) (find-file (expand-file-name "~/Documents/Org/Work.org"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System setup

(unless window-system

  (set-terminal-coding-system 'utf-8)

  (defface my-display-table-face
    '((((class color) (background dark)) :foreground "color-201" :background "black")
      (((class color) (background light)) :foreground "color-125" :background "color-255"))
    "Face for terminal truncation/wrapping glyphs."
    :group 'faces)

  (let ((truncation-glyph (make-glyph-code ?→ 'my-display-table-face))
        (wrap-glyph (make-glyph-code ?↓ 'my-display-table-face))
        (escape-glyph (make-glyph-code ?\\ 'my-display-table-face))
        (control-glyph (make-glyph-code ?^ 'my-display-table-face)))
    (set-display-table-slot standard-display-table 'truncation truncation-glyph)
    (set-display-table-slot standard-display-table 'wrap wrap-glyph)
    (set-display-table-slot standard-display-table 'escape escape-glyph)
    (set-display-table-slot standard-display-table 'control control-glyph))

  (bind-keys* ("<f1>"  . xterm-mouse-mode)
              ("C-M-z" . my-suspend-emacs)
              ("C-_"   . dabbrev-expand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS-specific setup

(let ((extra-config (concat user-emacs-directory (symbol-name system-type) ".el")))
  (when (file-exists-p extra-config)
    (load-file extra-config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finish up

(require 'btext-mode)

;; Disabled commands

(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;; Done

(provide 'my-init)
