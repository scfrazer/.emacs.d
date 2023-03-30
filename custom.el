(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#ffffff" "#183691" "#969896" "#a71d5d" "#969896" "#969896" "#795da3" "#969896"])
 '(auto-save-list-file-prefix "~/.emacs.d/.auto-save-list/.saves-")
 '(beacon-blink-delay 0.25)
 '(beacon-blink-duration 0.1)
 '(beacon-blink-when-window-scrolls nil)
 '(beacon-color "#D70087")
 '(beacon-lighter "")
 '(beacon-mode t)
 '(beacon-push-mark nil)
 '(beacon-size 40)
 '(bookmark-default-file "~/.emacs.d/.bookmarks")
 '(citre-capf-optimize-for-popup nil)
 '(citre-capf-substr-completion t)
 '(citre-completion-case-sensitive nil)
 '(citre-update-tags-file-when-no-definitions nil)
 '(clean-buffer-list-kill-buffer-names
   '("*Help*" "*Apropos*" "*Man " "*Buffer List*" "*Compile-Log*" "*vc*" "*vc-diff*"))
 '(completion-category-overrides '((file (styles basic substring))))
 '(completion-ignore-case t t)
 '(custom-safe-themes t)
 '(deft-new-file-format "Notes-%Y-%m-%d.md")
 '(desktop-lazy-verbose nil)
 '(desktop-load-locked-desktop t)
 '(desktop-minor-mode-table
   '((defining-kbd-macro nil)
     (isearch-mode nil)
     (vc-mode nil)
     (vc-dired-mode nil)
     (erc-track-minor-mode nil)
     (savehist-mode nil)
     (global-hl-line-mode global-hl-line-mode)))
 '(desktop-path '("."))
 '(desktop-restore-frames t)
 '(desktop-save 'if-exists)
 '(desktop-save-mode t)
 '(edebug-print-length nil)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-minor-mode-string " Doc")
 '(flyspell-mode-line-string " Sp")
 '(frame-background-mode 'light)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-indent-guides-delay 0.5)
 '(highlight-indent-guides-method 'character)
 '(highlight-tail-colors ((("#354440") . 0) (("#2f434b") . 20)))
 '(ibuffer-title-face 'default)
 '(icomplete-compute-delay 0)
 '(icomplete-delay-completions-threshold 10000)
 '(icomplete-max-delay-chars 0)
 '(marginalia-command-categories
   '((imenu . imenu)
     (my-imenu-nav . imenu)
     (my-complete-switch-to-buffer . buffer)
     (my-complete-recentf-file . file)))
 '(marginalia-margin-threshold 160)
 '(marginalia-separator-threshold 1000)
 '(midnight-mode t nil (midnight))
 '(minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
 '(orderless-skip-highlighting nil t)
 '(package-native-compile t)
 '(package-selected-packages
   '(web-beautify popup vertico which-key diffview python marginalia flymake project beacon goto-last-change tango-plus-theme avy bm browse-kill-ring deft dired-subtree fd-dired fill-function-arguments filladapt git-timemachine hydra iflipb json-mode markdown-mode multiple-cursors orderless relint rg sr-speedbar use-package web-mode with-editor yaml-mode))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(safe-local-variable-values
   '((eval modify-syntax-entry 35 "<")
     (eval modify-syntax-entry 10 ">")
     (sgml-always-quote-attributes)
     (sgml-indent-step . 1)
     (sgml-indent-data . t)
     (checkdoc-permit-comma-termination-flag . t)
     (checkdoc-force-docstrings-flag)
     (folded-file . t)))
 '(set-mark-command-repeat-pop t)
 '(speedbar-indentation-width 4)
 '(transient-history-file "~/.emacs.d/.transient/history.el")
 '(transient-levels-file "~/.emacs.d/.transient/levels.el")
 '(transient-values-file "~/.emacs.d/.transient/values.el")
 '(vc-annotate-background "#2b303b" t)
 '(vc-annotate-background-mode nil t)
 '(vc-annotate-color-map
   (list
    (cons 20 "#A3BE8C")
    (cons 40 "#bbbe86")
    (cons 60 "#d3be80")
    (cons 80 "#ECBE7B")
    (cons 100 "#e2ab77")
    (cons 120 "#d99973")
    (cons 140 "#D08770")
    (cons 160 "#cc8294")
    (cons 180 "#c97db8")
    (cons 200 "#c678dd")
    (cons 220 "#c370b6")
    (cons 240 "#c16890")
    (cons 260 "#BF616A")
    (cons 280 "#a35f69")
    (cons 300 "#875e68")
    (cons 320 "#6b5c67")
    (cons 340 "#65737E")
    (cons 360 "#65737E")) t)
 '(vc-annotate-very-old-color nil t)
 '(warning-suppress-types '((use-package) (comp) (undo discard-info)))
 '(web-mode-enable-control-block-indentation nil)
 '(web-mode-indent-style 1)
 '(which-key-add-column-padding 3)
 '(which-key-highlighted-command-list '("my-.+"))
 '(which-key-idle-delay 10000.0)
 '(which-key-idle-secondary-delay 0.05)
 '(which-key-lighter "")
 '(which-key-max-description-length 50)
 '(which-key-max-display-columns 3)
 '(which-key-mode t)
 '(which-key-paging-prefixes '("C-x" "C-c"))
 '(which-key-separator " -> ")
 '(which-key-show-early-on-C-h t)
 '(which-key-sort-order 'which-key-local-then-key-order)
 '(which-key-sort-uppercase-first nil)
 '(xref-search-program 'ripgrep)
 '(xref-show-definitions-function 'xref-show-definitions-buffer-at-bottom)
 '(yank-excluded-properties
   '(category field follow-link fontified font-lock-face help-echo intangible invisible keymap local-map mouse-face read-only yank-handler display highlight-indent-guides-prop)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon-fallback-background ((t (:background "#ffffff"))))
 '(web-mode-block-control-face ((t (:inherit font-lock-keyword-face))))
 '(web-mode-block-delimiter-face ((t (:inherit font-lock-type-face :weight bold))))
 '(which-key-highlighted-command-face ((t (:inherit font-lock-variable-name-face :underline t))))
 '(which-key-local-map-description-face ((t (:inherit font-lock-builtin-face)))))
