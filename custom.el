(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil t)
 '(ahs-inhibit-face-list nil t)
 '(ahs-select-invisible 'skip t)
 '(ahs-suppress-log t t)
 '(ansi-color-names-vector
   ["#ffffff" "#183691" "#969896" "#a71d5d" "#969896" "#969896" "#795da3" "#969896"])
 '(beacon-blink-delay 0.25)
 '(beacon-blink-duration 0.1)
 '(beacon-blink-when-window-scrolls nil)
 '(beacon-color "#D70087")
 '(beacon-lighter "")
 '(beacon-mode t)
 '(beacon-push-mark nil)
 '(beacon-size 40)
 '(clean-buffer-list-kill-buffer-names
   '("*Help*" "*Apropos*" "*Man " "*Buffer List*" "*Compile-Log*" "*vc*" "*vc-diff*"))
 '(completion-category-overrides '((file (styles basic substring))))
 '(completion-ignore-case t t)
 '(custom-safe-themes t)
 '(deft-new-file-format "Notes-%Y-%m-%d.md")
 '(desktop-lazy-verbose nil)
 '(desktop-load-locked-desktop t)
 '(desktop-path '("."))
 '(desktop-restore-frames t)
 '(desktop-save 'if-exists)
 '(desktop-save-mode t)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-minor-mode-string " Doc")
 '(fci-rule-color "#969896")
 '(flyspell-mode-line-string " Sp")
 '(frame-background-mode 'light)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-indent-guides-delay 0.5)
 '(highlight-indent-guides-method 'character)
 '(icomplete-compute-delay 0)
 '(icomplete-delay-completions-threshold 10000)
 '(icomplete-max-delay-chars 0)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#D08770"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#A3BE8C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#4f5b66"))
 '(midnight-mode t nil (midnight))
 '(minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
 '(modus-operandi-theme-bold-constructs t)
 '(modus-operandi-theme-completions 'moderate)
 '(modus-operandi-theme-faint-syntax nil)
 '(modus-operandi-theme-intense-paren-match t)
 '(modus-operandi-theme-intense-standard-completions t)
 '(modus-operandi-theme-slanted-constructs t)
 '(modus-vivendi-theme-bold-constructs t)
 '(modus-vivendi-theme-completions 'moderate)
 '(modus-vivendi-theme-intense-paren-match t)
 '(modus-vivendi-theme-intense-standard-completions t)
 '(modus-vivendi-theme-slanted-constructs t)
 '(nrepl-message-colors
   '("#183691" "#969896" "#a71d5d" "#969896" "#0086b3" "#795da3" "#a71d5d" "#969896"))
 '(objed-cursor-color "#BF616A")
 '(orderless-skip-highlighting t)
 '(package-hidden-regexps '("\\`diffview\\'"))
 '(package-selected-packages
   '(s monokai-pro-theme which-key diffview python marginalia selectrum selectrum-prescient flymake project beacon goto-last-change reformatter tango-plus-theme avy bm browse-kill-ring dash dash-functional deft dired-subtree fd-dired fill-function-arguments filladapt git-timemachine highlight-indent-guides hydra iflipb json-mode markdown-mode multiple-cursors orderless popup relint rg sr-speedbar use-package visual-regexp web-beautify web-mode with-editor yaml-mode))
 '(pdf-view-midnight-colors '("#969896" . "#f8eec7"))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(rustic-ansi-faces
   ["#2b303b" "#BF616A" "#A3BE8C" "#ECBE7B" "#8FA1B3" "#c678dd" "#46D9FF" "#c0c5ce"])
 '(safe-local-variable-values
   '((eval modify-syntax-entry 35 "<")
     (eval modify-syntax-entry 10 ">")
     (sgml-always-quote-attributes)
     (sgml-indent-step . 1)
     (sgml-indent-data . t)
     (checkdoc-permit-comma-termination-flag . t)
     (checkdoc-force-docstrings-flag)
     (folded-file . t)))
 '(selectrum-extend-current-candidate-highlight t)
 '(speedbar-indentation-width 4)
 '(tron-legacy-theme-softer-bg t)
 '(vc-annotate-background "#2b303b" t)
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
 '(yank-excluded-properties
   '(category field follow-link fontified font-lock-face help-echo intangible invisible keymap local-map mouse-face read-only yank-handler display highlight-indent-guides-prop)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon-fallback-background ((t (:background "#ffffff"))))
 '(which-key-highlighted-command-face ((t (:inherit font-lock-variable-name-face :underline t))))
 '(which-key-local-map-description-face ((t (:inherit font-lock-builtin-face)))))
