(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-inhibit-face-list nil)
 '(ahs-select-invisible (quote skip))
 '(ahs-suppress-log t)
 '(amx-backend (quote ido))
 '(amx-mode t)
 '(amx-show-key-bindings nil)
 '(clean-buffer-list-kill-buffer-names
   (quote
    ("*Help*" "*Apropos*" "*Man " "*Buffer List*" "*Compile-Log*" "*vc*" "*vc-diff*")))
 '(custom-safe-themes t)
 '(deft-new-file-format "Notes-%Y-%m-%d.md")
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-minor-mode-string " Doc")
 '(flyspell-mode-line-string " Sp")
 '(frame-background-mode (quote light))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-indent-guides-delay 0.5)
 '(highlight-indent-guides-method (quote character))
 '(icomplete-compute-delay 0)
 '(icomplete-delay-completions-threshold 10000)
 '(icomplete-max-delay-chars 0)
 '(midnight-mode t nil (midnight))
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(package-selected-packages
   (quote
    (amx auto-highlight-symbol avy bm browse-kill-ring crontab-mode dash dash-functional deft dired-subtree expand-region fd-dired fill-function-arguments filladapt flymake-easy flymake-eslint flymake-perlcritic fzf git-timemachine goto-chg highlight-indent-guides htmlize hydra icomplete-vertical ido-completing-read+ ido-vertical-mode iflipb json-mode markdown-mode modus-operandi-theme modus-vivendi-theme multiple-cursors orderless popup relint rg sr-speedbar use-package visual-regexp web-beautify web-mode with-editor yaml-mode)))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(safe-local-variable-values
   (quote
    ((eval modify-syntax-entry 35 "<")
     (eval modify-syntax-entry 10 ">")
     (sgml-always-quote-attributes)
     (sgml-indent-step . 1)
     (sgml-indent-data . t)
     (checkdoc-permit-comma-termination-flag . t)
     (checkdoc-force-docstrings-flag)
     (folded-file . t))))
 '(speedbar-indentation-width 4)
 '(yank-excluded-properties
   (quote
    (category field follow-link fontified font-lock-face help-echo intangible invisible keymap local-map mouse-face read-only yank-handler display highlight-indent-guides-prop))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
