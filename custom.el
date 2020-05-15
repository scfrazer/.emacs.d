(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start nil)
 '(ac-max-width 50)
 '(ac-quick-help-delay 0.5)
 '(ac-stop-flymake-on-completing nil)
 '(ac-trigger-key "C-c /")
 '(ac-use-fuzzy t)
 '(ac-use-menu-map t)
 '(ahs-case-fold-search nil)
 '(ahs-inhibit-face-list nil)
 '(ahs-select-invisible (quote skip))
 '(ahs-suppress-log t)
 '(clean-buffer-list-kill-buffer-names
   (quote
    ("*Help*" "*Apropos*" "*Man " "*Buffer List*" "*Compile-Log*" "*vc*" "*vc-diff*")))
 '(completion-category-overrides (quote ((file (styles basic substring)))))
 '(completion-ignore-case t t)
 '(completion-styles (quote (orderless)))
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
 '(jedi:tooltip-method nil)
 '(midnight-mode t nil (midnight))
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(package-selected-packages
   (quote
    (modus-vivendi-theme auto-highlight-symbol esup orderless modus-operandi-theme icomplete-vertical bm browse-kill-ring dash-functional fill-function-arguments flymake-easy flymake-eslint flymake-perlcritic goto-chg ido-vertical-mode iflipb popup visual-regexp web-beautify web-mode with-editor json-mode use-package highlight-indent-guides git-timemachine fzf relint yaml-mode deft markdown-mode fd-dired dired-rainbow dired-subtree rg hydra doneburn-theme spacemacs-theme expand-region sr-speedbar filladapt multiple-cursors htmlize dash crontab-mode avy)))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(safe-local-variable-values
   (quote
    ((js2-additional-externs "localStorage" "chrome" "url")
     (eval modify-syntax-entry 35 "<")
     (eval modify-syntax-entry 10 ">")
     (sgml-always-quote-attributes)
     (sgml-indent-step . 1)
     (sgml-indent-data . t)
     (checkdoc-permit-comma-termination-flag . t)
     (checkdoc-force-docstrings-flag)
     (folded-file . t)
     (clearcase-version-stamp-active . t))))
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
