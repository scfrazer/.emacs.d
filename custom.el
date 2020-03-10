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
 '(clean-buffer-list-kill-buffer-names
   (quote
    ("*Help*" "*Apropos*" "*Man " "*Buffer List*" "*Compile-Log*" "*vc*" "*vc-diff*")))
 '(custom-safe-themes t)
 '(deft-new-file-format "Notes-%Y-%m-%d.md")
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-minor-mode-string " Doc")
 '(flyspell-mode-line-string " Sp")
 '(frame-background-mode (quote light))
 '(highlight-indent-guides-delay 0.5)
 '(highlight-indent-guides-method (quote character))
 '(jedi:tooltip-method nil)
 '(midnight-mode t nil (midnight))
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(package-selected-packages
   (quote
    (git-timemachine fuzzy jedi jedi-direx fzf relint yaml-mode deft markdown-mode fd-dired dired-rainbow dired-subtree rg hydra doneburn-theme spacemacs-theme expand-region sr-speedbar filladapt multiple-cursors htmlize dash crontab-mode avy)))
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
