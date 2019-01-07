(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clean-buffer-list-kill-buffer-names
   (quote
    ("*Help*" "*Apropos*" "*Man " "*Buffer List*" "*Compile-Log*" "*vc*" "*vc-diff*")))
 '(custom-safe-themes t)
 '(frame-background-mode (quote light))
 '(midnight-mode t nil (midnight))
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(package-selected-packages
   (quote
    (fd-dired dired-rainbow dired-subtree rg hydra doneburn-theme spacemacs-theme expand-region sr-speedbar filladapt multiple-cursors htmlize dash crontab-mode avy)))
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
 '(speedbar-indentation-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
