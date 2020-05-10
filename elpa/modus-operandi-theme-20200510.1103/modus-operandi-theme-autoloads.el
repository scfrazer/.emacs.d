;;; modus-operandi-theme-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "modus-operandi-theme" "modus-operandi-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from modus-operandi-theme.el

(defvar modus-operandi-theme-override-colors-alist 'nil "\
Place to override default theme colors.
You can override a subset of the theme's default colors by
defining them in this alist.")

(custom-autoload 'modus-operandi-theme-override-colors-alist "modus-operandi-theme" t)

(when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "modus-operandi-theme" '("modus-operandi")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; modus-operandi-theme-autoloads.el ends here
