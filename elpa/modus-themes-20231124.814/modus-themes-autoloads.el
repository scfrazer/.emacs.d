;;; modus-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "modus-themes" "modus-themes.el" (0 0 0 0))
;;; Generated autoloads from modus-themes.el

(autoload 'modus-themes-contrast "modus-themes" "\
Measure WCAG contrast ratio between C1 and C2.
C1 and C2 are color values written in hexadecimal RGB.

\(fn C1 C2)" nil nil)

(autoload 'modus-themes-select "modus-themes" "\
Load a Modus THEME using minibuffer completion.
Run `modus-themes-after-load-theme-hook' after loading the theme.
Disable other themes per `modus-themes-disable-other-themes'.

\(fn THEME)" t nil)

(autoload 'modus-themes-toggle "modus-themes" "\
Toggle between the two `modus-themes-to-toggle'.
If `modus-themes-to-toggle' does not specify two Modus themes,
prompt with completion for a theme among our collection (this is
practically the same as the `modus-themes-select' command).

Run `modus-themes-after-load-theme-hook' after loading the theme.
Disable other themes per `modus-themes-disable-other-themes'." t nil)

(autoload 'modus-themes-theme "modus-themes" "\
Bind NAME's color PALETTE around face specs and variables.
Face specifications are passed to `custom-theme-set-faces'.
While variables are handled by `custom-theme-set-variables'.
Those are stored in `modus-themes-faces' and
`modus-themes-custom-variables' respectively.

Optional OVERRIDES are appended to PALETTE, overriding
corresponding entries.

\(fn NAME PALETTE &optional OVERRIDES)" nil t)

(function-put 'modus-themes-theme 'lisp-indent-function '0)

(when load-file-name (let ((dir (file-name-directory load-file-name))) (unless (equal dir (expand-file-name "themes/" data-directory)) (add-to-list 'custom-theme-load-path dir))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "modus-themes" '("modus-themes-")))

;;;***

;;;### (autoloads nil nil ("modus-operandi-deuteranopia-theme.el"
;;;;;;  "modus-operandi-theme.el" "modus-operandi-tinted-theme.el"
;;;;;;  "modus-operandi-tritanopia-theme.el" "modus-themes-pkg.el"
;;;;;;  "modus-vivendi-deuteranopia-theme.el" "modus-vivendi-theme.el"
;;;;;;  "modus-vivendi-tinted-theme.el" "modus-vivendi-tritanopia-theme.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; modus-themes-autoloads.el ends here
