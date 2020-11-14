;;; fd-dired-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fd-dired" "fd-dired.el" (0 0 0 0))
;;; Generated autoloads from fd-dired.el

(autoload 'fd-dired "fd-dired" "\
Run `fd' and go into Dired mode on a buffer of the output.
The command run (after changing into DIR) is essentially

    fd . ARGS -ls

except that the car of the variable `fd-dired-ls-option' specifies what to
use in place of \"-ls\" as the final argument.

\(fn DIR ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fd-dired" '("fd-dired-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fd-dired-autoloads.el ends here
