;;; relint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "relint" "relint.el" (0 0 0 0))
;;; Generated autoloads from relint.el

(autoload 'relint-file "relint" "\
Scan FILE, an elisp file, for regexp-related errors.

\(fn FILE)" t nil)

(autoload 'relint-directory "relint" "\
Scan all *.el files in DIR for regexp-related errors.

\(fn DIR)" t nil)

(autoload 'relint-current-buffer "relint" "\
Scan the current buffer for regexp errors.
The buffer must be in emacs-lisp-mode.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "relint" '("relint-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; relint-autoloads.el ends here
