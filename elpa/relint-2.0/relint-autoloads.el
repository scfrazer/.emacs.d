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
The buffer must be in emacs-lisp-mode." t nil)

(autoload 'relint-buffer "relint" "\
Scan BUFFER for regexp mistakes. Return list of diagnostics.
Each element in the returned list is an object with the slots

  message    the message string
  beg-pos    starting position in the buffer
  end-pos    ending position the buffer (inclusive), or nil
  pos-type   if `string', then the buffer at BEG-POS..END-POS is inside
             a string literal corresponding to STRING at BEG-IDX..END-IDX;
             otherwise BEG-POS..END-POS just point to code
  string     the string the message is about, or nil
  beg-idx    starting offset in STRING, or nil
  end-idx    ending offset in STRING (inclusive), or nil
  severity   `error', `warning' or `info'

Accessors are prefixed by `relint-diag-': eg, (relint-diag-message D) returns
the message of object D.

BEG-POS..END-POS is the range of interest in the buffer, and may
correspond to the range BEG-IDX..END-IDX in STRING but not necessarily so.

\(fn BUFFER)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "relint" '("relint-")))

;;;***

;;;### (autoloads nil nil ("relint-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; relint-autoloads.el ends here
