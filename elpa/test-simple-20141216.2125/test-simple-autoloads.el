;;; test-simple-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "test-simple" "test-simple.el" (0 0 0 0))
;;; Generated autoloads from test-simple.el

(autoload 'test-simple-start "test-simple" "\


\(fn &optional TEST-START-MSG)" nil t)

(autoload 'test-simple-clear "test-simple" "\
Initializes and resets everything to run tests. You should run
this before running any assertions. Running more than once clears
out information from the previous run.

\(fn &optional TEST-INFO TEST-START-MSG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "test-simple" '("assert-" "end-tests" "note" "test-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; test-simple-autoloads.el ends here
