;;; iflipb-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "iflipb" "iflipb.el" (0 0 0 0))
;;; Generated autoloads from iflipb.el

(autoload 'iflipb-next-buffer "iflipb" "\
Flip to the next buffer in the buffer list.

Consecutive invocations switch to less recent buffers in the
buffer list. Buffers matching `iflipb-always-ignore-buffers' are
always ignored. Without a prefix argument, buffers matching
`iflipb-ignore-buffers' are also ignored.

\(fn ARG)" t nil)

(autoload 'iflipb-previous-buffer "iflipb" "\
Flip to the previous buffer in the buffer list.

Consecutive invocations switch to more recent buffers in the
buffer list.

\(fn)" t nil)

(autoload 'iflipb-kill-buffer "iflipb" "\
Same as `kill-buffer' but keep the iflipb buffer list state.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "iflipb" '("iflipb-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; iflipb-autoloads.el ends here
