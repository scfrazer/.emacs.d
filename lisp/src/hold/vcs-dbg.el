;;; vcs-dbg.el

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom stuff

;;;###autoload
(defgroup vcs-dbg nil
  "*HDL debug."
  :group 'debug)

;;;###autoload
(defcustom vcs-dbg-hook nil
  "*List of functions to call on entry to vcs-dbg mode."
  :group 'vcs-dbg
  :type 'hook)

;;;###autoload
(defface vcs-dbg-bp-face
  '((t (:underline t :background "color-224")))
  "Face to highlight breakpoints"
  :group 'vcs-dbg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar vcs-dbg-buffer-name "*vcs-dbg*"
  "Buffer name.")

(provide 'vcs-dbg)
