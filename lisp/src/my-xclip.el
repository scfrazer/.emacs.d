;;; my-xclip.el

(defvar my-xclip-program (executable-find "xclip")
  "Name of XClip program tool.")

(defun my-xclip-copy (&optional arg)
  "Copy last kill to xclipboard.  With prefix arg, copy region."
  (interactive "P")
  (my-xclip-copy-text
   (if arg
       (buffer-substring-no-properties (region-beginning) (region-end))
     (substring-no-properties (current-kill 0 t))))
  (if arg
      (message "Copied region to xclipboard")
    (message "Copied last kill to xclipboard")))

(defun my-xclip-yank ()
  "Yank from xclipboard."
  (interactive "*")
  (when (and my-xclip-program (getenv "DISPLAY"))
    (call-process "xclip" nil t nil "-o")))

(defun my-xclip-copy-text (text)
  "Copy text to eh xclipboard."
  (when (and my-xclip-program (getenv "DISPLAY"))
    (let* ((process-connection-type nil)
           (proc (start-process "xclip" nil "xclip" "-selection" "primary")))
      (process-send-string proc text)
      (process-send-eof proc))))

(provide 'my-xclip)
