;;; hdl-dbg-vcs.el

(setq hdl-dbg-non-source-line-regexp "[ \t]*\\(//\\|$\\)"
      hdl-dbg-bpnt-str-fcn 'hdl-dbg-vcs-bpnt-str-fcn
      hdl-dbg-sim-bpnt-str-fcn 'hdl-dbg-vcs-sim-bpnt-str-fcn
      )

(defun hdl-dbg-vcs-bpnt-str-fcn (filename line-num condition time)
  "Create breakpoint string."
  (concat "change " time " break on line " (number-to-string line-num) " @"
          (file-name-sans-extension (file-name-nondirectory filename))
          (if condition
              (concat " if " condition)
            "")
          " -- " filename "\n"))

(defun hdl-dbg-vcs-sim-bpnt-str-fcn (filename line-num condition time)
  "Create simulator breakpoint string."
  (concat "break on line " (number-to-string line-num) " @"
          (file-name-sans-extension (file-name-nondirectory filename))
          (if condition (insert " if " condition) "")
          "\n"))

(provide 'hdl-dbg-vcs)
