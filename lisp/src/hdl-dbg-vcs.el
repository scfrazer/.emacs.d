;;; hdl-dbg-vcs.el

(require 'hdl-dbg)
(require 'sv-mode)

(setq hdl-dbg-bpnt-not-allowed-fcn 'hdl-dbg-vcs-bpnt-not-allowed-fcn
      hdl-dbg-bpnt-str-fcn 'hdl-dbg-vcs-bpnt-str-fcn
      hdl-dbg-sim-bpnt-str-fcn 'hdl-dbg-vcs-sim-bpnt-str-fcn
      )

(defun hdl-dbg-vcs-bpnt-not-allowed-fcn ()
  "Is a breakpoint allowed here?"
  (or (looking-at "[ \t]*\\(//\\|$\\)")
      (sv-mode-in-comment-or-string)))

(defun hdl-dbg-vcs-bpnt-str-fcn (filename line-num condition time)
  "Create breakpoint string."
  (concat "change " time " break on line " (number-to-string line-num) " @"
          (file-name-sans-extension (file-name-nondirectory filename))
          (if condition (concat " if " condition) "")
          " -- " filename "\n"))

(defun hdl-dbg-vcs-sim-bpnt-str-fcn (filename line-num condition time)
  "Create simulator breakpoint string."
  (concat "break on line " (number-to-string line-num) " @"
          (file-name-sans-extension (file-name-nondirectory filename))
          (if condition (insert " if " condition) "")
          "\n"))

;; Integrate into sv-mode

(define-key sv-mode-map (kbd "<f10>") 'hdl-dbg-toggle-breakpoint)
(define-key sv-mode-map (kbd "C-<f10>") 'hdl-dbg-show-control-window)
(define-key sv-mode-map (kbd "<left-fringe> <mouse-1>") 'hdl-dbg-mouse-toggle-breakpoint)
(define-key sv-mode-map (kbd "<left-fringe> <M-mouse-1>") 'hdl-dbg-mouse-conditional-breakpoint)

(provide 'hdl-dbg-vcs)
