;;; hdl-dbg-vcs.el

(require 'hdl-dbg)
(require 'sv-mode)

(setq hdl-dbg-parse-target-buffer-regexp
      "^change \\(.+\\) break on line \\([0-9]+\\) @[a-zA-Z0-9_]+ \\(if \\(.+\\) \\)?-- \\(.+\\)"
      hdl-dbg-parse-target-buffer-regexp-groups (list 5 2 4 1))

(setq hdl-dbg-bpnt-not-allowed-fcn 'hdl-dbg-vcs-bpnt-not-allowed
      hdl-dbg-bpnt-str-fcn 'hdl-dbg-vcs-bpnt-str
      hdl-dbg-sim-bpnt-str-fcn 'hdl-dbg-vcs-sim-bpnt-str
      hdl-dbg-bpnt-regexp-fcn 'hdl-dbg-vcs-bpnt-regexp
      hdl-dbg-sim-del-bpnt-str-fcn 'hdl-dbg-vcs-sim-del-bpnt-str
      hdl-dbg-target-file-p-fcn 'hdl-dbg-vcs-target-file-p
      hdl-dbg-source-file-p-fcn 'hdl-dbg-vcs-source-file-p)

(defun hdl-dbg-vcs-bpnt-not-allowed ()
  "Is a breakpoint allowed here?"
  (or (looking-at "[ \t]*\\(//\\|$\\)")
      (sv-mode-in-comment-or-string)))

(defun hdl-dbg-vcs-bpnt-str (filename line-num condition time)
  "Create breakpoint string."
  (concat "change " time " break on line " (number-to-string line-num) " @"
          (file-name-sans-extension (file-name-nondirectory filename))
          (if condition (concat " if " condition) "")
          " -- " filename "\n"))

(defun hdl-dbg-vcs-sim-bpnt-str (filename line-num condition time)
  "Create simulator breakpoint string."
  (concat "break on line " (number-to-string line-num) " @"
          (file-name-sans-extension (file-name-nondirectory filename))
          (if condition (insert " if " condition) "")
          "\n"))

(defun hdl-dbg-vcs-bpnt-regexp (filename line-num condition time)
  "Create regexp to find a breakpoint in the target file."
  (concat "^change [0-9]+ .s break on line "
          (if line-num (number-to-string line-num) "")
          ".+"
          (or filename "")))

(defun hdl-dbg-vcs-sim-del-bpnt-str (filename line-num condition time)
  "Create simulator delete breakpoint string."
  (concat "delete break \"line " (number-to-string line-num) ".*"
          (file-name-sans-extension (file-name-nondirectory filename)) "\"\n"))

(defun hdl-dbg-vcs-target-file-p (filename)
  "Is filename the target file?"
  (string= (file-name-nondirectory filename) "verbose.txt"))

(defun hdl-dbg-vcs-source-file-p (filename)
  "Is filename a source file?"
  (string= (file-name-extension filename) "e"))

;; Integrate into sv-mode

(define-key sv-mode-map (kbd "<f10>") 'hdl-dbg-toggle-breakpoint)
(define-key sv-mode-map (kbd "C-<f10>") 'hdl-dbg-show-control-window)
(define-key sv-mode-map (kbd "<left-fringe> <mouse-1>") 'hdl-dbg-mouse-toggle-breakpoint)
(define-key sv-mode-map (kbd "<left-fringe> <M-mouse-1>") 'hdl-dbg-mouse-conditional-breakpoint)

(provide 'hdl-dbg-vcs)
