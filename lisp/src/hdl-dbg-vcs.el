;;; hdl-dbg-vcs.el

(require 'hdl-dbg)
(require 'sv-mode)

(setq hdl-dbg-parse-target-buffer-regexp
      "^\\s-*stop\\s-+-file\\s-+{\\(.+\\)}\\s-+-line\\s-+{\\([0-9]+\\)}"
      hdl-dbg-parse-target-buffer-regexp-groups (list 1 2 99 99))

(setq hdl-dbg-bpnt-not-allowed-fcn 'hdl-dbg-vcs-bpnt-not-allowed
      hdl-dbg-bpnt-str-fcn 'hdl-dbg-vcs-bpnt-str
      hdl-dbg-sim-bpnt-str-fcn 'hdl-dbg-vcs-sim-bpnt-str
      hdl-dbg-bpnt-regexp-fcn 'hdl-dbg-vcs-bpnt-regexp
      hdl-dbg-sim-del-bpnt-str-fcn 'hdl-dbg-vcs-sim-del-bpnt-str
      hdl-dbg-target-file-p-fcn 'hdl-dbg-vcs-target-file-p
      hdl-dbg-source-file-p-fcn 'hdl-dbg-vcs-source-file-p
      hdl-dbg-filename-to-module-fcn 'hdl-dbg-vcs-filename-to-module
      hdl-dbg-module-eq-filename-fcn 'hdl-dbg-vcs-module-eq-filename)

(defun hdl-dbg-vcs-bpnt-not-allowed ()
  "Is a breakpoint allowed here?"
  (or (looking-at "[ \t]*\\(//\\|$\\)")
      (sv-mode-in-comment-or-string)))

(defun hdl-dbg-vcs-bpnt-str (filename line-num condition time)
  "Create breakpoint string."
  (concat "stop -file {" filename "} -line {"(number-to-string line-num) "}\n"))

(defun hdl-dbg-vcs-sim-bpnt-str (filename line-num condition time)
  "Create simulator breakpoint string."
  (concat "stop -file {" filename "} -line {"(number-to-string line-num) "}\n"))

(defun hdl-dbg-vcs-bpnt-regexp (filename line-num condition time)
  "Create regexp to find a breakpoint in the target file."
  (concat "^\\s-*stop\\s-+-file\\s-+{"
          (or filename ".+?")
          "}\\s-+-line\\s-+{" (if line-num (number-to-string line-num) ".+?") "}"))

(defun hdl-dbg-vcs-sim-del-bpnt-str (filename line-num condition time)
  "Create simulator delete breakpoint string."
  (concat "TODO" "\n"))

(defun hdl-dbg-vcs-target-file-p (filename)
  "Is filename the target file?"
  (string= (file-name-nondirectory filename) "breakpoint.tcl"))

(defun hdl-dbg-vcs-source-file-p (filename)
  "Is filename a source file?"
  (or (string= (file-name-extension filename) "sv")
      (string= (file-name-extension filename) "svh")))

(defun hdl-dbg-vcs-filename-to-module (filename)
  "Convert filename to 'module' name."
  (file-name-nondirectory filename))

(defun hdl-dbg-vcs-module-eq-filename (module filename)
  "Compare 'module' name to filename."
  (string-match module (file-name-nondirectory filename)))

;; Integrate into sv-mode

(define-key sv-mode-map (kbd "<f10>") 'hdl-dbg-toggle-breakpoint)
(define-key sv-mode-map (kbd "<C-f10>") 'hdl-dbg-show-control-window)
(define-key sv-mode-map (kbd "<M-f10>") 'hdl-dbg-copy-all-to-clipboard)
(define-key sv-mode-map (kbd "<left-fringe> <mouse-1>") 'hdl-dbg-mouse-toggle-breakpoint)
(define-key sv-mode-map (kbd "<left-fringe> <M-mouse-1>") 'hdl-dbg-mouse-conditional-breakpoint)

(provide 'hdl-dbg-vcs)
