;;; project-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "project" "project.el" (0 0 0 0))
;;; Generated autoloads from project.el

(autoload 'project-current "project" "\
Return the project instance in DIRECTORY, defaulting to `default-directory'.

When no project is found in that directory, the result depends on
the value of MAYBE-PROMPT: if it is nil or omitted, return nil,
else ask the user for a directory in which to look for the
project, and if no project is found there, return a \"transient\"
project instance.

The \"transient\" project instance is a special kind of value
which denotes a project rooted in that directory and includes all
the files under the directory except for those that should be
ignored (per `project-ignores').

See the doc string of `project-find-functions' for the general form
of the project instance object.

\(fn &optional MAYBE-PROMPT DIRECTORY)" nil nil)

(defvar project-prefix-map (let ((map (make-sparse-keymap))) (define-key map "!" 'project-shell-command) (define-key map "&" 'project-async-shell-command) (define-key map "f" 'project-find-file) (define-key map "F" 'project-or-external-find-file) (define-key map "b" 'project-switch-to-buffer) (define-key map "s" 'project-shell) (define-key map "d" 'project-dired) (define-key map "v" 'project-vc-dir) (define-key map "c" 'project-compile) (define-key map "e" 'project-eshell) (define-key map "k" 'project-kill-buffers) (define-key map "p" 'project-switch-project) (define-key map "g" 'project-find-regexp) (define-key map "G" 'project-or-external-find-regexp) (define-key map "r" 'project-query-replace-regexp) map) "\
Keymap for project commands.")
 (define-key ctl-x-map "p" project-prefix-map)

(autoload 'project-other-window-command "project" "\
Run project command, displaying resultant buffer in another window.

The following commands are available:

\\{project-prefix-map}
\\{project-other-window-map}" t nil)
 (define-key ctl-x-4-map "p" #'project-other-window-command)

(autoload 'project-other-frame-command "project" "\
Run project command, displaying resultant buffer in another frame.

The following commands are available:

\\{project-prefix-map}
\\{project-other-frame-map}" t nil)
 (define-key ctl-x-5-map "p" #'project-other-frame-command)

(autoload 'project-other-tab-command "project" "\
Run project command, displaying resultant buffer in a new tab.

The following commands are available:

\\{project-prefix-map}" t nil)

(when (bound-and-true-p tab-prefix-map) (define-key tab-prefix-map "p" #'project-other-tab-command))

(autoload 'project-find-regexp "project" "\
Find all matches for REGEXP in the current project's roots.
With \\[universal-argument] prefix, you can specify the directory
to search in, and the file name pattern to search for.  The
pattern may use abbreviations defined in `grep-files-aliases',
e.g. entering `ch' is equivalent to `*.[ch]'.  As whitespace
triggers completion when entering a pattern, including it
requires quoting, e.g. `\\[quoted-insert]<space>'.

\(fn REGEXP)" t nil)

(autoload 'project-or-external-find-regexp "project" "\
Find all matches for REGEXP in the project roots or external roots.
With \\[universal-argument] prefix, you can specify the file name
pattern to search for.

\(fn REGEXP)" t nil)

(autoload 'project-find-file "project" "\
Visit a file (with completion) in the current project.

The completion default is the filename at point, determined by
`thing-at-point' (whether such file exists or not)." t nil)

(autoload 'project-or-external-find-file "project" "\
Visit a file (with completion) in the current project or external roots.

The completion default is the filename at point, determined by
`thing-at-point' (whether such file exists or not)." t nil)

(autoload 'project-dired "project" "\
Start Dired in the current project's root." t nil)

(autoload 'project-vc-dir "project" "\
Run VC-Dir in the current project's root." t nil)

(autoload 'project-shell "project" "\
Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists." t nil)

(autoload 'project-eshell "project" "\
Start Eshell in the current project's root directory.
If a buffer already exists for running Eshell in the project's root,
switch to it.  Otherwise, create a new Eshell buffer.
With \\[universal-argument] prefix arg, create a new Eshell buffer even
if one already exists." t nil)

(autoload 'project-async-shell-command "project" "\
Run `async-shell-command' in the current project's root directory." t nil)

(autoload 'project-shell-command "project" "\
Run `shell-command' in the current project's root directory." t nil)

(autoload 'project-search "project" "\
Search for REGEXP in all the files of the project.
Stops when a match is found.
To continue searching for the next match, use the
command \\[fileloop-continue].

\(fn REGEXP)" t nil)

(autoload 'project-query-replace-regexp "project" "\
Query-replace REGEXP in all the files of the project.
Stops when a match is found and prompts for whether to replace it.
If you exit the query-replace, you can later continue the query-replace
loop using the command \\[fileloop-continue].

\(fn FROM TO)" t nil)

(autoload 'project-compile "project" "\
Run `compile' in the project root.
Arguments the same as in `compile'.

\(fn COMMAND &optional COMINT)" t nil)

(autoload 'project-switch-to-buffer "project" "\
Display buffer BUFFER-OR-NAME in the selected window.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

\(fn BUFFER-OR-NAME)" t nil)

(autoload 'project-display-buffer "project" "\
Display BUFFER-OR-NAME in some window, without selecting it.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer' as a subroutine, which see
for how it is determined where the buffer will be displayed.

\(fn BUFFER-OR-NAME)" t nil)

(autoload 'project-display-buffer-other-frame "project" "\
Display BUFFER-OR-NAME preferably in another frame.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer-other-frame' as a subroutine,
which see for how it is determined where the buffer will be
displayed.

\(fn BUFFER-OR-NAME)" t nil)

(autoload 'project-kill-buffers "project" "\
Kill the buffers belonging to the current project.
Two buffers belong to the same project if their project
instances, as reported by `project-current' in each buffer, are
identical.  Only the buffers that match a condition in
`project-kill-buffer-conditions' will be killed.  If NO-CONFIRM
is non-nil, the command will not ask the user for confirmation.
NO-CONFIRM is always nil when the command is invoked
interactively.

\(fn &optional NO-CONFIRM)" t nil)

(autoload 'project-remember-project "project" "\
Add project PR to the front of the project list.
Save the result in `project-list-file' if the list of projects has changed.

\(fn PR)" nil nil)

(autoload 'project-known-project-roots "project" "\
Return the list of root directories of all known projects." nil nil)

(defvar project-switch-commands '((102 "Find file" project-find-file) (103 "Find regexp" project-find-regexp) (100 "Dired" project-dired) (118 "VC-Dir" project-vc-dir) (101 "Eshell" project-eshell)) "\
Alist mapping keys to project switching menu entries.
Used by `project-switch-project' to construct a dispatch menu of
commands available upon \"switching\" to another project.

Each element is of the form (KEY LABEL COMMAND), where COMMAND is the
command to run when KEY is pressed.  LABEL is used to distinguish
the menu entries in the dispatch menu.")

(autoload 'project-switch-project "project" "\
\"Switch\" to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'.

When called in a program, it will use the project corresponding
to directory DIR.

\(fn DIR)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "project" '("project-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; project-autoloads.el ends here
