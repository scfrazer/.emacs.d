;;; magit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "git-rebase" "git-rebase.el" (21680 4820 0
;;;;;;  0))
;;; Generated autoloads from git-rebase.el

(autoload 'git-rebase-mode "git-rebase" "\
Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

\(fn)" t nil)

(defconst git-rebase-filename-regexp "/git-rebase-todo\\'")

(add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp 'git-rebase-mode))

;;;***

;;;### (autoloads nil "magit" "magit.el" (21680 4821 0 0))
;;; Generated autoloads from magit.el

(autoload 'magit-status "magit" "\
Show the status of the current Git repository in a buffer.
With a prefix argument prompt for a repository to be shown.
With two prefix arguments prompt for an arbitrary directory.
If that directory isn't the root of an existing repository
then offer to initialize it as a new repository.

\(fn &optional DIRECTORY)" t nil)

(autoload 'magit-show-refs-head "magit" "\
List and compare references in a dedicated buffer.
Refs are compared with `HEAD'.

\(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs-current "magit" "\
List and compare references in a dedicated buffer.
Refs are compared with the current branch or `HEAD' if
it is detached.

\(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs "magit" "\
List and compare references in a dedicated buffer.
Refs are compared with a branch read form the user.

\(fn &optional REF ARGS)" t nil)

(autoload 'magit-find-file "magit" "\


\(fn REV FILE)" t nil)

(autoload 'magit-find-file-other-window "magit" "\


\(fn REV FILE)" t nil)

(autoload 'magit-dired-jump "magit" "\
Visit file at point using Dired.
With a prefix argument, visit in other window.  If there
is no file at point then instead visit `default-directory'.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'magit-init "magit" "\
Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally.

\(fn DIRECTORY)" t nil)

(autoload 'magit-checkout "magit" "\
Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch then that becomes the current
branch.  If it is something else then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.

\(git checkout REVISION).

\(fn REVISION)" t nil)

(autoload 'magit-branch-and-checkout "magit" "\
Create and checkout BRANCH at branch or revision START-POINT.

\(git checkout [ARGS] -b BRANCH START-POINT).

\(fn BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-branch-delete "magit" "\
Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point.

\(fn BRANCHES &optional FORCE)" t nil)

(autoload 'magit-branch-set-upstream "magit" "\
Change the UPSTREAM branch of BRANCH.

\(fn BRANCH UPSTREAM)" t nil)

(autoload 'magit-request-pull "magit" "\


\(fn URL START END)" t nil)

(autoload 'magit-branch-rename "magit" "\
Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.

\(git branch -m|-M OLD NEW).

\(fn OLD NEW &optional FORCE)" t nil)

(autoload 'magit-branch-edit-description "magit" "\
Edit the description of BRANCH.

\(fn BRANCH)" t nil)

(autoload 'magit-merge "magit" "\
Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

\(git merge --no-edit|--no-commit [ARGS] REV)

\(fn REV &optional ARGS NOCOMMIT)" t nil)

(autoload 'magit-merge-editmsg "magit" "\
Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.

\(git merge --edit [ARGS] rev)

\(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-nocommit "magit" "\
Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.

\(git merge --no-commit [ARGS] rev)

\(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-abort "magit" "\
Abort the current merge operation.

\(git merge --abort)

\(fn)" t nil)

(autoload 'magit-reset-index "magit" "\
Reset the index to COMMIT.
Keep the head and working tree as-is, so if COMMIT refers to the
head this effectivley unstages all changes.

\(git reset --mixed COMMIT)

\(fn COMMIT)" t nil)

(autoload 'magit-reset "magit" "\
Reset the head and index to COMMIT, but not the working tree.
With a prefix argument also reset the working tree.

\(git reset --mixed|--hard COMMIT)

\(fn COMMIT &optional HARD)" t nil)

(autoload 'magit-reset-head "magit" "\
Reset the head and index to COMMIT, but not the working tree.

\(git reset --mixed COMMIT)

\(fn COMMIT)" t nil)

(autoload 'magit-reset-soft "magit" "\
Reset the head to COMMIT, but not the index and working tree.

\(git reset --soft REVISION)

\(fn COMMIT)" t nil)

(autoload 'magit-reset-hard "magit" "\
Reset the head, index, and working tree to COMMIT.

\(git reset --hard REVISION)

\(fn COMMIT)" t nil)

(autoload 'magit-tag "magit" "\
Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.

\(git tag [--annotate] NAME REV)

\(fn NAME REV &optional ARGS)" t nil)

(autoload 'magit-tag-delete "magit" "\
Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.

\(git tag -d TAGS)

\(fn TAGS)" t nil)

(autoload 'magit-submodule-add "magit" "\
Add the repository at URL as a submodule.
Optional PATH is the path to the submodule relative to the root
of the superproject. If it is nil then the path is determined
based on URL.

\(fn URL &optional PATH)" t nil)

(autoload 'magit-submodule-setup "magit" "\
Clone and register missing submodules and checkout appropriate commits.

\(fn)" t nil)

(autoload 'magit-submodule-init "magit" "\
Register submodules listed in .gitmodules into .git/config.

\(fn)" t nil)

(autoload 'magit-submodule-update "magit" "\
Clone missing submodules and checkout appropriate commits.
With a prefix argument also register submodules in .git/config.

\(fn &optional INIT)" t nil)

(autoload 'magit-submodule-sync "magit" "\
Update each submodule's remote URL according to .gitmodules.

\(fn)" t nil)

(autoload 'magit-submodule-fetch "magit" "\
Fetch submodule.
With a prefix argument fetch all remotes.

\(fn &optional ALL)" t nil)

(autoload 'magit-git-command "magit" "\
Execute a Git subcommand asynchronously, displaying the output.
With a prefix argument run Git in the root of the current
repository.  Non-interactively run Git in DIRECTORY with ARGS.

\(fn ARGS DIRECTORY)" t nil)

(autoload 'magit-format-patch "magit" "\


\(fn RANGE)" t nil)

;;;***

;;;### (autoloads nil "magit-apply" "magit-apply.el" (21680 4820
;;;;;;  0 0))
;;; Generated autoloads from magit-apply.el

(autoload 'magit-stage-file "magit-apply" "\
Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation.

\(fn FILE)" t nil)

(autoload 'magit-stage-modified "magit-apply" "\
Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.
\('git add --update|--all .').

\(fn &optional ALL)" t nil)

(autoload 'magit-unstage-file "magit-apply" "\
Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads nil "magit-bisect" "magit-bisect.el" (21680 4820
;;;;;;  0 0))
;;; Generated autoloads from magit-bisect.el

(autoload 'magit-bisect-start "magit-bisect" "\
Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a know
good and a bad commit.  To move the session forward use the
other actions from the bisect popup (\\<magit-status-mode-map>\\[magit-bisect-popup]).

\(fn BAD GOOD)" t nil)

(autoload 'magit-bisect-reset "magit-bisect" "\
After bisecting cleanup bisection state and return to original HEAD.

\(fn)" t nil)

(autoload 'magit-bisect-good "magit-bisect" "\
While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question.

\(fn)" t nil)

(autoload 'magit-bisect-bad "magit-bisect" "\
While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question.

\(fn)" t nil)

(autoload 'magit-bisect-skip "magit-bisect" "\
While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one.

\(fn)" t nil)

(autoload 'magit-bisect-run "magit-bisect" "\
Bisect automatically by running commands after each step.

\(fn CMDLINE)" t nil)

;;;***

;;;### (autoloads nil "magit-blame" "magit-blame.el" (21680 4820
;;;;;;  0 0))
;;; Generated autoloads from magit-blame.el

(autoload 'magit-blame "magit-blame" "\
Display edit history of FILE up to REVISION.
Interactively blame the file being visited in the current buffer.
If the buffer visits a revision of that file, then blame up to
that revision, otherwise blame the file's full history, including
uncommitted changes.

If Magit-Blame mode is already turned on then blame recursively, by
visiting REVISION:FILE (using `magit-find-file'), where revision
is the revision before the revision that added the lines at
point.

ARGS is a list of additional arguments to pass to `git blame';
only arguments available from `magit-blame-popup' should be used.

\(fn REVISION FILE &optional ARGS)" t nil)

;;;***

;;;### (autoloads nil "magit-commit" "magit-commit.el" (21680 4820
;;;;;;  0 0))
;;; Generated autoloads from magit-commit.el

(autoload 'magit-commit "magit-commit" "\
Create a new commit on HEAD.
With a prefix argument amend to the commit at HEAD instead.

\(git commit [--amend] ARGS)

\(fn &optional ARGS)" t nil)

(autoload 'magit-commit-amend "magit-commit" "\
Amend the last commit.

\(git commit --amend ARGS)

\(fn &optional ARGS)" t nil)

(autoload 'magit-commit-extend "magit-commit" "\
Amend the last commit, without editing the message.
With a prefix argument do change the committer date, otherwise
don't.  The option `magit-commit-extend-override-date' can be
used to inverse the meaning of the prefix argument.

\(git commit --amend --no-edit)

\(fn &optional ARGS OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-reword "magit-commit" "\
Reword the last commit, ignoring staged changes.

With a prefix argument do change the committer date, otherwise
don't.  The option `magit-commit-rewrite-override-date' can be
used to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

\(git commit --amend --only)

\(fn &optional ARGS OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-fixup "magit-commit" "\
Create a fixup commit.
With a prefix argument the target commit has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

\(git commit --no-edit --fixup=COMMIT [ARGS])

\(fn &optional COMMIT ARGS CONFIRM)" t nil)

(autoload 'magit-commit-squash "magit-commit" "\
Create a squash commit.
With a prefix argument the target commit has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

\(git commit --no-edit --squash=COMMIT [ARGS])

\(fn &optional COMMIT ARGS CONFIRM)" t nil)

(autoload 'magit-commit-instant-fixup "magit-commit" "\
Create a fixup commit and instantly rebase.

\(git commit --no-edit --fixup=COMMIT ARGS;
 git rebase -i COMMIT^ --autosquash --autostash)

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-instant-squash "magit-commit" "\
Create a squash commit and instantly rebase.

\(git commit --no-edit --squash=COMMIT ARGS;
 git rebase -i COMMIT^ --autosquash --autostash)

\(fn &optional COMMIT ARGS)" t nil)

;;;***

;;;### (autoloads nil "magit-diff" "magit-diff.el" (21680 4820 0
;;;;;;  0))
;;; Generated autoloads from magit-diff.el

(autoload 'magit-diff-dwim "magit-diff" "\
Show changes for the thing at point.

\(fn &optional ARGS)" t nil)

(autoload 'magit-diff "magit-diff" "\
Show differences between two commits.
RANGE should be a range (A..B or A...B) but can also be a single
commit.  If one side of the range is omitted, then it defaults
to HEAD.  If just a commit is given, then changes in the working
tree relative to that commit are shown.

\(fn RANGE &optional ARGS)" t nil)

(autoload 'magit-diff-working-tree "magit-diff" "\
Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer.

\(fn &optional REV ARGS)" t nil)

(autoload 'magit-diff-staged "magit-diff" "\
Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer.

\(fn &optional REV ARGS)" t nil)

(autoload 'magit-diff-unstaged "magit-diff" "\
Show changes between the working tree and the index.

\(fn &optional ARGS)" t nil)

(autoload 'magit-diff-unpushed "magit-diff" "\
Show unpushed changes.

\(fn &optional ARGS)" t nil)

(autoload 'magit-diff-unpulled "magit-diff" "\
Show unpulled changes.

\(fn &optional ARGS)" t nil)

(autoload 'magit-diff-while-committing "magit-diff" "\


\(fn &optional ARGS)" t nil)

(autoload 'magit-diff-paths "magit-diff" "\
Show changes between any two files on disk.

\(fn A B)" t nil)

(autoload 'magit-show-commit "magit-diff" "\
Show the commit at point.
If there is no commit at point or with a prefix argument prompt
for a commit.

\(fn COMMIT &optional NOSELECT MODULE ARGS)" t nil)

;;;***

;;;### (autoloads nil "magit-ediff" "magit-ediff.el" (21680 4820
;;;;;;  0 0))
;;; Generated autoloads from magit-ediff.el

(autoload 'magit-ediff-resolve "magit-ediff" "\
Resolve outstanding conflicts in FILE using Ediff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'.

\(fn FILE)" t nil)

(autoload 'magit-ediff-stage "magit-ediff" "\
Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-compare "magit-ediff" "\
Compare REVA:FILEA with REVB:FILEB using Ediff.
FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil then this stands for the
working tree state.

\(fn REVA REVB FILEA FILEB)" t nil)

(autoload 'magit-ediff-dwim "magit-ediff" "\
Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the users
mind at all, then it asks the user for a command to run.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magit-extras" "magit-extras.el" (21680 4820
;;;;;;  0 0))
;;; Generated autoloads from magit-extras.el

(autoload 'magit-run-git-gui "magit-extras" "\
Run `git gui' for the current git repository.

\(fn)" t nil)

(autoload 'magit-run-git-gui-blame "magit-extras" "\
Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the HEAD, with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

\(fn COMMIT FILENAME &optional LINENUM)" t nil)

(autoload 'magit-run-gitk "magit-extras" "\
Run Gitk for the current git repository.
Without a prefix argument run `gitk --all', with
a prefix argument run gitk without any arguments.

\(fn ARG)" t nil)

(autoload 'magit-clean "magit-extras" "\
Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.

\(git clean -f -d [-x|-X])

\(fn &optional ARG)" t nil)

(autoload 'magit-add-change-log-entry "magit-extras" "\
Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

\(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil)

(autoload 'magit-add-change-log-entry-other-window "magit-extras" "\
Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer.

\(fn &optional WHOAMI FILE-NAME)" t nil)

;;;***

;;;### (autoloads nil "magit-log" "magit-log.el" (21680 4821 0 0))
;;; Generated autoloads from magit-log.el

(autoload 'magit-log-current "magit-log" "\


\(fn REVS &optional ARGS FILES)" t nil)

(autoload 'magit-log "magit-log" "\


\(fn REVS &optional ARGS FILES)" t nil)

(autoload 'magit-log-head "magit-log" "\


\(fn ARGS)" t nil)

(autoload 'magit-log-buffer-file "magit-log" "\
Show log for the file visited in the current buffer.

\(fn)" t nil)

(autoload 'magit-reflog-current "magit-log" "\
Display the reflog of the current branch.

\(fn)" t nil)

(autoload 'magit-reflog "magit-log" "\
Display the reflog of a branch.

\(fn REF)" t nil)

(autoload 'magit-reflog-head "magit-log" "\
Display the HEAD reflog.

\(fn)" t nil)

(autoload 'magit-cherry "magit-log" "\
Show commits in a branch that are not merged in the upstream branch.

\(fn HEAD UPSTREAM)" t nil)

;;;***

;;;### (autoloads nil "magit-remote" "magit-remote.el" (21680 4821
;;;;;;  0 0))
;;; Generated autoloads from magit-remote.el

(autoload 'magit-clone "magit-remote" "\
Clone the REPOSITORY to DIRECTORY.

\(fn REPOSITORY DIRECTORY)" t nil)

(autoload 'magit-remote-add "magit-remote" "\
Add a remote named REMOTE and fetch it.

\(fn REMOTE URL)" t nil)

(autoload 'magit-remote-rename "magit-remote" "\
Rename the remote named OLD to NEW.

\(fn OLD NEW)" t nil)

(autoload 'magit-remote-set-url "magit-remote" "\
Change the url of the remote named REMOTE to URL.

\(fn REMOTE URL)" t nil)

(autoload 'magit-remote-remove "magit-remote" "\
Delete the remote named REMOTE.

\(fn REMOTE)" t nil)

(autoload 'magit-fetch-current "magit-remote" "\
Fetch from the upstream repository of the current branch.
If `HEAD' is detached or if the upstream is not configured,
then read the remote.

\(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-fetch "magit-remote" "\
Fetch from another repository.

\(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-fetch-all "magit-remote" "\
Fetch from another repository.

\(fn &optional ARGS)" t nil)

(autoload 'magit-pull-current "magit-remote" "\
Fetch from another repository and merge into current branch.

\(fn REMOTE BRANCH &optional ARGS)" t nil)

(autoload 'magit-pull "magit-remote" "\
Fetch from another repository and merge a fetched branch.

\(fn REMOTE BRANCH &optional ARGS)" t nil)

(autoload 'magit-push-current "magit-remote" "\
Push the current branch to its upstream branch.
If the upstream isn't set, then read the remote branch.

\(fn BRANCH REMOTE &optional REMOTE-BRANCH ARGS)" t nil)

(autoload 'magit-push "magit-remote" "\
Push a branch to its upstream branch.
If the upstream isn't set, then read the remote branch.

\(fn BRANCH REMOTE &optional REMOTE-BRANCH ARGS)" t nil)

(autoload 'magit-push-elsewhere "magit-remote" "\
Push a branch or commit to some remote branch.
Read the local and remote branch.

\(fn BRANCH REMOTE REMOTE-BRANCH &optional ARGS)" t nil)

(autoload 'magit-push-matching "magit-remote" "\
Push all matching branches to another repository.
If multiple remotes exit, then read one from the user.
If just one exists, use that without requiring confirmation.

\(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-tags "magit-remote" "\
Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default.

\(fn REMOTE &optional ARGS)" t nil)

;;;***

;;;### (autoloads nil "magit-sequence" "magit-sequence.el" (21680
;;;;;;  4821 0 0))
;;; Generated autoloads from magit-sequence.el

(autoload 'magit-sequencer-continue "magit-sequence" "\


\(fn)" t nil)

(autoload 'magit-sequencer-skip "magit-sequence" "\


\(fn)" t nil)

(autoload 'magit-sequencer-abort "magit-sequence" "\


\(fn)" t nil)

(autoload 'magit-cherry-pick "magit-sequence" "\


\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-cherry-apply "magit-sequence" "\


\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-revert "magit-sequence" "\


\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-revert-no-commit "magit-sequence" "\


\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-am-apply-patches "magit-sequence" "\


\(fn &optional FILES ARGS)" t nil)

(autoload 'magit-am-apply-maildir "magit-sequence" "\


\(fn &optional MAILDIR ARGS)" t nil)

(autoload 'magit-am-continue "magit-sequence" "\


\(fn)" t nil)

(autoload 'magit-am-skip "magit-sequence" "\


\(fn)" t nil)

(autoload 'magit-am-abort "magit-sequence" "\


\(fn)" t nil)

(autoload 'magit-rebase "magit-sequence" "\
Start a non-interactive rebase sequence.

\(git rebase UPSTREAM[^] [ARGS])

\(fn UPSTREAM &optional ARGS)" t nil)

(autoload 'magit-rebase-onto "magit-sequence" "\
Start a non-interactive rebase sequence, using `--onto'.

\(git rebase --onto NEWBASE UPSTREAM[^] [ARGS])

\(fn NEWBASE UPSTREAM &optional ARGS)" t nil)

(autoload 'magit-rebase-interactive "magit-sequence" "\
Start an interactive rebase sequence.

\(git rebase -i COMMIT[^] [ARGS])

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-rebase-autosquash "magit-sequence" "\
Combine squash and fixup commits with their intended targets.

\(git rebase -i COMMIT[^] --autosquash --autostash [ARGS])

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-rebase-edit-commit "magit-sequence" "\
Edit a single older commit using rebase.

\(fn COMMIT)" t nil)

(autoload 'magit-rebase-reword-commit "magit-sequence" "\
Reword a single older commit using rebase.

\(fn COMMIT)" t nil)

(autoload 'magit-rebase-continue "magit-sequence" "\
Restart the current rebasing operation.

\(fn)" t nil)

(autoload 'magit-rebase-skip "magit-sequence" "\
Skip the current commit and restart the current rebase operation.

\(fn)" t nil)

(autoload 'magit-rebase-edit "magit-sequence" "\
Edit the todo list of the current rebase operation.

\(fn)" t nil)

(autoload 'magit-rebase-abort "magit-sequence" "\
Abort the current rebase operation, restoring the original branch.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magit-stash" "magit-stash.el" (21680 4821
;;;;;;  0 0))
;;; Generated autoloads from magit-stash.el

(autoload 'magit-stash "magit-stash" "\
Create a stash of the index and working tree.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-index "magit-stash" "\
Create a stash of the index only.
Unstaged and untracked changes are not stashed.

\(fn MESSAGE)" t nil)

(autoload 'magit-stash-worktree "magit-stash" "\
Create a stash of the working tree only.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-keep-index "magit-stash" "\
Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot "magit-stash" "\
Create a snapshot of the index and working tree.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot-index "magit-stash" "\
Create a snapshot of the index only.
Unstaged and untracked changes are not stashed.

\(fn)" t nil)

(autoload 'magit-snapshot-worktree "magit-stash" "\
Create a snapshot of the working tree only.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn &optional INCLUDE-UNTRACKED)" t nil)

;;;***

;;;### (autoloads nil "magit-wip" "magit-wip.el" (21680 4821 0 0))
;;; Generated autoloads from magit-wip.el

(autoload 'magit-wip-save-mode "magit-wip" "\
On each save, also commit to a work-in-progress ref.

After saving the buffer this mode also commits the changes to
the work-in-progress ref for the current branch.  Use option
`magit-wip-ref-format' to configure what refname is used.

While this mode can be activated manually it is better to do
so using either

  git config --add magit.extension wip-save

to activate it in individual repositories or

  git config --global --add magit.extension wip-save

to activate it in all repositories.  These settings only take
effect after _also_ turning on `global-magit-wip-save-mode'.

\(fn &optional ARG)" t nil)

(defvar global-magit-wip-save-mode nil "\
Non-nil if Global-Magit-Wip-Save mode is enabled.
See the command `global-magit-wip-save-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-magit-wip-save-mode'.")

(custom-autoload 'global-magit-wip-save-mode "magit-wip" nil)

(autoload 'global-magit-wip-save-mode "magit-wip" "\
Toggle Magit-Wip-Save mode in all buffers.
With prefix ARG, enable Global-Magit-Wip-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-Save mode is enabled in all buffers where
`turn-on-magit-wip-save' would do it.
See `magit-wip-save-mode' for more information on Magit-Wip-Save mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("git-commit.el" "magit-backup.el" "magit-core.el"
;;;;;;  "magit-git.el" "magit-mode.el" "magit-pkg.el" "magit-popup.el"
;;;;;;  "magit-process.el" "magit-section.el" "magit-utils.el" "with-editor.el")
;;;;;;  (21680 7694 148883 0))

;;;***

(provide 'magit-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-autoloads.el ends here
