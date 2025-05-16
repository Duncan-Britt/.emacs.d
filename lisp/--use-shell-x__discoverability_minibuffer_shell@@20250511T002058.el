;;; --use-shell-x__discoverability_minibuffer_shell@@20250511T002058.el --- use-shellx -*- lexical-binding: t -*-

;;; Commentary:
;; title: use-shell-x
;; keywords: :discoverability:minibuffer:shell:
;; date: [2025-05-11 Sun 00:20]
;; identifier: 20250511T002058

;;; Code:
(use-package-local-or-remote
 shell-x
 "~/code/my-emacs-packages/shell-x/"
 "Duncan-Britt/shell-x"
 :bind
 (("s-e" . shell-x-execute-command)
  ("s-c" . shell-x-compile))
 :custom
 (shell-x-debug-logging t)
 :config
 (define-key project-prefix-map "c" #'shell-x-project-compile)
 (define-key project-prefix-map "e" #'shell-x-project-execute-command) ;; NOTE: e would normally be for opening up eshell in the project root.

 (defvar my/shell-x-collect-command-info-system-prompt "You are an LLM tasked with formatting natural language man pages
and --help output of command line programs into Prolog DCGs.

Be sure to include switches and other args. For switches, if used with an equals =
be sure to include that in the annotation, for example, --repository=REPOSITORY
would include =REPOSITORY in the annotation:
git_flag_completion --> [[\"--repository\", \"=REPOSITORY Path to repository to operate on\"]]

Your task will require great care because of the diversity of different formats of the
manuals and inconsistencies. Use your good judgement.

An Full Example:
Input:
jj(1)                       General Commands Manual                      jj(1)

NAME
       jj - Jujutsu (An experimental VCS)

SYNOPSIS
       jj [-R|--repository] [--ignore-working-copy] [--ignore-immutable]
       [--at-operation] [--debug] [--color] [--quiet] [--no-pager] [--config]
       [--config-file] [-h|--help] [-V|--version] [subcommands]

DESCRIPTION
       Jujutsu (An experimental VCS)

       To get started, see the tutorial [`jj help -k tutorial`].

       [`jj help -k tutorial`]: https://jj-vcs.github.io/jj/latest/tutorial/

OPTIONS
       -R, --repository=REPOSITORY
              Path to repository to operate on

              By default, Jujutsu searches for the closest .jj/ directory in
              an ancestor of the current working directory.

       --ignore-working-copy
              Don't snapshot the working copy, and don't update it

              By default, Jujutsu snapshots the working copy at the beginning
              of every command. The working copy is also updated at the end of
              the command, if the command modified the working-copy commit
              (`@`). If you want to avoid snapshotting the working copy and
              instead see a possibly stale working-copy commit, you can use
              `--ignore-working-copy`. This may be useful e.g. in a command
              prompt, especially if you have another process that commits the
              working copy.

              Loading the repository at a specific operation with
              `--at-operation` implies `--ignore-working-copy`.

       --ignore-immutable
              Allow rewriting immutable commits

              By default, Jujutsu prevents rewriting commits in the configured
              set of immutable commits. This option disables that check and
              lets you rewrite any commit but the root commit.

              This option only affects the check. It does not affect the
              `immutable_heads()` revset or the `immutable` template keyword.

       --at-operation=AT_OPERATION
              Operation to load the repo at

              Operation to load the repo at. By default, Jujutsu loads the
              repo at the most recent operation, or at the merge of the
              divergent operations if any.

              You can use `--at-op=<operation ID>` to see what the repo looked
              like at an earlier operation. For example `jj --at-op=<operation
              ID> st` will show you what `jj st` would have shown you when the
              given operation had just finished. `--at-op=@` is pretty much
              the same as the default except that divergent operations will
              never be merged.

              Use `jj op log` to find the operation ID you want. Any
              unambiguous prefix of the operation ID is enough.

              When loading the repo at an earlier operation, the working copy
              will be ignored, as if `--ignore-working-copy` had been
              specified.

              It is possible to run mutating commands when loading the repo at
              an earlier operation. Doing that is equivalent to having run
              concurrent commands starting at the earlier operation. There's
              rarely a reason to do that, but it is possible.

       --debug
              Enable debug logging

       --color=WHEN
              When to colorize output

              [possible values: always, never, debug, auto]

       --quiet
              Silence non-primary command output

              For example, `jj file list` will still list files, but it won't
              tell you if the working copy was snapshotted or if descendants
              were rebased.

              Warnings and errors will still be printed.

       --no-pager
              Disable the pager

       --config=NAME=VALUE
              Additional configuration options (can be repeated)

              The name should be specified as TOML dotted keys. The value
              should be specified as a TOML expression. If string value isn't
              enclosed by any TOML constructs (such as array notation), quotes
              can be omitted.

       --config-file=PATH
              Additional configuration files (can be repeated)

       -h, --help
              Print help (see a summary with '-h')

       -V, --version
              Print version

SUBCOMMANDS
       jj-abandon(1)
              Abandon a revision

       jj-absorb(1)
              Move changes from a revision into the stack of mutable revisions

       jj-bookmark(1)
              Manage bookmarks [default alias: b]

       jj-commit(1)
              Update the description and create a new change on top

       jj-config(1)
              Manage config options

       jj-describe(1)
              Update the change description or other metadata

       jj-diff(1)
              Compare file contents between two revisions

       jj-diffedit(1)
              Touch up the content changes in a revision with a diff editor

       jj-duplicate(1)
              Create new changes with the same content as existing ones

       jj-edit(1)
              Sets the specified revision as the working-copy revision

       jj-evolog(1)
              Show how a change has evolved over time

       jj-file(1)
              File operations

       jj-fix(1)
              Update files with formatting fixes or other changes

       jj-git(1)
              Commands for working with Git remotes and the underlying Git
              repo

       jj-help(1)
              Print this message or the help of the given subcommand(s)

       jj-interdiff(1)
              Compare the changes of two commits

       jj-log(1)
              Show revision history

       jj-new(1)
              Create a new, empty change and (by default) edit it in the
              working copy

       jj-next(1)
              Move the working-copy commit to the child revision

       jj-operation(1)
              Commands for working with the operation log

       jj-parallelize(1)
              Parallelize revisions by making them siblings

       jj-prev(1)
              Change the working copy revision relative to the parent revision

       jj-rebase(1)
              Move revisions to different parent(s)

       jj-resolve(1)
              Resolve conflicted files with an external merge tool

       jj-restore(1)
              Restore paths from another revision

       jj-revert(1)
              Apply the reverse of the given revision(s)

       jj-root(1)
              Show the current workspace root directory (shortcut for `jj
              workspace root`)

       jj-show(1)
              Show commit description and changes in a revision

       jj-sign(1)
              Cryptographically sign a revision

       jj-simplify-parents(1)
              Simplify parent edges for the specified revision(s)

       jj-sparse(1)
              Manage which paths from the working-copy commit are present in
              the working copy

       jj-split(1)
              Split a revision in two

       jj-squash(1)
              Move changes from a revision into another revision

       jj-status(1)
              Show high-level repo status

       jj-tag(1)
              Manage tags

       jj-util(1)
              Infrequently used commands such as for generating shell
              completions

       jj-undo(1)
              Undo an operation (shortcut for `jj op undo`)

       jj-unsign(1)
              Drop a cryptographic signature

       jj-version(1)
              Display version information

       jj-workspace(1)
              Commands for working with workspaces

EXTRA
       'jj help --help' lists available keywords. Use 'jj help -k' to show
       help for one of these keywords.

VERSION
       v0.28.2

                                   jj 0.28.2                             jj(1)

OUTPUT:
jj_completion --> [[jj, \"Jujutsu (An experimental VCS)\"]].
jj_completion --> [jj], jj_subcommand_completion.

jj_subcommand(abandon, \"Abandon a revision\").
jj_subcommand(absorb, \"Move changes from revision into the stack of mutable revisions\").
jj_subcommand(bookmark, \"Manage bookmarks [default alias: b]\").
jj_subcommand(commit, \"Update the description and create a new change on top\").
jj_subcommand(config, \"Manage config options\").
jj_subcommand(describe, \"Update the change description or other metadata\").
jj_subcommand(diff, \"Compare file contents between two revisions\").
jj_subcommand(diffedit, \"Touch up the content changes in a revision with a diff editor\").
jj_subcommand(duplicate, \"Create new changes with the same content as existing ones\").
jj_subcommand(edit, \"Sets the specified revision as the working-copy revision\").
jj_subcommand(evolog, \"Show how a change has evolved over time\").
jj_subcommand(file, \"File operations\").
jj_subcommand(fix, \"Update files with formatting fixes or other changes\").
jj_subcommand(git, \"Commands for working with Git remotes and the underlying Git repo\").
jj_subcommand(help, \"Print this message or the help of the given subcommand(s)\").
jj_subcommand(interdiff, \"Compare the changes of two commits\").
jj_subcommand(log, \"Show revision history\").
jj_subcommand(new, \"Create a new, empty change and edit it in the working copy\").
jj_subcommand(next, \"Move the working-copy commit to the child revision\").
jj_subcommand(operation, \"Commands for working with the operation log\").
jj_subcommand(parallelize, \"Parallelize revisions by making them siblings\").
jj_subcommand(prev, \"Change the working copy revision relative to the parent revision\").
jj_subcommand(rebase, \"Move revisions to different parent(s)\").
jj_subcommand(resolve, \"Resolve conflicted files with an external merge tool\").
jj_subcommand(restore, \"Restore paths from another revision\").
jj_subcommand(revert, \"Apply the reverse of the given revision(s)\").
jj_subcommand(root, \"Show the current workspace root directory\").
jj_subcommand(show, \"Show commit description and changes in a revision\").
jj_subcommand(sign, \"Cryptographically sign a revision\").
jj_subcommand('simplify-parents', \"Simplify parent edges for the specified revision(s)\").
jj_subcommand(sparse, \"Manage which paths from the working-copy commit are present in the working copy\").
jj_subcommand(split, \"Split a revision in two\").
jj_subcommand(squash, \"Move changes from a revision into another revision\").
jj_subcommand(status, \"Show high-level repo status\").
jj_subcommand(tag, \"Manage tags\").
jj_subcommand(util, \"Infrequently used commands such as for generating shell completions\").
jj_subcommand(undo, \"Undo an operation\").
jj_subcommand(unsign, \"Drop a cryptographic signature\").
jj_subcommand(version, \"Display version information\").
jj_subcommand(workspace, \"Commands for working with workspaces\").

jj_subcommand_completion -->
    { jj_subcommand(Command, Description) },
    [[Command, Description]].
jj_subcommand_completion -->
    { jj_subcommand(Command, _),
      \+ jj_custom_completion(Command, _) },
    [Command],
    jj_flag_completion.
jj_subcommand_completion -->
    { jj_subcommand(Command, _),
      jj_custom_completion(Command, CompletionRule) },
    [Command],
    CompletionRule.

jj_global_flag('-R=', \"Path to repository to operate on\").
jj_global_flag('--repository=', \"Path to repository to operate on\").
jj_global_flag('--ignore-working-copy', \"Don't snapshot the working copy, and don't update it\").
jj_global_flag('--ignore-immutable', \"Allow rewriting immutable commits\").
jj_global_flag('--at-operation=', \"Operation to load the repo at\").
jj_global_flag('--debug', \"Enable debug logging\").
jj_global_flag('--color=always', \"Always colorize output\").
jj_global_flag('--color=never', \"Never colorize output\").
jj_global_flag('--color=debug', \"When to colorize output\").
jj_global_flag('--color=auto', \"When to colorize output\").
jj_global_flag('--quiet', \"Silence non-primary command output\").
jj_global_flag('--no-pager', \"Disable the pager\").
jj_global_flag('--config=', \"Additional configuration options\").
jj_global_flag('--config-file=', \"Additional configuration files\").
jj_global_flag('-h', \"Print help\").
jj_global_flag('--help', \"Print help\").
jj_global_flag('-V', \"Print version\").
jj_global_flag('--version', \"Print version\").

jj_flag_completion -->
    { jj_global_flag(Flag, Description) },
    [[Flag, Description]].
jj_flag_completion -->
    [_],
    jj_flag_completion.

INPUT:
swipl --help
swipl: Usage:
    1) swipl [options] prolog-file ... [-- arg ...]
    2) swipl [options] [-o executable] -c prolog-file ...
    3) swipl app ...        Use \"swipl app list\" for available apps
    4) swipl --help         Display this message
    5) swipl --version      Display version information
    6) swipl --abi-version  Display ABI version key
    7) swipl --arch         Display architecture
    8) swipl --dump-runtime-variables[=format]
                            Dump link info in sh(1) format

Options:
    -x state                 Start from state (must be first)
    -g goal                  Run goal (may be repeated)
    -t toplevel              Toplevel goal
    -f file                  User initialisation file
    -F file                  Site initialisation file
    -l file                  Script source file
    -s file                  Script source file
    -p alias=path            Define file search path 'alias'
    -D name=value            Set a Prolog flag
    -O                       Optimised compilation
    --on-error=style         One of print, halt or status
    --on-warning=style       One of print, halt or status
    --tty[=bool]             (Dis)allow tty control
    --packs[=bool]           Do (not) attach add-ons
    --signals[=bool]         Do (not) modify signal handling
    --sigalert[=num]         Use signal num for alerting threads
    --threads[=bool]         Do (not) allow for threads
    --debug[=bool]           Do (not) generate debug info
    --debug-on-interrupt[=bool] Trap the debugger on interrupt
    --quiet[=bool] (-q)      Do (not) suppress informational messages
    --traditional            Disable extensions of version 7
    --home[=DIR]             Print home or use DIR as SWI-Prolog home
    --stack-limit=size[BKMG] Specify maximum size of Prolog stacks
    --table-space=size[BKMG] Specify maximum size of SLG tables
    --shared-table-space=size[BKMG] Maximum size of shared SLG tables
    --pce[=bool]             Make the xpce gui available
    --pldoc[=port]           Start PlDoc server [at port]

Boolean options may be written as --name=bool, --name, --no-name or --noname.
Both '-' or '_' are accepted as word-separator for long options.

OUTPUT:
swipl_completion --> [[swipl, \"SWI-Prolog command line interface\"]].
swipl_completion --> [swipl], swipl_flag_completion.
swipl_completion --> [swipl], swipl_app_list_completion.

swipl_app_list_completion -->
    [['app list', \"Use \\\"swipl app list\\\" for available apps\"]].
swipl_app_list_completion -->
    ['app list'],
    swipl_app_list_flag_completion.

swipl_app_list_flag_completion -->
    { swipl_app_list_flag(Flag, Description) },
    [[Flag, Description]].
swipl_app_list_flag_completion -->
    [_],
    swipl_app_list_flag_completion.

swipl_app_list_help_flag('-h').
swipl_app_list_help_flag('-?').
swipl_app_list_help_flag('--help').

swipl_app_list_long_flag('-l').
swipl_app_list_long_flag('-long').

swipl_app_list_flag(X, \"Show this help message and exit\") :-
    swipl_app_list_help_flag(X).
swipl_app_list_flag(X, \"Long format\") :-
    swipl_app_list_long_flag(X).

swipl_flag('-x', \"Start from state (must be first)\").
swipl_flag('-g', \"Run goal (may be repeated)\").
swipl_flag('-t', \"Toplevel goal\").
swipl_flag('-f', \"User initialisation file\").
swipl_flag('-F', \"Site initialisation file\").
swipl_flag('-l', \"Script source file\").
swipl_flag('-s', \"Script source file\").
swipl_flag('-p', \"Define file search path 'alias'\").
swipl_flag('-D', \"Set a Prolog flag\").
swipl_flag('-O', \"Optimised compilation\").
swipl_flag('--on-error=print', \"Print on error\").
swipl_flag('--on-error=halt', \"Halt on error\").
swipl_flag('--on-error=status', \"Status on error\").
swipl_flag('--on-warning=print', \"Print on warning\").
swipl_flag('--on-warning=halt', \"Halt on warning\").
swipl_flag('--on-warning=status', \"Status on warning\").
swipl_flag('--tty', \"Allow tty control\").
swipl_flag('--no-tty', \"Disallow tty control\").
swipl_flag('--packs', \"Attach add-ons\").
swipl_flag('--no-packs', \"Do not attach add-ons\").
swipl_flag('--signals', \"Modify signal handling\").
swipl_flag('--no-signals', \"Do not modify signal handling\").
swipl_flag('--sigalert=', \"Use signal num for alerting threads\").
swipl_flag('--threads', \"Allow for threads\").
swipl_flag('--no-threads', \"Do not allow for threads\").
swipl_flag('--debug', \"Generate debug info\").
swipl_flag('--no-debug', \"Do not generate debug info\").
swipl_flag('--debug-on-interrupt', \"Trap the debugger on interrupt\").
swipl_flag('--no-debug-on-interrupt', \"Do not trap the debugger on interrupt\").
swipl_flag('--quiet', \"Suppress informational messages\").
swipl_flag('-q', \"Suppress informational messages\").
swipl_flag('--no-quiet', \"Do not suppress informational messages\").
swipl_flag('--traditional', \"Disable extensions of version 7\").
swipl_flag('--home=', \"Use DIR as SWI-Prolog home\").
swipl_flag('--home', \"Print home directory\").
swipl_flag('--stack-limit=', \"Specify maximum size of Prolog stacks\").
swipl_flag('--table-space=', \"Specify maximum size of SLG tables\").
swipl_flag('--shared-table-space=', \"Maximum size of shared SLG tables\").
swipl_flag('--pce', \"Make the xpce gui available\").
swipl_flag('--no-pce', \"Do not make the xpce gui available\").
swipl_flag('--pldoc=', \"Start PlDoc server at specified port\").
swipl_flag('--pldoc', \"Start PlDoc server\").
swipl_flag('--help', \"Display help message\").
swipl_flag('--version', \"Display version information\").
swipl_flag('--abi-version', \"Display ABI version key\").
swipl_flag('--arch', \"Display architecture\").
swipl_flag('--dump-runtime-variables', \"Dump link info in sh(1) format\").
swipl_flag('-o', \"Output executable (with -c)\").
swipl_flag('-c', \"Compile Prolog files\").

swipl_flag_completion -->
    { swipl_flag(Flag, Description) },
    [[Flag, Description]].
swipl_flag_completion -->
    [_],
    swipl_flag_completion.


INPUT:
"
   "Start of LLM prompt for shell command info.
See `my/shell-x-collect-command-info'.")

 (defun my/shell-x-collect-command-info ()
   "Alistify shell command info from buffer."
   (interactive)
   (let ((llm-prompt (buffer-substring-no-properties (point-min) (point-max)))
         (buffer (get-buffer-create (shell-x--unique-buffer-name "Collect Command Info"))))
     (pop-to-buffer buffer)
     (insert my/shell-x-collect-command-info-system-prompt)
     (insert llm-prompt)
     (gptel-send))))

(provide '--use-shell-x__discoverability_minibuffer_shell@@20250511T002058)
;;; --use-shell-x__discoverability_minibuffer_shell@@20250511T002058.el ends here
