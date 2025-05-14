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
jj_completion --> [[\"Jujutsu (An experimental VCS)\"]].
jj_completion --> jj_flag_completion.
jj_completion --> jj_subcommand_completion.

jj_flag_completion --> [[\"-R\", \"Path to repository to operate on\"]].
jj_flag_completion --> [[\"--repository\", \"Path to repository to operate on\"]].
jj_flag_completion --> [[\"--ignore-working-copy\", \"Don't snapshot the working copy, and don't update it\"]].
jj_flag_completion --> [[\"--ignore-immutable\", \"Allow rewriting immutable commits\"]].
jj_flag_completion --> [[\"--at-operation\", \"Operation to load the repo at\"]].
jj_flag_completion --> [[\"--debug\", \"Enable debug logging\"]].
jj_flag_completion --> [[\"--color\", \"When to colorize output\"]].
jj_flag_completion --> [[\"--quiet\", \"Silence non-primary command output\"]].
jj_flag_completion --> [[\"--no-pager\", \"Disable the pager\"]].
jj_flag_completion --> [[\"--config\", \"Additional configuration options\"]].
jj_flag_completion --> [[\"--config-file\", \"Additional configuration files\"]].
jj_flag_completion --> [[\"-h\", \"Print help\"]].
jj_flag_completion --> [[\"--help\", \"Print help\"]].
jj_flag_completion --> [[\"-V\", \"Print version\"]].
jj_flag_completion --> [[\"--version\", \"Print version\"]].

jj_subcommand_completion --> [[\"abandon\", \"Abandon a revision\"]].
jj_subcommand_completion --> [[\"absorb\", \"Move changes from revision into the stack of mutable revisions\"]].
jj_subcommand_completion --> [[\"bookmark\", \"Manage bookmarks [default alias: b]\"]].
jj_subcommand_completion --> [[\"commit\", \"Update the description and create a new change on top\"]].
jj_subcommand_completion --> [[\"config\", \"Manage config options\"]].
jj_subcommand_completion --> [[\"describe\", \"Update the change description or other metadata\"]].
jj_subcommand_completion --> [[\"diff\", \"Compare file contents between two revisions\"]].
jj_subcommand_completion --> [[\"diffedit\", \"Touch up the content changes in a revision with a diff editor\"]].
jj_subcommand_completion --> [[\"duplicate\", \"Create new changes with the same content as existing ones\"]].
jj_subcommand_completion --> [[\"edit\", \"Sets the specified revision as the working-copy revision\"]].
jj_subcommand_completion --> [[\"evolog\", \"Show how a change has evolved over time\"]].
jj_subcommand_completion --> [[\"file\", \"File operations\"]].
jj_subcommand_completion --> [[\"fix\", \"Update files with formatting fixes or other changes\"]].
jj_subcommand_completion --> [[\"git\", \"Commands for working with Git remotes and the underlying Git repo\"]].
jj_subcommand_completion --> [[\"help\", \"Print this message or the help of the given subcommand(s)\"]].
jj_subcommand_completion --> [[\"interdiff\", \"Compare the changes of two commits\"]].
jj_subcommand_completion --> [[\"log\", \"Show revision history\"]].
jj_subcommand_completion --> [[\"new\", \"Create a new, empty change and edit it in the working copy\"]].
jj_subcommand_completion --> [[\"next\", \"Move the working-copy commit to the child revision\"]].
jj_subcommand_completion --> [[\"operation\", \"Commands for working with the operation log\"]].
jj_subcommand_completion --> [[\"parallelize\", \"Parallelize revisions by making them siblings\"]].
jj_subcommand_completion --> [[\"prev\", \"Change the working copy revision relative to the parent revision\"]].
jj_subcommand_completion --> [[\"rebase\", \"Move revisions to different parent(s)\"]].
jj_subcommand_completion --> [[\"resolve\", \"Resolve conflicted files with an external merge tool\"]].
jj_subcommand_completion --> [[\"restore\", \"Restore paths from another revision\"]].
jj_subcommand_completion --> [[\"revert\", \"Apply the reverse of the given revision(s)\"]].
jj_subcommand_completion --> [[\"root\", \"Show the current workspace root directory\"]].
jj_subcommand_completion --> [[\"show\", \"Show commit description and changes in a revision\"]].
jj_subcommand_completion --> [[\"sign\", \"Cryptographically sign a revision\"]].
jj_subcommand_completion --> [[\"simplify-parents\", \"Simplify parent edges for the specified revision(s)\"]].
jj_subcommand_completion --> [[\"sparse\", \"Manage which paths from the working-copy commit are present in the working copy\"]].
jj_subcommand_completion --> [[\"split\", \"Split a revision in two\"]].
jj_subcommand_completion --> [[\"squash\", \"Move changes from a revision into another revision\"]].
jj_subcommand_completion --> [[\"status\", \"Show high-level repo status\"]].
jj_subcommand_completion --> [[\"tag\", \"Manage tags\"]].
jj_subcommand_completion --> [[\"util\", \"Infrequently used commands such as for generating shell completions\"]].
jj_subcommand_completion --> [[\"undo\", \"Undo an operation\"]].
jj_subcommand_completion --> [[\"unsign\", \"Drop a cryptographic signature\"]].
jj_subcommand_completion --> [[\"version\", \"Display version information\"]].
jj_subcommand_completion --> [[\"workspace\", \"Commands for working with workspaces\"]].

INPUT:
jj-commit(1)                General Commands Manual               jj-commit(1)

NAME
       jj-commit - Update the description and create a new change on top

SYNOPSIS
       jj commit [-i|--interactive] [--tool] [-m|--message] [--reset-author]
       [--author] [-R|--repository] [--ignore-working-copy]
       [--ignore-immutable] [--at-operation] [--debug] [--color] [--quiet]
       [--no-pager] [--config] [--config-file] [-h|--help] [FILESETS]

DESCRIPTION
       Update the description and create a new change on top

OPTIONS
       -i, --interactive
              Interactively choose which changes to include in the first
              commit

       --tool=NAME
              Specify diff editor to be used (implies --interactive)

       -m, --message=MESSAGE
              The change description to use (don't open editor)

       --reset-author
              Reset the author to the configured user

              This resets the author name, email, and timestamp.

              You can use it in combination with the JJ_USER and JJ_EMAIL
              environment variables to set a different author:

              $ JJ_USER='Foo Bar' JJ_EMAIL=foo@bar.com jj commit
              --reset-author

       --author=AUTHOR
              Set author to the provided string

              This changes author name and email while retaining author
              timestamp for non-discardable commits.

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

       [FILESETS]
              Put these paths in the first commit

                                    commit                        jj-commit(1)


OUTPUT:

jj_subcommand_completion --> [\"commit\"], jj_commit_completion.
jj_commit_completion --> [[\"-i\", \"Interactively choose which changes to include in the first commit\"]].
jj_commit_completion --> [[\"--interactive\", \"Interactively choose which changes to include in the first commit\"]].
jj_commit_completion --> [[\"--tool\", \"Specify diff editor to be used (implies --interactive)\"]].
jj_commit_completion --> [[\"-m\", \"The change description to use (don't open editor)\"]].
jj_commit_completion --> [[\"--message\", \"The change description to use (don't open editor)\"]].
jj_commit_completion --> [[\"--reset-author\", \"Reset the author to the configured user\"]].
jj_commit_completion --> [[\"--author\", \"Set author to the provided string\"]].
jj_commit_completion --> [[\"-R\", \"Path to repository to operate on\"]].
jj_commit_completion --> [[\"--repository\", \"Path to repository to operate on\"]].
jj_commit_completion --> [[\"--ignore-working-copy\", \"Don't snapshot the working copy, and don't update it\"]].
jj_commit_completion --> [[\"--ignore-immutable\", \"Allow rewriting immutable commits\"]].
jj_commit_completion --> [[\"--at-operation\", \"Operation to load the repo at\"]].
jj_commit_completion --> [[\"--debug\", \"Enable debug logging\"]].
jj_commit_completion --> [[\"--color\", \"When to colorize output\"]].
jj_commit_completion --> [[\"--quiet\", \"Silence non-primary command output\"]].
jj_commit_completion --> [[\"--no-pager\", \"Disable the pager\"]].
jj_commit_completion --> [[\"--config\", \"Additional configuration options (can be repeated)\"]].
jj_commit_completion --> [[\"--config-file\", \"Additional configuration files (can be repeated)\"]].
jj_commit_completion --> [[\"-h\", \"Print help\"]].
jj_commit_completion --> [[\"--help\", \"Print help\"]].
"
   "Start of LLM prompt for shell command info.
See `my/shell-x-collect-command-info'.")

 (defun my/shell-x-collect-command-info ()
   "Alistify shell command info from buffer."
   (interactive)
   (let ((llm-prompt (buffer-substring-no-properties (point-min) (point-max)))
         (gptel--system-message my/shell-x-collect-command-info-system-prompt) ;; WARN: Internal variable, may not work for future versions of GPTEL.
         (buffer (get-buffer-create (shell-x--unique-buffer-name "Collect Command Info"))))
     (pop-to-buffer buffer)
     (insert llm-prompt)
     (gptel-send))))

(provide '--use-shell-x__discoverability_minibuffer_shell@@20250511T002058)
;;; --use-shell-x__discoverability_minibuffer_shell@@20250511T002058.el ends here
