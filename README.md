<!--
SPDX-FileCopyrightText: The aro Authors
SPDX-License-Identifier: 0BSD
-->

# aro

aro lists the repositories of GitHub organizations and users, GitLab groups and Forgejo instances, clones them in one of two layouts, keeps every clone up to date without losing local work, and generates [airmux](https://github.com/dermoumi/airmux) sessions and [ttt](https://github.com/eugenioenko/ttt) workspaces for them. It runs on [babashka](https://babashka.org) and calls `git`, `curl` and, for tunnels, `ssh`.

## Commands

```shell
aro fetch [SOURCE...]                       # list the sources' repositories into the inventory
aro list                                    # print every repository with layout, branch and directory
aro generate [--out DIR]                    # write the airmux sessions and ttt workspaces
aro compare [--toml FILE...]                # compare with the files on disk, print suggested config
aro clone [REPO...] [--max N]               # clone what is missing, one at a time, at most N
aro update [REPO...] [--jobs N]             # fetch and update every clone
aro maintain --schedule S [REPO...]         # run git's maintenance tasks for hourly, daily or weekly
aro prune [REPO...] [--dry-run]             # delete local branches origin's default branch holds
aro orphans [--delete]                      # list clones no repository names, remove those without local work
aro layout REPO worktrees|plain [--keep B]  # switch a clone between the layouts
```

`aro clone --max N` clones at most N missing repositories and leaves the rest to the next run, so a timer works through a large backlog over several days instead of saturating the link in one go. `--delay SECONDS`, or `:delay` in the config, waits between two clones.

`REPO` is an id such as `github.com:metio/aro`, a clone URL, or a directory inside a clone, so `aro layout . worktrees` works from within a repository. The config is read from `$XDG_CONFIG_HOME/aro/config.edn` unless `--config` names another file.

## Fetching and the inventory

`aro fetch` is the only command that talks to a forge. It writes what each source listed to the inventory file, and every other command reads that file. A source that fails keeps the records of its last successful fetch, so a forge that is unreachable for the moment changes nothing. Several machines can share one inventory: point `:inventory` at a file in a repository they all pull, and let one machine run `aro fetch` with a `:fetched` hook that commits and pushes it.

```clojure
{:inventory "/home/me/dotfiles/aro-inventory.edn"
 :hooks {:fetched ["/home/me/.local/bin/git-commit-and-push-file" "{inventory}" "chore: update the aro inventory"]}}
```

Comment lines at the top of the inventory, such as a license header, survive a rewrite.

## Sources

```clojure
{:sources
 [{:id "metio" :github {:org "metio"} :token-command ["gh" "auth" "token"]}
  {:id "me" :github {:user "sebhoss"}}
  {:id "codeberg" :forgejo {:url "https://codeberg.org" :user "sebhoss"} :session-dir "me"}
  {:id "work" :gitlab {:url "https://git.example.org" :group "team"}
   :token-command ["passage" "show" "git.example.org/token"]
   :tunnel {:ssh "admin@git.example.org" :target "localhost:443"}
   :layout :worktrees}]}
```

| Key | Meaning |
| --- | --- |
| `:github`, `:gitlab`, `:forgejo` | The forge and the organization, user or group to list: `:org` or `:user` on GitHub and Forgejo, `:group` or `:user` on GitLab, or `:membership true` for every GitLab project the token's account is a member of. GitLab groups include their subgroups. |
| `:token-command` | A command printing a token on its first line, for private repositories. A `:user` with a token lists the repositories the token's account owns, private ones included; a token of another account lists none. |
| `:tunnel` | An SSH port forward for a forge API that only listens on its own host. |
| `:host` | The host recorded for the repositories, when the URL differs from the host used for cloning. |
| `:archived`, `:forks` | `true` keeps archived repositories or forks, which are left out otherwise. Disabled GitHub repositories, which cannot be cloned, are always left out. |
| `:layout` | The layout of the source's clones, overriding the top-level `:layout` (default `:plain`). |
| `:root` | The namespace session files are relative to. Defaults to the organization, user or group. |
| `:session-dir` | The directory below the airmux directory for the source's sessions. Defaults to the id. |

A `:tunnel` runs `ssh -N -L 127.0.0.1:PORT:TARGET SSH` on a free local port for the duration of the fetch, and curl reaches the forge's URL through `--connect-to`. The request keeps the forge's host name, so TLS still checks the forge's own certificate. The SSH destination must be an account that may forward ports; the `git` account of a forge usually may not.

## Repositories

Overrides go in `:repositories`, keyed by id; repositories the forges do not list go in `:add`.

```clojure
{:repositories {"github.com:metio/aro" {:layout :worktrees :branch "main"}
                "github.com:sebhoss/dotfiles" {:dir "~/.local/share/chezmoi"
                                               :session {:pane_commands ["alias s='nix develop'"]}}
                "github.com:metio/old" {:include true}
                "github.com:metio/noise" {:exclude true}}
 :add [{:id "gitlab.com:someone/project" :source "me" :default-branch "master"}]}
```

`:dir` is absolute, starts with `~`, or is relative to `:git-root` (default `~/git`). Without one, a clone lives at `<git-root>/<host>/<path>`. `:mirrors` lists the ids of copies of the repository on other forges, such as `["gitlab.com:someone/project" "codeberg.org:someone/project"]`: `aro clone` and `aro update` set `remote.origin.pushurl` to the repository and each mirror, so one `git push` reaches all of them, while fetches still come from the repository alone. A mirror gets no clone or session of its own, even when a source lists it. Without `:mirrors`, aro removes any push URL of `origin`. `:session` replaces keys of the generated session, a `nil` value removes one, and `:session false` generates none. `:session-file` moves the session to another path below the airmux directory.

## Groups and custom sessions

A group collects repositories into a ttt workspace and, with `:session`, an airmux session opening it.

```clojure
{:groups [{:id "helm-charts"
           :match [{:namespace "team/helm-charts"} {:topic "helm"}]
           :include ["git.example.org:team/tools/chart-tests"]
           :exclude ["git.example.org:team/helm-charts/attic"]
           :dirs ["~/notes/helm"]
           :ttt "team-helm-charts.ttt"
           :session {:file "work/helm-charts/all.yml" :session_name "team helm charts"}}]
 :sessions {"metio/jaas.yml" {:session_name "jaas" :windows [...]}}}
```

Each map in `:match` selects the repositories for which all its keys hold: `:namespace`, `:namespace-prefix`, `:topic`, `:host` or `:source`. `:dirs` adds folders that are no repository to the workspace, resolved like a repository's `:dir`. `:sessions` are written as they are, for sessions spanning several repositories.

## Layouts

The plain layout is an ordinary clone. The worktrees layout is a bare repository in `.git` with a worktree per branch next to it: the default branch tracking its upstream, and `development` started from it without an upstream.

`aro layout REPO worktrees` moves the checkout as it is into a worktree named after its branch, with uncommitted changes, the index, untracked and ignored files included, and adds the default branch and `development` next to it. `aro layout REPO plain` keeps the worktree of the default branch, or of `--keep`, as the plain clone. The other worktrees are removed and their branches remain. A worktree to remove that holds uncommitted changes, untracked or ignored files refuses the switch before anything changes, and the error lists those files. Both directions refuse a clone with a merge or rebase in progress, and switching to worktrees refuses one with submodules. Set the repository's `:layout` in the config afterwards, so sessions and new clones follow.

## Orphans

`aro orphans` lists every clone below `:git-root` that is neither the clone of a configured repository nor inside a group's `:dirs`, such as clones of repositories that were archived, moved or deleted on their forge. Each line names what the clone holds only locally. `aro orphans --delete` removes the orphans holding nothing of that kind, along with the directories they leave empty, and keeps the rest with the reason. A clone counts as holding local work when a checkout has uncommitted changes or untracked files, when it has stash entries, when a branch or tag holds commits no remote-tracking branch holds, or when git cannot read it. Ignored files do not count, so build output and editor settings are removed with the clone.

## Updating

`aro update` fetches every clone and brings each checked-out branch up to date with its upstream, or, for the default branch and `development` without an upstream, with the default branch of `origin`. It never loses local work:

- A branch without commits of its own is fast-forwarded. Git refuses when uncommitted changes touch a file the update changes, and the branch is left as it is.
- A branch with commits of its own is rebased, with uncommitted changes stashed around the rebase. When those changes touch a file the update changes, or the rebase conflicts, the branch is left as it was.
- A detached HEAD, a merge or rebase in progress, a branch without upstream, and a branch whose upstream is gone are skipped.

Skipped branches print `[SKIP]` with the reason. A failed fetch prints `[ERROR]` and makes the command exit non-zero, which suits a systemd timer with `OnFailure=`. Git runs with `GIT_TERMINAL_PROMPT=0` and, unless `GIT_SSH_COMMAND` is set, `ssh -o BatchMode=yes`, so a run without a terminal fails instead of waiting for input.

## Maintenance and pruning

`aro maintain --schedule hourly|daily|weekly` runs `git maintenance run` for that schedule in every clone, with the tasks of git's incremental strategy except `prefetch`, which `aro update` makes redundant: `commit-graph` hourly, `loose-objects` and `incremental-repack` daily, and `pack-refs` weekly. Without a strategy, a scheduled run has no tasks, and aro sets it for the run instead of in each clone's config. Clones that do not exist yet are skipped, so the list of clones comes from the config and the inventory alone. A templated systemd service running `aro maintain --schedule=%i`, started by a timer per schedule, keeps every clone maintained.

`aro prune` deletes the local branches whose commits the default branch of origin already holds, such as branches merged upstream and branches never committed to. The default branch, `development` and every branch a checkout has checked out stay. A branch whose upstream is gone but that holds commits origin lacks, such as one deleted after a squash merge, stays too and is reported with `[SKIP]`. `--dry-run` lists what would be deleted.

## Generating and comparing

`aro generate` writes a session per repository and group to `:airmux-dir` (default `$XDG_CONFIG_HOME/airmux`) and the workspaces to `:ttt-dir` (default `$XDG_DATA_HOME/ttt`). A manifest in the airmux directory records what aro wrote, so a later run deletes sessions it no longer generates and leaves other files alone.

`aro compare` reads the sessions and workspaces on disk and prints the config that would make aro generate them: layout overrides from the clones on disk, session keys that differ, files that match no repository, and group includes and excludes. A hand-written session naming directories that do not exist is reported as stale, and the generated keys are kept for it. `--toml` reads chezmoi data files listing repositories as `[[x_git_repos]]` tables with `upstream`, `worktrees`, `branch` and `local`.

## Development

```shell
nix develop --command bb test
```

## License

0BSD, see [LICENSES/0BSD.txt](LICENSES/0BSD.txt).
