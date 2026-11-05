;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.git
  "Clones in one of two layouts, updates that never lose local work, and
  switching a clone between the layouts.

  The plain layout is an ordinary clone. The worktrees layout is a bare
  repository in `.git` with a worktree per branch beside it: the default
  branch tracking its upstream, and `development` started from it without
  one."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]))

(def development "development")

(def ^:dynamic *environment*
  "Variables added to the environment of every git call."
  {})

(defn- environment []
  (merge (cond-> {"GIT_TERMINAL_PROMPT" "0"}
           (str/blank? (System/getenv "GIT_SSH_COMMAND")) (assoc "GIT_SSH_COMMAND" "ssh -o BatchMode=yes"))
         *environment*))

(defn git
  "Runs git with `args` and returns {:exit :out :err}."
  [& args]
  (apply process/shell {:out :string :err :string :continue true :extra-env (environment)} "git" (map str args)))

(defn- git!
  "Runs git with `args` and returns its trimmed output, throwing on failure."
  [& args]
  (let [{:keys [exit out err]} (apply git args)]
    (when-not (zero? exit)
      (throw (ex-info (str "git " (str/join " " args) ": " (str/trim (str err " " out)))
                      {:aro/problem :git})))
    (str/trim out)))

(defn- lines [s]
  (remove str/blank? (str/split-lines (or s ""))))

(defn- git-dir [dir]
  (str (fs/path dir ".git")))

(defn layout-on-disk
  "The layout of the clone at `dir`, or nil when there is none."
  [dir]
  (let [dot-git (fs/path dir ".git")]
    (cond
      (not (fs/directory? dot-git)) nil
      (= "true" (str/trim (:out (git "--git-dir" dot-git "config" "--bool" "core.bare")))) :worktrees
      :else :plain)))

(defn clone-url [{:keys [url host path]}]
  (or url (str host ":" path)))

(defn- remove-push-urls!
  "Removes the push URLs of origin, so that a push goes to its fetch URL
  alone. Mirrors receive their commits from `sync-mirrors!`."
  [dir]
  (let [gd (git-dir dir)]
    (when (seq (lines (:out (git "--git-dir" gd "config" "--get-all" "remote.origin.pushurl"))))
      (git! "--git-dir" gd "config" "--unset-all" "remote.origin.pushurl"))))

(defn- ensure-empty! [dir]
  (when (and (fs/exists? dir) (seq (fs/list-dir dir)))
    (throw (ex-info (str dir " is not empty") {:aro/problem :not-empty})))
  (fs/create-dirs dir))

(defn- add-development! [gd dir branch]
  (let [has-branch (zero? (:exit (git "--git-dir" gd "show-ref" "--verify" "--quiet" (str "refs/heads/" development))))]
    (if has-branch
      (git! "--git-dir" gd "worktree" "add" (fs/path dir development) development)
      (git! "--git-dir" gd "worktree" "add" "--no-track" "-b" development (fs/path dir development) (str "origin/" branch)))))

(defn- add-default! [gd dir branch]
  (let [has-branch (zero? (:exit (git "--git-dir" gd "show-ref" "--verify" "--quiet" (str "refs/heads/" branch))))]
    (if has-branch
      (git! "--git-dir" gd "worktree" "add" (fs/path dir branch) branch)
      (git! "--git-dir" gd "worktree" "add" "--track" "-b" branch (fs/path dir branch) (str "origin/" branch)))))

(defn- clone-into!
  "Clones `repository` into its empty :dir in its :layout."
  [{:keys [dir layout branch] :as repository}]
  (if (= :worktrees layout)
    (let [gd (git-dir dir)]
      (git! "clone" "--bare" "--quiet" (clone-url repository) gd)
      (git! "--git-dir" gd "config" "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*")
      (git! "--git-dir" gd "config" "core.logallrefupdates" "true")
      (git! "--git-dir" gd "fetch" "--quiet" "--no-recurse-submodules" "origin")
      (doseq [ref (lines (git! "--git-dir" gd "for-each-ref" "--format=%(refname)" "refs/heads"))]
        (git! "--git-dir" gd "update-ref" "-d" ref))
      (add-default! gd dir branch)
      (add-development! gd dir branch))
    (git! "clone" "--quiet" "--branch" branch (clone-url repository) dir)))

(defn clone!
  "Clones `repository` into its :dir in its :layout. Returns :cloned, or
  :exists when the directory already holds a clone. A clone that fails
  partway is removed, so the next run tries again instead of finding a
  clone without checkouts."
  [{:keys [dir] :as repository}]
  (if (layout-on-disk dir)
    :exists
    (do
      (ensure-empty! dir)
      (try
        (clone-into! repository)
        (catch Exception e
          (fs/delete-tree dir)
          (throw e)))
      :cloned)))

(declare worktrees)

(defn- strays
  "The entries of the worktrees layout at `dir` that are neither its .git nor
  one of `checkouts`."
  [dir checkouts]
  (let [dir (fs/canonicalize dir)
        paths (set (map :path checkouts))]
    (remove #(or (= ".git" (str (fs/file-name %))) (paths %)) (map str (fs/list-dir dir)))))

(defn local-work
  "What deleting the clone at `dir` would lose: uncommitted changes or
  untracked files in a checkout, stash entries, and commits no remote-tracking
  branch holds, whether a branch, a tag, a note or a detached HEAD reaches
  them. A git call that fails counts as local work, so a clone git
  cannot read is never taken for an empty one. Ignored files do not count."
  [dir]
  (let [run (fn [& args]
              (let [{:keys [exit out err]} (apply git "-C" (str dir) args)]
                (if (zero? exit) {:lines (vec (lines out))} {:failed (str "git " (first args) " fails: " (str/trim err))})))
        worktrees-layout (= :worktrees (layout-on-disk dir))
        checkouts (if worktrees-layout (map :path (worktrees dir)) [(str dir)])
        beside (when worktrees-layout (strays dir (worktrees dir)))
        dirty (for [checkout checkouts
                    :let [{:keys [exit out err]} (git "-C" checkout "status" "--porcelain")]
                    :when (or (not (zero? exit)) (seq (lines out)))]
                (if (zero? exit)
                  (str checkout " has uncommitted changes or untracked files")
                  (str "git status fails in " checkout ": " (str/trim err))))
        stash (if (seq (:lines (run "for-each-ref" "refs/stash")))
                (run "reflog" "show" "--format=%H" "refs/stash")
                {:lines []})
        unpushed (run "rev-list" "--exclude=refs/prefetch/*" "--exclude=refs/stash" "--all" "--not" "--remotes")]
    (cond-> (vec dirty)
      (seq beside) (conj (str dir " holds files outside its worktrees: " (str/join ", " (sort beside))))
      (:failed stash) (conj (:failed stash))
      (seq (:lines stash)) (conj (str (count (:lines stash)) " stash " (if (< 1 (count (:lines stash))) "entries" "entry")))
      (:failed unpushed) (conj (:failed unpushed))
      (seq (:lines unpushed)) (conj (str (count (:lines unpushed)) " commit" (when (< 1 (count (:lines unpushed))) "s") " on no remote-tracking branch")))))

(defn- worktrees
  "The checkouts of the worktrees layout at `dir`: [{:path :branch}]."
  [dir]
  (->> (str/split (git! "--git-dir" (git-dir dir) "worktree" "list" "--porcelain") #"\n\n")
       (map (fn [entry]
              (let [fields (into {} (for [line (lines entry)]
                                      (let [[k v] (str/split line #" " 2)] [k (or v true)])))]
                {:path (get fields "worktree")
                 :branch (some-> (get fields "branch") (str/replace-first "refs/heads/" ""))
                 :bare (contains? fields "bare")
                 :prunable (contains? fields "prunable")})))
       (remove #(or (:bare %) (:prunable %)))
       vec))

(defn- in-progress? [checkout]
  (some #(fs/exists? (git! "-C" checkout "rev-parse" "--path-format=absolute" "--git-path" %))
        ["rebase-merge" "rebase-apply" "MERGE_HEAD" "CHERRY_PICK_HEAD" "REVERT_HEAD"]))

(defn- local-paths
  "The paths with uncommitted changes in `checkout`, untracked ones included."
  [checkout]
  (->> (concat (str/split (:out (git "-C" checkout "diff" "--name-only" "--no-renames" "-z" "HEAD")) #"\u0000")
               (str/split (:out (git "-C" checkout "ls-files" "--others" "--exclude-standard" "-z")) #"\u0000"))
       (remove str/blank?)
       set))

(defn- ignored-in-the-way
  "The ignored files of `checkout` that `target` starts tracking, which a
  fast-forward or rebase onto it would overwrite."
  [checkout target]
  (let [incoming (lines (git! "-C" checkout "diff" "--name-only" "--no-renames" (str "HEAD..." target)))]
    (when (seq incoming)
      (->> (str/split (:out (apply git "-C" checkout "ls-files" "--others" "--ignored" "--exclude-standard" "-z" "--" incoming)) #"\u0000")
           (remove str/blank?)
           sort))))

(defn- target-of
  "The ref `branch` follows: its upstream, or for the default and the
  development branch without one, the default branch of origin."
  [checkout branch default-branch]
  (let [upstream (git "-C" checkout "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}")
        track (git! "-C" checkout "for-each-ref" "--format=%(upstream:track)" (str "refs/heads/" branch))]
    (cond
      (= "[gone]" track) {:skip "its upstream is gone"}
      (zero? (:exit upstream)) {:target (str/trim (:out upstream))}
      (#{default-branch development} branch) {:target (str "origin/" default-branch)}
      :else {:skip "it has no upstream"})))

(defn- integrate!
  "Moves `checkout` onto `target`: a fast-forward when it has no commits of
  its own, else a rebase, which is aborted when it conflicts. Returns whether
  the branch moved."
  [checkout ahead target]
  (if (zero? ahead)
    (zero? (:exit (git "-C" checkout "merge" "--ff-only" "--quiet" target)))
    (or (zero? (:exit (git "-C" checkout "rebase" "--quiet" target)))
        (do (git "-C" checkout "rebase" "--abort") false))))

(defn- stash!
  "Stashes the uncommitted changes of `checkout`, untracked files included,
  and returns the stash commit, or nil when git found nothing to stash. The
  top of the stash is compared before and after, so an older entry is never
  taken for this one."
  [checkout]
  (let [top #(str/trim (:out (git "-C" checkout "rev-parse" "--verify" "--quiet" "refs/stash")))
        before (top)]
    (git! "-C" checkout "stash" "push" "--include-untracked" "--quiet" "--message" "aro update")
    (let [after (top)]
      (when (and (seq after) (not= before after)) after))))

(defn- drop-stash!
  "Drops the stash entry `stash`, wherever it is in the list by now."
  [checkout stash]
  (let [i (.indexOf ^java.util.List (lines (git! "-C" checkout "stash" "list" "--format=%H")) stash)]
    (when-not (neg? i)
      (git! "-C" checkout "stash" "drop" "--quiet" (str "stash@{" i "}")))))

(defn- unstash!
  "Applies `stash` with its staged changes; returns whether it applied cleanly."
  [checkout stash]
  (zero? (:exit (git "-C" checkout "stash" "apply" "--index" "--quiet" stash))))

(defn- roll-back!
  "Puts `checkout` back at `head` with the changes of `stash` as they were.
  The untracked files of the stash are removed first, since a failed apply
  may have restored some of them and git refuses to overwrite untracked
  files. On `head` the stash applies cleanly, because it was made there."
  [checkout head stash]
  (git! "-C" checkout "reset" "--hard" "--quiet" head)
  (when stash
    (when (zero? (:exit (git "-C" checkout "rev-parse" "--verify" "--quiet" (str stash "^3"))))
      (doseq [file (remove str/blank? (str/split (git! "-C" checkout "ls-tree" "-r" "-z" "--name-only" (str stash "^3")) #"\u0000"))]
        (fs/delete-if-exists (fs/path checkout file))))
    (when-not (unstash! checkout stash)
      (throw (ex-info (str "the uncommitted changes could not be restored; they are kept in the stash as " stash)
                      {:aro/problem :stash :stash stash})))
    (drop-stash! checkout stash)))

(defn sync-checkout!
  "Brings the branch checked out at `checkout` up to its target without
  losing uncommitted work: the changes are stashed, the branch fast-forwards
  or rebases its own commits onto the target, and the changes are applied
  again. When the rebase or the reapplied changes conflict, the checkout goes
  back to the commit it was on with its changes as they were. Returns
  {:status :current|:updated|:skipped :message}."
  [checkout default-branch]
  (let [branch (let [{:keys [exit out]} (git "-C" checkout "symbolic-ref" "--quiet" "--short" "HEAD")]
                 (when (zero? exit) (str/trim out)))
        skip (fn [why] {:status :skipped :message (str (or branch "HEAD") " left as it is: " why)})]
    (cond
      (nil? branch) (skip "detached")
      (in-progress? checkout) (skip "a merge or rebase is in progress")
      :else
      (let [{:keys [target] :as followed} (target-of checkout branch default-branch)]
        (cond
          (:skip followed) (skip (:skip followed))
          (not (zero? (:exit (git "-C" checkout "rev-parse" "--verify" "--quiet" (str target "^{commit}"))))) (skip (str target " does not exist"))
          :else
          (let [[ahead behind] (map parse-long (str/split (git! "-C" checkout "rev-list" "--left-right" "--count" (str "HEAD..." target)) #"\s+"))]
            (cond
              (zero? behind)
              {:status :current :message (str branch " is up to date with " target)}

              (seq (ignored-in-the-way checkout target))
              (skip (str target " starts tracking ignored files it would overwrite: " (str/join ", " (ignored-in-the-way checkout target))))

              :else
              (let [head (str/trim (git! "-C" checkout "rev-parse" "HEAD"))
                    changed (local-paths checkout)
                    stash (when (seq changed) (stash! checkout))
                    moved (try (integrate! checkout ahead target)
                               (catch Exception e (roll-back! checkout head stash) (throw e)))
                    moved-message (if (zero? ahead)
                                    (str branch " fast-forwarded " behind " commits to " target)
                                    (str branch " rebased " ahead " commits onto " target))]
                (cond
                  (not moved)
                  (do (roll-back! checkout head stash)
                      (skip (str "rebasing its " ahead " commits onto " target " conflicts")))

                  (or (nil? stash) (unstash! checkout stash))
                  (do (when stash (drop-stash! checkout stash))
                      {:status :updated :message moved-message})

                  :else
                  (let [incoming (set (lines (git! "-C" checkout "diff" "--name-only" "--no-renames" (str head "..." target))))]
                    (roll-back! checkout head stash)
                    (skip (str "its uncommitted changes to " (str/join ", " (sort (filter incoming changed)))
                               " conflict with " target))))))))))))

(defn fetch!
  "Fetches origin into the clone at `dir`. Returns nil, or {:status :failed
  :message} when the fetch fails."
  [dir]
  (let [{:keys [exit err]} (git "--git-dir" (git-dir dir) "fetch" "--prune" "--quiet" "--no-recurse-submodules" "origin")]
    (when-not (zero? exit)
      {:status :failed :message (str "fetch failed: " (str/trim err))})))

(defn update!
  "Removes the push URLs of origin in the clone of `repository`, fetches it
  and syncs each of its checkouts. Returns
  {:status :missing|:failed|:done :message :checkouts [result]}."
  [{:keys [dir branch]}]
  (if-let [layout (layout-on-disk dir)]
    (do (remove-push-urls! dir)
        (or (fetch! dir)
            {:status :done
             :checkouts (vec (for [checkout (if (= :plain layout) [(str dir)] (map :path (worktrees dir)))]
                               (assoc (try (sync-checkout! checkout branch)
                                           (catch Exception e {:status :failed :message (ex-message e)}))
                                      :checkout checkout)))}))
    {:status :missing :message "not cloned"}))

(defn- remote-refs
  "The refs of `url` matching `patterns`: {:refs {ref sha}}, or {:failed
  message} when `url` cannot be read."
  [gd url & patterns]
  (let [{:keys [exit out err]} (apply git "--git-dir" gd "ls-remote" "--refs" url patterns)]
    (if (zero? exit)
      {:refs (into {} (for [line (lines out)] (vec (reverse (str/split line #"\s+" 2)))))}
      {:failed (str/trim err)})))

(defn- object? [gd sha]
  (zero? (:exit (git "--git-dir" gd "cat-file" "-e" sha))))

(defn- ancestor? [gd a b]
  (zero? (:exit (git "--git-dir" gd "merge-base" "--is-ancestor" a b))))

(defn branch-state
  "How the commit `mirror` of a mirror's branch relates to `origin`, the
  commit of origin's: :missing without one, else :equal, :behind, :ahead or
  :diverged. Both commits must be in the clone at `gd`."
  [gd origin mirror]
  (cond
    (nil? mirror) :missing
    (= origin mirror) :equal
    (ancestor? gd mirror origin) :behind
    (ancestor? gd origin mirror) :ahead
    :else :diverged))

(defn tag-plan
  "The tags of `origin-tags` to push to a mirror holding `mirror-tags`, both
  {ref sha}: {:push {ref sha}} for those the mirror lacks, and :differ
  [{:ref :origin :mirror}] for those it holds at another object, which are
  never overwritten."
  [origin-tags mirror-tags]
  {:push (into (sorted-map) (remove (comp (set (keys mirror-tags)) key)) origin-tags)
   :differ (vec (for [[ref sha] (sort origin-tags)
                      :let [theirs (get mirror-tags ref)]
                      :when (and theirs (not= sha theirs))]
                  {:ref ref :origin sha :mirror theirs}))})

(defn- fetch-mirror-branch!
  "Fetches `branch` of `mirror` into the clone at `gd` through a private
  ref, which is deleted again so that no ref keeps the mirror's commits.
  Returns {:sha} of the fetched commit, or {:failed message}."
  [gd n mirror branch]
  (let [private (str "refs/aro/mirrors/" n "/" branch)
        {:keys [exit err]} (git "--git-dir" gd "fetch" "--quiet" "--no-tags" "--no-recurse-submodules" "--no-write-fetch-head"
                                mirror (str "+refs/heads/" branch ":" private))]
    (if (zero? exit)
      (let [sha (git! "--git-dir" gd "rev-parse" private)]
        (git! "--git-dir" gd "update-ref" "-d" private)
        {:sha sha})
      {:failed (str/trim err)})))

(defn- push-refs!
  "Pushes `refs`, {ref sha}, to `url` without forcing any of them and
  without running the clone's hooks. Returns nil, or the error of git."
  [gd url refs]
  (when (seq refs)
    (let [{:keys [exit err]} (apply git "--git-dir" gd "push" "--quiet" "--no-verify" url
                                    (for [[ref sha] refs] (str sha ":" ref)))]
      (when-not (zero? exit) (str/trim err)))))

(defn- short-ref [ref]
  (str/replace ref #"^refs/(heads|tags)/" ""))

(defn- sync-branch!
  "Brings `branch` of `mirror` to `origin` by creating or fast-forwarding it,
  and leaves a mirror that is ahead or has diverged as it is."
  [gd n mirror branch origin mirror-sha]
  (let [{:keys [sha failed]} (if (or (nil? mirror-sha) (object? gd mirror-sha))
                               {:sha mirror-sha}
                               (fetch-mirror-branch! gd n mirror branch))
        warn (fn [& parts] {:status :warned :message (str/join parts)})
        state (when-not failed (branch-state gd origin sha))
        pushed (fn [message]
                 (if-let [err (push-refs! gd mirror {(str "refs/heads/" branch) origin})]
                   (warn "pushing " branch " to " mirror " failed: " err)
                   {:status :updated :message message}))]
    (case state
      nil (warn "fetching " branch " of " mirror " failed: " failed)
      :equal nil
      :missing (pushed (str "created " branch " on " mirror " at " origin))
      :behind (pushed (str "fast-forwarded " branch " on " mirror " from " sha " to " origin))
      :ahead (warn branch " on " mirror " left as it is: its " sha " is ahead of origin's " origin)
      :diverged (warn branch " on " mirror " left as it is: its " sha " and origin's " origin " have diverged"))))

(defn- sync-tags!
  "Pushes the tags of `origin-tags` that `mirror-tags` lacks to `mirror` and
  reports those it holds at another object."
  [gd mirror origin-tags mirror-tags]
  (let [{:keys [push differ]} (tag-plan origin-tags mirror-tags)
        err (push-refs! gd mirror push)]
    (concat
     (cond
       err [{:status :warned :message (str "pushing " (count push) " tags to " mirror " failed: " err)}]
       (seq push) [{:status :updated :message (str "pushed " (count push) (if (= 1 (count push)) " tag" " tags") " to " mirror)}])
     (for [d differ]
       {:status :warned
        :message (str "tag " (short-ref (:ref d)) " on " mirror " left as it is: it points at " (:mirror d) ", origin's at " (:origin d))}))))

(defn- sync-mirror!
  "Brings `mirror`, the `n`th mirror of the clone at `gd`, up to origin's
  `branch` at `origin` and its `origin-tags`. Returns [{:status
  :updated|:warned :message :mirror}]."
  [gd n mirror branch origin origin-tags]
  (let [{:keys [refs failed]} (remote-refs gd mirror (str "refs/heads/" branch) "refs/tags/*")]
    (mapv #(assoc % :mirror mirror)
          (if failed
            [{:status :warned :message (str mirror " cannot be reached: " failed)}]
            (let [mirror-tags (into {} (filter (comp #(str/starts-with? % "refs/tags/") key)) refs)]
              (concat (remove nil? [(sync-branch! gd n mirror branch origin (get refs (str "refs/heads/" branch)))])
                      (sync-tags! gd mirror origin-tags mirror-tags)))))))

(defn- origin-tags!
  "The tags of origin, {ref sha}, with their objects fetched into the clone
  at `gd` where it lacks them, or {:failed message}."
  [gd]
  (let [{:keys [refs failed] :as result} (remote-refs gd "origin" "refs/tags/*")]
    (if failed
      result
      (let [missing (remove (partial object? gd) (distinct (vals refs)))
            {:keys [exit err]} (if (seq missing)
                                 (apply git "--git-dir" gd "fetch" "--quiet" "--no-tags" "--no-recurse-submodules" "--no-write-fetch-head" "origin" missing)
                                 {:exit 0})]
        (if (zero? exit) {:refs refs} {:failed (str "fetching the tags of origin failed: " (str/trim err))})))))

(defn sync-mirrors!
  "Pushes the commit origin's default branch is at in the clone of
  `repository`, and origin's tags, to each of its :mirrors. No push is
  forced: a mirror whose branch is ahead or has diverged, or that holds a
  tag at another object, keeps it and is warned about, as is a mirror that
  cannot be reached or rejects the push. Returns {:status :missing|:done
  :message :mirrors [{:status :updated|:warned :message :mirror}]}."
  [{:keys [dir branch mirrors]}]
  (cond
    (empty? mirrors) {:status :done :mirrors []}
    (not (layout-on-disk dir)) {:status :missing :message "not cloned"}
    :else
    (let [gd (git-dir dir)
          origin (str/trim (:out (git "--git-dir" gd "rev-parse" "--verify" "--quiet" (str "refs/remotes/origin/" branch "^{commit}"))))
          warned (fn [message] {:status :done :mirrors [{:status :warned :message message}]})]
      (if (str/blank? origin)
        (warned (str "origin/" branch " does not exist, so no mirror is synced"))
        (let [{:keys [refs failed]} (origin-tags! gd)]
          (if failed
            (warned failed)
            {:status :done
             :mirrors (vec (mapcat (fn [n mirror] (sync-mirror! gd n mirror branch origin refs))
                                   (range) mirrors))}))))))

(def schedules #{"hourly" "daily" "weekly"})

(defn maintain!
  "Runs git's maintenance tasks for `schedule`, one of `schedules`, in the
  clone of `repository`. The tasks are those of the incremental strategy,
  because without a strategy a scheduled run has none, minus prefetch:
  `update!` does the fetching, and an hourly fetch of every clone would
  multiply the traffic by the number of clones. Returns
  {:status :missing|:failed|:done :message}."
  [{:keys [dir]} schedule]
  (if (layout-on-disk dir)
    (let [{:keys [exit err]} (git "--git-dir" (git-dir dir) "-c" "maintenance.strategy=incremental"
                                  "-c" "maintenance.prefetch.enabled=false" "maintenance" "run" (str "--schedule=" schedule) "--quiet")]
      (if (zero? exit)
        {:status :done :message (str schedule " maintenance done")}
        {:status :failed :message (str schedule " maintenance failed: " (str/trim err))}))
    {:status :missing :message "not cloned"}))

(defn- checked-out
  "The branches some worktree of the clone at `dir` has checked out; for a
  plain clone that is its own checkout and its linked worktrees."
  [dir]
  (set (keep :branch (worktrees dir))))

(defn prune-branches!
  "Deletes the local branches of the clone of `repository` whose commits the
  default branch of origin holds, keeping the default branch, development
  and every branch a checkout has checked out. A worktree whose directory is
  gone no longer keeps its branch: git refuses to delete a branch any
  registered worktree holds, so those registrations are pruned first. A
  branch whose upstream is gone but that holds commits origin lacks is kept
  and reported. With `dry-run`, nothing is deleted. Returns {:status :missing|:failed|:done
  :message :deleted [branch] :kept [{:branch :message}]}."
  [{:keys [dir branch]} dry-run]
  (if (layout-on-disk dir)
    (let [gd (git-dir dir)
          _ (when-not dry-run (git! "--git-dir" gd "worktree" "prune"))
          default branch
          target (str "refs/remotes/origin/" default)
          protected (into #{default development} (checked-out dir))]
      (if-not (zero? (:exit (git "--git-dir" gd "rev-parse" "--verify" "--quiet" target)))
        {:status :failed :message (str "origin/" default " does not exist")}
        (let [local (for [line (lines (git! "--git-dir" gd "for-each-ref" "--format=%(refname:short) %(upstream:track)" "refs/heads"))
                          :let [[name track] (str/split line #" " 2)]
                          :when (not (protected name))]
                      {:name name :gone (= "[gone]" track)})
              merged? #(zero? (:exit (git "--git-dir" gd "merge-base" "--is-ancestor" (str "refs/heads/" (:name %)) target)))
              {deleted true others false} (group-by (comp boolean merged?) local)]
          (when-not dry-run
            (doseq [{:keys [name]} deleted]
              (git! "--git-dir" gd "branch" "--delete" "--force" name)))
          {:status :done
           :deleted (mapv :name deleted)
           :kept (vec (for [{:keys [name gone]} others
                            :when gone]
                        (let [n (git! "--git-dir" gd "rev-list" "--count" (str target "..refs/heads/" name))]
                          {:branch name
                           :message (str "its upstream is gone and it holds " n (if (= "1" n) " commit" " commits")
                                         " origin/" default " lacks")})))})))
    {:status :missing :message "not cloned"}))

(defn- move-children! [from to]
  (doseq [child (fs/list-dir from)]
    (fs/move child (fs/path to (fs/file-name child)))))

(defn- worktree-admin-dir
  "The directory in the common git dir that `checkout`'s .git file names."
  [checkout]
  (str/trim (str/replace-first (slurp (str (fs/path checkout ".git"))) #"^gitdir:\s*" "")))

(defn- branch-dir [branch]
  (str/replace branch "/" "-"))

(defn to-worktrees!
  "Turns the plain clone at `dir` into the worktrees layout. The checkout
  moves as it is, uncommitted changes, ignored files and index included,
  into a worktree named after its branch; worktrees for `default-branch` and
  development are added beside it."
  [dir default-branch]
  (let [dir (str (fs/canonicalize dir))
        branch (let [{:keys [exit out]} (git "-C" dir "symbolic-ref" "--quiet" "--short" "HEAD")]
                 (when (zero? exit) (str/trim out)))
        parking (str dir ".aro-layout")]
    (when-not (= :plain (layout-on-disk dir))
      (throw (ex-info (str dir " is not a plain clone") {:aro/problem :layout})))
    (when-not branch
      (throw (ex-info (str dir " has a detached HEAD; check out a branch first") {:aro/problem :layout})))
    (when (in-progress? dir)
      (throw (ex-info (str dir " has a merge or rebase in progress") {:aro/problem :layout})))
    (when (fs/exists? (fs/path dir ".gitmodules"))
      (throw (ex-info (str dir " has submodules, which aro does not move") {:aro/problem :layout})))
    (when (fs/exists? parking)
      (throw (ex-info (str parking " exists; remove it first") {:aro/problem :layout})))
    (fs/move dir parking)
    (fs/create-dirs dir)
    (fs/move (fs/path parking ".git") (fs/path dir ".git"))
    (let [gd (git-dir dir)
          checkout (str (fs/path dir (branch-dir branch)))
          index (fs/path gd "index")]
      (git! "--git-dir" gd "config" "core.bare" "true")
      (git! "--git-dir" gd "config" "core.logallrefupdates" "true")
      (git! "--git-dir" gd "worktree" "add" "--no-checkout" checkout branch)
      (when (fs/exists? index)
        (fs/move index (fs/path (worktree-admin-dir checkout) "index") {:replace-existing true}))
      (move-children! parking checkout)
      (fs/delete parking)
      (let [checked-out (set (map :branch (worktrees dir)))]
        (when-not (checked-out default-branch) (add-default! gd dir default-branch))
        (when-not (checked-out development) (add-development! gd dir default-branch)))
      checkout)))

(defn to-plain!
  "Turns the worktrees layout at `dir` into a plain clone of the worktree of
  `keep`. The other worktrees are removed and their branches remain. A
  worktree to remove that holds uncommitted changes, untracked or ignored
  files refuses the switch before anything changes, since removing it would
  delete them."
  [dir keep]
  (let [dir (str (fs/canonicalize dir))]
    (when-not (= :worktrees (layout-on-disk dir))
      (throw (ex-info (str dir " is not in the worktrees layout") {:aro/problem :layout})))
    (let [gd (git-dir dir)
          all (worktrees dir)
          kept (or (first (filter #(= keep (:branch %)) all))
                   (throw (ex-info (str "no worktree of " dir " has " keep " checked out") {:aro/problem :layout})))
          others (remove #{kept} all)
          strays (strays dir all)]
      (when-not (= dir (str (fs/parent (:path kept))))
        (throw (ex-info (str (:path kept) " is not directly inside " dir) {:aro/problem :layout})))
      (when (seq strays)
        (throw (ex-info (str dir " holds files outside its worktrees: " (str/join ", " (sort strays))) {:aro/problem :layout})))
      (when (in-progress? (:path kept))
        (throw (ex-info (str (:path kept) " has a merge or rebase in progress") {:aro/problem :layout})))
      (when-let [held (seq (for [{:keys [path]} others
                                 file (concat (remove str/blank? (str/split-lines (git! "-C" path "status" "--porcelain" "--untracked-files=all")))
                                              (map #(str "!! " %) (remove str/blank? (str/split-lines (git! "-C" path "ls-files" "--others" "--ignored" "--exclude-standard" "--directory")))))]
                             (str path ": " file)))]
        (throw (ex-info (str dir " has worktrees whose files removing them would delete:\n  " (str/join "\n  " held))
                        {:aro/problem :layout})))
      (when-let [detached (seq (for [{:keys [path]} others
                                     :when (seq (lines (git! "-C" path "rev-list" "HEAD" "--not" "--branches" "--remotes")))]
                                 path))]
        (throw (ex-info (str dir " has worktrees whose HEAD holds commits no branch holds: " (str/join ", " detached))
                        {:aro/problem :layout})))
      (doseq [{:keys [path]} others]
        (git! "--git-dir" gd "worktree" "remove" path))
      (let [checkout (:path kept)
            admin (worktree-admin-dir checkout)
            parking (str (fs/path dir ".aro-layout"))]
        (when (fs/exists? (fs/path admin "index"))
          (fs/copy (fs/path admin "index") (fs/path gd "index") {:replace-existing true}))
        (git! "--git-dir" gd "symbolic-ref" "HEAD" (str "refs/heads/" keep))
        (git! "--git-dir" gd "config" "core.bare" "false")
        (fs/delete (fs/path checkout ".git"))
        (fs/move checkout parking)
        (move-children! parking dir)
        (fs/delete parking)
        (fs/delete-tree admin)
        (git! "-C" dir "worktree" "prune")
        dir))))
