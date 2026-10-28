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

(defn push-urls
  "The push URLs of `repository`: its own and those of its mirrors, or none
  without mirrors, so that a push goes to the fetch URL alone."
  [{:keys [mirrors] :as repository}]
  (when (seq mirrors)
    (into [(clone-url repository)] mirrors)))

(defn- configure-push! [dir repository]
  (let [gd (git-dir dir)
        wanted (vec (push-urls repository))]
    (when (not= wanted (vec (lines (:out (git "--git-dir" gd "config" "--get-all" "remote.origin.pushurl")))))
      (git "--git-dir" gd "config" "--unset-all" "remote.origin.pushurl")
      (doseq [url wanted]
        (git! "--git-dir" gd "config" "--add" "remote.origin.pushurl" url)))))

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

(defn clone!
  "Clones `repository` into its :dir in its :layout. Returns :cloned, or
  :exists when the directory already holds a clone."
  [{:keys [dir layout branch] :as repository}]
  (if (layout-on-disk dir)
    :exists
    (do
      (ensure-empty! dir)
      (if (= :worktrees layout)
        (let [gd (git-dir dir)]
          (git! "clone" "--bare" "--quiet" (clone-url repository) gd)
          (git! "--git-dir" gd "config" "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*")
          (git! "--git-dir" gd "config" "core.logallrefupdates" "true")
          (git! "--git-dir" gd "fetch" "--quiet" "origin")
          (doseq [ref (lines (git! "--git-dir" gd "for-each-ref" "--format=%(refname)" "refs/heads"))]
            (git! "--git-dir" gd "update-ref" "-d" ref))
          (add-default! gd dir branch)
          (add-development! gd dir branch))
        (git! "clone" "--quiet" "--branch" branch (clone-url repository) dir))
      (configure-push! dir repository)
      :cloned)))

(declare worktrees)

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
        checkouts (if (= :worktrees (layout-on-disk dir)) (map :path (worktrees dir)) [(str dir)])
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

(defn sync-checkout!
  "Brings the branch checked out at `checkout` up to its target without
  losing uncommitted work: a fast-forward when it has no commits of its own,
  else a rebase that stashes the local changes around it. A merge or rebase
  git refuses or that conflicts leaves the checkout as it was. Returns
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

              (zero? ahead)
              (if (zero? (:exit (git "-C" checkout "merge" "--ff-only" "--quiet" target)))
                {:status :updated :message (str branch " fast-forwarded " behind " commits to " target)}
                (skip (str "uncommitted changes touch files " target " changes")))

              (seq (local-paths checkout))
              (let [incoming (set (lines (git! "-C" checkout "diff" "--name-only" "--no-renames" (str "HEAD..." target))))
                    touched (filter incoming (local-paths checkout))]
                (if (seq touched)
                  (skip (str "it has " ahead " own commits and uncommitted changes to " (str/join ", " (sort touched)) ", which " target " changes"))
                  (if (zero? (:exit (git "-C" checkout "rebase" "--autostash" "--quiet" target)))
                    {:status :updated :message (str branch " rebased " ahead " commits onto " target)}
                    (do (git "-C" checkout "rebase" "--abort")
                        (skip (str "rebasing its " ahead " commits onto " target " conflicts"))))))

              :else
              (if (zero? (:exit (git "-C" checkout "rebase" "--quiet" target)))
                {:status :updated :message (str branch " rebased " ahead " commits onto " target)}
                (do (git "-C" checkout "rebase" "--abort")
                    (skip (str "rebasing its " ahead " commits onto " target " conflicts")))))))))))

(defn update!
  "Points the push URLs of `repository` at it and its mirrors, fetches it and
  syncs each of its checkouts. Returns
  {:status :missing|:failed|:done :message :checkouts [result]}."
  [{:keys [dir branch] :as repository}]
  (if-let [layout (layout-on-disk dir)]
    (let [_ (configure-push! dir repository)
          fetch (git "--git-dir" (git-dir dir) "fetch" "--prune" "--quiet" "origin")]
      (if-not (zero? (:exit fetch))
        {:status :failed :message (str "fetch failed: " (str/trim (:err fetch)))}
        {:status :done
         :checkouts (vec (for [checkout (if (= :plain layout) [(str dir)] (map :path (worktrees dir)))]
                           (assoc (try (sync-checkout! checkout branch)
                                       (catch Exception e {:status :failed :message (ex-message e)}))
                                  :checkout checkout)))}))
    {:status :missing :message "not cloned"}))

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

(defn- checked-out [dir layout]
  (set (if (= :plain layout)
         (let [{:keys [exit out]} (git "-C" dir "symbolic-ref" "--quiet" "--short" "HEAD")]
           (when (zero? exit) [(str/trim out)]))
         (keep :branch (worktrees dir)))))

(defn prune-branches!
  "Deletes the local branches of the clone of `repository` whose commits the
  default branch of origin holds, keeping the default branch, development
  and every branch a checkout has checked out. A branch whose upstream is
  gone but that holds commits origin lacks is kept and reported. With
  `dry-run`, nothing is deleted. Returns {:status :missing|:failed|:done
  :message :deleted [branch] :kept [{:branch :message}]}."
  [{:keys [dir branch]} dry-run]
  (if-let [layout (layout-on-disk dir)]
    (let [gd (git-dir dir)
          default branch
          target (str "refs/remotes/origin/" default)
          protected (into #{default development} (checked-out dir layout))]
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
          paths (set (map :path all))
          strays (remove #(or (= ".git" (str (fs/file-name %))) (paths (str %))) (map str (fs/list-dir dir)))]
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
