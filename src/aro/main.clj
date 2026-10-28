;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.main
  (:require [aro.compare :as compare]
            [aro.config :as config]
            [aro.forge :as forge]
            [aro.git :as git]
            [aro.inventory :as inventory]
            [aro.model :as model]
            [aro.sessions :as sessions]
            [aro.tunnel :as tunnel]
            [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import (java.util.concurrent Semaphore)))

(def usage
  "Usage: aro [--config FILE] COMMAND [ARGS]

Commands:
  fetch [SOURCE...]        List the repositories of the sources from their forges into the inventory
  list                     Print every repository with its layout and directory
  generate [--out DIR]     Write the airmux sessions and ttt workspaces
  compare [--toml FILE...] Compare the generated files with those on disk and print suggested overrides
  clone [REPO...] [--max N]
                           Clone the repositories that are missing, one at a time, at most N of them
  update [REPO...] [--jobs N]
                           Fetch every clone and bring its branches up to date, keeping local changes
  maintain --schedule hourly|daily|weekly [REPO...] [--jobs N]
                           Run git's maintenance tasks for the schedule in every clone
  prune [REPO...] [--dry-run] [--jobs N]
                           Delete local branches whose commits the default branch of origin holds
  layout REPO worktrees|plain [--keep BRANCH]
                           Switch a clone between the worktrees and the plain layout

REPO is an id like github.com:metio/aro or a directory inside a clone.")

(defn- ok [& parts] (println (str "[OK] " (str/join parts))))
(defn- skipped [& parts] (println (str "[SKIP] " (str/join parts))))
(defn- error [& parts] (binding [*out* *err*] (println (str "[ERROR] " (str/join parts)))))

(defn- run-hook! [config event replacements]
  (when-let [command (get-in config [:hooks event])]
    (let [command (map #(reduce-kv (fn [s k v] (str/replace s (str "{" (name k) "}") (str v))) % replacements) command)
          {:keys [exit]} (apply process/shell {:continue true} command)]
      (when-not (zero? exit)
        (error "the " (name event) " hook exited with " exit)))))

(defn fetch!
  "Fetches the sources named in `ids`, every source without, and writes the
  inventory. A source that fails keeps its previous records. Returns whether
  every source succeeded."
  [config ids & {:keys [get-json] :or {get-json forge/json-getter}}]
  (let [known (set (map :id (:sources config)))
        unknown (remove known ids)]
    (when (seq unknown)
      (throw (ex-info (str "unknown sources: " (str/join ", " unknown)) {:aro/problem :unknown-source})))
    (let [path (:inventory config)
          before (inventory/read-inventory path)
          results (for [source (:sources config)
                        :when (or (empty? ids) (some #{(:id source)} ids))]
                    (try
                      (let [url (or (get-in source [:gitlab :url]) (get-in source [:forgejo :url]) (get-in source [:github :url] "https://api.github.com"))
                            records (tunnel/with-tunnel (:tunnel source) url
                                      (fn [curl-args]
                                        (forge/list-repositories source (get-json curl-args) (forge/token source))))]
                        (ok (:id source) ": " (count records) " repositories")
                        [(:id source) records])
                      (catch Exception e
                        (error (:id source) ": " (ex-message e))
                        [(:id source) nil])))
          results (doall results)
          after (reduce (fn [inv [id records]] (if records (assoc-in inv [:sources id] records) inv))
                        before results)]
      (when (inventory/write-inventory! path after)
        (ok "wrote " path)
        (run-hook! config :fetched {:inventory path}))
      (every? second results))))

(defn- repositories [config]
  (model/repositories config (:sources (inventory/read-inventory (:inventory config)))))

(defn- list! [config]
  (doseq [r (repositories config)]
    (println (str (:id r) "\t" (name (:layout r)) "\t" (:branch r) "\t" (:dir r))))
  true)

(defn- generate! [config {:keys [out]}]
  (let [config (cond-> config
                 out (assoc :airmux-dir (str (fs/path out "airmux")) :ttt-dir (str (fs/path out "ttt"))))
        {:keys [written deleted]} (sessions/write! (sessions/plan config (repositories config)) (:airmux-dir config))]
    (doseq [path written] (ok "wrote " path))
    (doseq [path deleted] (ok "deleted " path))
    true))

(defn- print-edn [label value]
  (when (seq value)
    (println)
    (println (str ";; " label))
    (pprint/pprint value)))

(defn- compare! [config {:keys [toml]}]
  (let [result (compare/compare-all config
                                    (:sources (inventory/read-inventory (:inventory config)))
                                    (compare/read-toml-repositories toml))]
    (doseq [[status text] (sort-by first (:report result))]
      (println (str "[" (str/upper-case (name status)) "] " text)))
    (print-edn ":repositories overrides" (:repositories result))
    (print-edn ":groups changes" (:groups result))
    (print-edn ":add entries" (:add result))
    (print-edn ":sessions to keep verbatim" (:sessions result))
    true))

(defn clone-all!
  "Clones the repositories of `repositories` that are missing, one at a time
  and at most `max` of them, so that a run with many missing ones leaves the
  rest to later runs. Returns whether none failed."
  [repositories max]
  (let [missing (remove (comp git/layout-on-disk :dir) repositories)
        [now later] (if max (split-at max missing) [missing []])
        success (reduce (fn [success r]
                          (try
                            (git/clone! r)
                            (ok "cloned " (:id r) " into " (:dir r))
                            success
                            (catch Exception e
                              (error (:id r) ": " (ex-message e))
                              false)))
                        true
                        now)]
    (when (seq later)
      (ok (count later) " more missing, left for the next run"))
    success))

(defn- report-update [{:keys [id]} {:keys [status message checkouts]}]
  (case status
    :missing (skipped id ": " message)
    :failed (error id ": " message)
    :done (doseq [{:keys [status message checkout]} checkouts]
            (case status
              :current nil
              :updated (ok id " (" checkout "): " message)
              :skipped (skipped id " (" checkout "): " message)
              :failed (error id " (" checkout "): " message)))))

(defn- report-maintain [{:keys [id]} {:keys [status message]}]
  (case status
    :missing nil
    :failed (error id ": " message)
    :done nil))

(defn- report-prune [{:keys [id]} {:keys [status message deleted kept]} dry-run]
  (case status
    :missing nil
    :failed (error id ": " message)
    :done (do (doseq [branch deleted]
                (ok id ": " (if dry-run "would delete " "deleted ") branch ", which origin holds"))
              (doseq [{:keys [branch message]} kept]
                (skipped id ": kept " branch ": " message)))))

(defn- failed? [{:keys [status checkouts]}]
  (or (= :failed status) (some #(= :failed (:status %)) checkouts)))

(defn run-all!
  "Runs `work` on each of `repositories` with at most `jobs` at a time and
  passes each repository and its result to `report` once it is done.
  Returns whether none failed."
  [repositories jobs work report]
  (let [permits (Semaphore. (max 1 jobs))
        lock (Object.)
        tasks (mapv (fn [r]
                      (future
                        (.acquire permits)
                        (try
                          (let [result (try (work r)
                                            (catch Exception e {:status :failed :message (ex-message e)}))]
                            (locking lock (report r result))
                            result)
                          (finally (.release permits)))))
                    repositories)]
    (not-any? failed? (mapv deref tasks))))

(defn- maintain-all! [repositories {:keys [schedule jobs]}]
  (when-not (git/schedules schedule)
    (throw (ex-info (str "--schedule is one of " (str/join ", " (sort git/schedules)))
                    {:aro/problem :usage})))
  (run-all! repositories jobs #(git/maintain! % schedule) report-maintain))

(defn- prune-all! [repositories {:keys [dry-run jobs]}]
  (run-all! repositories jobs #(git/prune-branches! % dry-run) #(report-prune %1 %2 dry-run)))

(defn- layout! [config [repo target] {:keys [keep]}]
  (let [[r] (model/find-repositories (repositories config) [repo])]
    (case target
      "worktrees" (ok "moved the checkout into " (git/to-worktrees! (:dir r) (:branch r)))
      "plain" (ok (git/to-plain! (:dir r) (or keep (:branch r))) " is a plain clone of " (or keep (:branch r)))
      (throw (ex-info (str "the layout is worktrees or plain, not " target) {:aro/problem :usage})))
    (when (not= (keyword target) (:layout r))
      (println (str "Set {:repositories {" (pr-str (:id r)) " {:layout " (keyword target) "}}} in the config so sessions and clones follow.")))
    true))

(def ^:private spec
  {:config {:desc "The config file"}
   :out {:desc "Write generated files below this directory"}
   :toml {:coerce [] :desc "chezmoi data files listing repositories"}
   :jobs {:coerce :long :default 4}
   :max {:coerce :long :desc "Clone at most N repositories"}
   :keep {:desc "The branch whose worktree becomes the plain clone"}
   :schedule {:desc "hourly, daily or weekly"}
   :dry-run {:coerce :boolean :desc "Report what would be deleted"}
   :help {:coerce :boolean :alias :h}})

(defn parse
  "{:args :opts} of `argv`, options taken from anywhere around the command
  and its arguments, everything after -- an argument."
  [argv]
  (loop [[a & more] argv options [] args []]
    (cond
      (nil? a) {:args args :opts (cli/parse-opts options {:spec spec})}
      (= "--" a) (recur nil options (into args more))
      (and (str/starts-with? a "-") (not= "-" a))
      (let [k (keyword (str/replace a #"^--?" ""))
            flag? (or (str/includes? a "=") (= :boolean (get-in spec [k :coerce])) (= :h k) (empty? more))]
        (if flag?
          (recur more (conj options a) args)
          (recur (rest more) (conj options a (first more)) args)))
      :else (recur more options (conj args a)))))

(defn -main [& argv]
  (let [{:keys [args opts]} (parse argv)
        [command & args] args]
    (if (or (:help opts) (nil? command))
      (println usage)
      (try
        (let [config (config/load-config (or (:config opts) (config/default-path)))
              success (case command
                        "fetch" (fetch! config args)
                        "list" (list! config)
                        "generate" (generate! config opts)
                        "compare" (compare! config opts)
                        "clone" (clone-all! (model/find-repositories (repositories config) args) (:max opts))
                        "update" (run-all! (model/find-repositories (repositories config) args) (:jobs opts) git/update! report-update)
                        "maintain" (maintain-all! (model/find-repositories (repositories config) args) opts)
                        "prune" (prune-all! (model/find-repositories (repositories config) args) opts)
                        "layout" (layout! config args opts)
                        (do (error "unknown command " command) (println usage) false))]
          (shutdown-agents)
          (System/exit (if success 0 1)))
        (catch clojure.lang.ExceptionInfo e
          (error (ex-message e))
          (System/exit 1))))))
