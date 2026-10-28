;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.compare
  "Comparing what aro would generate with the sessions and workspaces on
  disk and the clones they point at, and turning every difference into the
  override that removes it."
  (:require [aro.git :as git]
            [aro.model :as model]
            [aro.sessions :as sessions]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn- plain
  "`x` with the ordered maps of the YAML parser as array maps, so it prints
  as EDN."
  [x]
  (walk/postwalk #(cond (map? %) (apply array-map (mapcat identity %))
                       (seq? %) (vec %)
                       :else %)
                 x))

(defn- expand-home [s]
  (some-> s (str/replace "${HOME}" (System/getProperty "user.home"))))

(defn- canonical [dir]
  (when-not (str/blank? dir)
    (str (fs/canonicalize (str/replace (expand-home dir) #"/+$" "")))))

(defn read-sessions
  "{relative-path session} of every YAML file below `airmux-dir`."
  [airmux-dir]
  (if-not (fs/directory? airmux-dir)
    {}
    (into (sorted-map)
          (for [file (fs/glob airmux-dir "**.{yml,yaml}")]
            [(str (fs/relativize airmux-dir file))
             (plain (yaml/parse-string (slurp (str file)) :keywords false))]))))

(defn- session-dir [session]
  (canonical (or (get session "working_dir") (get session "root"))))

(defn- repository-of
  "The repository whose clone `session` works in: its working_dir, or the
  directory above it for a session opened in a worktree."
  [session by-dir]
  (when-let [dir (session-dir session)]
    (or (by-dir dir) (by-dir (some-> (fs/parent dir) str)))))

(defn- key-differences [generated existing]
  (into (array-map)
        (for [k (distinct (concat (keys generated) (keys existing)))
              :when (not= (get generated k) (get existing k))]
          [k (get existing k)])))

(defn- normalized-folders [workspace]
  (set (keep #(canonical (get % "path")) (get workspace "folders"))))

(defn read-toml-repositories
  "The [[x_git_repos]] tables of the TOML files of the chezmoi data, as
  {:id {:worktrees :branch :local}}. Reads only the flat subset those files
  use: table array headers and `key = \"string\"` or boolean lines."
  [files]
  (into {}
        (for [file files
              table (rest (str/split (slurp (str file)) #"(?m)^\[\[[^\]]+\]\]\s*$"))
              :let [fields (into {} (for [[_ k v] (re-seq #"(?m)^\s*(\w+)\s*=\s*(.+?)\s*$" table)]
                                      [(keyword k) (cond (= "true" v) true (= "false" v) false :else (str/replace v #"^\"|\"$" ""))]))]
              :when (:upstream fields)]
          [(str/replace (:upstream fields) #"\.git$" "") fields])))

(defn- source-host [{:keys [host github gitlab forgejo]}]
  (or host
      (when github "github.com")
      (some-> (or (:url gitlab) (:url forgejo)) java.net.URI. .getHost)))

(defn- source-for
  "The id of the source a missing repository most likely belongs to: one on
  its host whose root its path is below."
  [config id]
  (let [[host path] (str/split id #":" 2)]
    (:id (first (filter (fn [{:keys [root github gitlab forgejo] :as source}]
                          (let [root (or root (:group gitlab) (:user gitlab) (:org github) (:user github) (:org forgejo) (:user forgejo))]
                            (and (= host (source-host source))
                                 (or (nil? root) (str/starts-with? path (str root "/"))))))
                        (:sources config))))))

(defn- referenced-dirs
  "Every directory a session names in a working_dir or root, at any depth."
  [session]
  (->> (tree-seq coll? #(if (map? %) (vals %) (seq %)) session)
       (filter map?)
       (mapcat #(keep (fn [k] (get % k)) ["working_dir" "root"]))
       (filter string?)))

(defn- mentions? [value dirs]
  (some (fn [dir] (some #(= dir %) (referenced-dirs {"x" value}))) dirs))

(defn- with-overrides [config overrides]
  (update config :repositories #(merge-with merge % overrides)))

(defn- everything
  "The repositories of `config` with archived ones and forks included."
  [config sources]
  (model/repositories (update config :sources (partial mapv #(assoc % :archived true :forks true))) sources))

(defn- shape-layout
  "The layout a session's working_dir implies for the clone at `dir`."
  [session dir]
  (let [working (session-dir session)]
    (cond (= working dir) :plain
          (= (some-> working fs/parent str) dir) :worktrees)))

(defn- toml-overrides [repositories toml]
  (into {}
        (for [r repositories
              :let [entry (get toml (:id r))
                    override (cond-> {}
                               (and (:local entry) (not= (canonical (:local entry)) (canonical (:dir r))))
                               (assoc :dir (:local entry))
                               (and entry (not= (get entry :branch "main") (:branch r)))
                               (assoc :branch (get entry :branch "main")))]
              :when (seq override)]
          [(:id r) override])))

(defn- layout-overrides
  "The layout of each clone as the disk shows it, else as the TOML data
  states it, else as its existing session implies it, where that differs
  from what the config gives it."
  [repositories toml matched]
  (into {}
        (for [r repositories
              :let [entry (get toml (:id r))
                    session (some-> (first (get matched (:id r))) val)
                    on-disk (git/layout-on-disk (:dir r))
                    layout (or on-disk
                               (when entry (if (false? (:worktrees entry)) :plain :worktrees))
                               (when session (shape-layout session (canonical (:dir r)))))]
              :when (and layout (not= layout (:layout r)))]
          [(:id r) {:layout layout
                    ::reason (cond on-disk "the clone is" entry "the TOML data says" :else "the session implies")}])))

(defn- group-changes
  "{group-id {:include :exclude :session}} turning each group's generated
  workspace and session into the ones on disk."
  [config repositories plan existing by-dir report]
  (into {}
        (for [group (:groups config)
              :let [members (model/group-members group repositories)
                    ttt-path (sessions/ttt-file config group)
                    theirs (when (fs/exists? ttt-path) (normalized-folders (json/parse-string (slurp ttt-path))))
                    mine (set (map #(canonical (:dir %)) members))
                    listed (set (keep canonical (model/group-dirs config group)))
                    include (when theirs (sort (keep #(:id (by-dir %)) (remove mine theirs))))
                    unknown (when theirs (sort (remove #(or (mine %) (listed %) (by-dir %)) theirs)))
                    exclude (when theirs (sort (map :id (filter #(not (theirs (canonical (:dir %)))) members))))
                    session-file (get-in group [:session :file] (str (:id group) ".yml"))
                    differences (when-let [session (and (:session group) (get existing session-file))]
                                  (key-differences (get-in plan [:airmux session-file]) session))
                    change (cond-> {}
                             (seq include) (assoc :include (vec include))
                             (seq exclude) (assoc :exclude (vec exclude))
                             (seq differences) (assoc :session differences))]]
          (do
            (cond
              (nil? theirs) (swap! report conj [:new (str ttt-path " is new, for group " (:id group))])
              (and (empty? include) (empty? exclude)) (swap! report conj [:same (str ttt-path)])
              :else (swap! report conj [:differs (str ttt-path ": " (count include) " to include, " (count exclude) " to exclude")]))
            (doseq [folder unknown]
              (swap! report conj [:unknown (str ttt-path " lists " folder ", which is no repository")]))
            (when (seq differences)
              (swap! report conj [:differs (str session-file ": " (str/join ", " (keys differences)))]))
            [(:id group) change]))))

(defn compare-all
  "The comparison of what `config` generates from the inventory `sources`
  with the files on disk, the clones, and the `toml` entries. Returns
  {:report [[status text]] :repositories {id override} :groups {id change}
  :sessions {path session} :add [addition]}: the config changes after which
  aro would generate what is on disk."
  [config sources toml]
  (let [report (atom [])
        existing (read-sessions (:airmux-dir config))
        visible-ids (set (map :id (model/repositories config sources)))
        overrides-1 (toml-overrides (everything config sources) toml)
        all-1 (everything (with-overrides config overrides-1) sources)
        by-dir-1 (into {} (map (juxt #(canonical (:dir %)) identity)) all-1)
        matched (group-by #(:id (repository-of (val %) by-dir-1)) existing)
        includes (into {} (for [id (keys matched) :when (and id (not (visible-ids id)))]
                            (do (swap! report conj [:hidden (str (str/join ", " (map key (get matched id))) " is for " id ", which is archived or a fork")])
                                [id {:include true}])))
        relevant (filter #(or (visible-ids (:id %)) (includes (:id %))) all-1)
        layouts (layout-overrides relevant toml matched)
        _ (doseq [[id {:keys [layout] ::keys [reason]}] layouts]
            (swap! report conj [:layout (str id ": " reason " " (name layout))]))
        overrides (merge-with merge overrides-1 includes (update-vals layouts #(dissoc % ::reason)))
        config (with-overrides config overrides)
        repositories (model/repositories config sources)
        visible (set (map :id repositories))
        by-dir (into {} (map (juxt #(canonical (:dir %)) identity)) (everything config sources))
        plan (sessions/plan config repositories)
        session-overrides (atom {})
        custom (atom (sorted-map))]
    (doseq [r repositories
            :let [candidates (get matched (:id r))
                  [path session] (or (first (filter #(= (:session-file r) (key %)) candidates)) (first candidates))]]
      (if (nil? session)
        (swap! report conj [:new (str (:session-file r) " is new, for " (:id r))])
        (let [missing (when (git/layout-on-disk (:dir r))
                        (remove #(fs/exists? (expand-home %)) (referenced-dirs session)))
              differences (key-differences (get-in plan [:airmux (:session-file r)]) session)
              kept (into (array-map) (remove #(mentions? (val %) missing)) differences)]
          (when (not= path (:session-file r))
            (swap! session-overrides assoc-in [(:id r) :session-file] path))
          (when (seq missing)
            (swap! report conj [:stale (str path " names missing " (str/join ", " (distinct missing)) "; aro's session is kept for those keys")]))
          (cond
            (empty? differences) (swap! report conj [:same path])
            (empty? kept) (swap! report conj [:same (str path " apart from the missing directories")])
            :else (do (swap! report conj [:differs (str path ": " (str/join ", " (keys kept)))])
                      (swap! session-overrides assoc-in [(:id r) :session] kept)))
          (doseq [[extra-path extra] (remove #(= path (key %)) candidates)]
            (swap! custom assoc extra-path extra)))))
    (let [group-files (set (for [g (:groups config) :when (:session g)] (get-in g [:session :file] (str (:id g) ".yml"))))]
      (doseq [[path session] (get matched nil)
              :when (not (group-files path))]
        (swap! custom assoc path session)))
    (doseq [path (keys @custom)]
      (swap! report conj [:custom (str path " matches no repository")]))
    (let [groups (group-changes config repositories plan existing by-dir report)
          group-ttt (set (map #(sessions/ttt-file config %) (:groups config)))]
      (doseq [file (when (fs/directory? (:ttt-dir config)) (fs/glob (:ttt-dir config) "*.ttt"))
              :when (not (group-ttt (str file)))]
        (swap! report conj [:custom (str file " is no group's workspace")]))
      (let [known (set (map :id (everything config sources)))
            missing (sort (remove known (keys toml)))]
        (doseq [id missing]
          (swap! report conj [:missing (str id " is in the TOML data, not the inventory")]))
        {:report @report
         :repositories (into (sorted-map)
                             (merge-with merge
                                         overrides
                                         (into {} (for [id (mapcat :include (vals groups)) :when (not (visible id))]
                                                    [id {:include true}]))
                                         @session-overrides
                                         (into {} (for [id missing
                                                        :let [entry (get toml id)
                                                              layout (if (false? (:worktrees entry)) :plain :worktrees)
                                                              override (cond-> {}
                                                                         (not= layout (:layout config)) (assoc :layout layout)
                                                                         (:local entry) (assoc :dir (:local entry)))]
                                                        :when (seq override)]
                                                    [id override]))))
         :groups (into (sorted-map) (filter (comp seq val) groups))
         :sessions @custom
         :add (vec (for [id missing
                         :let [entry (get toml id)
                               source (source-for config id)]]
                     (cond-> {:id id}
                       source (assoc :source source)
                       (:branch entry) (assoc :default-branch (:branch entry)))))}))))
