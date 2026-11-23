;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.sessions
  "The sessions and workspaces aro generates: a session per repository and
  per group, a ttt workspace per group, and the custom sessions of the config
  verbatim. Sessions are maps with string keys in the shape airmux reads, so
  an override or a custom session is written exactly as it would appear in
  airmux's YAML; each session manager renders them in its own format."
  (:require [aro.model :as model]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def ^:private key-order ["session_name" "name" "working_dir" "root" "pane_commands" "windows"])

(defn home-relative
  "`dir` with the home directory spelled ${HOME}, as airmux expands it."
  [dir]
  (let [home (System/getProperty "user.home")]
    (if (or (= dir home) (str/starts-with? dir (str home "/")))
      (str "${HOME}" (subs dir (count home)))
      dir)))

(defn- ordered
  "`session` as an array map in airmux's usual key order, other keys after."
  [session]
  (let [known (filter #(contains? session %) key-order)
        others (sort (remove (set key-order) (keys session)))]
    (apply array-map (mapcat (fn [k] [k (get session k)]) (concat known others)))))

(defn apply-override
  "`session` with the keys of `override` replaced; a nil value removes the
  key."
  [session override]
  (ordered (reduce-kv (fn [s k v] (if (nil? v) (dissoc s k) (assoc s k v)))
                      session
                      (walk/stringify-keys override))))

(defn repository-session [{:keys [name dir layout branch session]}]
  (let [working-dir (home-relative dir)
        base (if (= :worktrees layout)
               {"session_name" (model/session-name name)
                "working_dir" (str working-dir "/development")
                "pane_commands" [(str "alias p='git pull origin " branch "'")]
                "windows" [{"development" {"panes" [{"clear" true}]}}
                           {branch {"working_dir" (str working-dir "/" branch)
                                    "panes" [{"clear" true}]}}]}
               {"session_name" (model/session-name name)
                "working_dir" working-dir
                "pane_commands" [(str "alias p='git pull origin " branch "'")]
                "windows" [{"edit" {"panes" [{"clear" true}]}}]})]
    (apply-override base session)))

(defn- common-dir [dirs]
  (let [split (map #(str/split % #"/") dirs)
        prefix (reduce (fn [a b] (vec (map first (take-while (fn [[x y]] (= x y)) (map vector a b))))) split)]
    ;; absolute paths sharing only the root leave the prefix [""]
    (let [dir (str/join "/" (if (= 1 (count dirs)) (butlast prefix) prefix))]
      (if (str/blank? dir) "/" dir))))

(defn ttt-file [config group]
  (str (fs/path (:ttt-dir config) (or (:ttt group) (str (:id group) ".ttt")))))

(defn ttt-workspace [members dirs]
  {"folders" (mapv (fn [dir] {"path" dir}) (concat (map :dir members) dirs))})

(defn group-session
  "The session of `group` opening its workspace, in the common directory of
  its members, or of its `dirs` without members; none for a group with
  neither, since it gets no workspace either."
  [config group members dirs]
  (when-let [session (and (or (seq members) (seq dirs)) (:session group))]
    (apply-override {"session_name" (model/session-name (:id group))
                     "working_dir" (home-relative (common-dir (if (seq members) (map :dir members) dirs)))
                     "windows" [{"edit" {"panes" [{"pane_commands" [(str "ttt --workspace " (home-relative (ttt-file config group)))]}]}}]}
                    (dissoc session :file))))

(defn- name-candidates
  "The session names of `repository` from shortest to longest: its name, then
  with one more segment of its namespace in front each, then with its host."
  [{:keys [id namespace name]}]
  (let [segments (vec (remove str/blank? (str/split (or namespace "") #"/")))
        host (first (str/split (str id) #":" 2))
        spelled (fn [parts] (model/session-name (str/join "-" (remove str/blank? parts))))]
    (vec (concat (for [k (range (inc (count segments)))]
                   (spelled (concat (take-last k segments) [name])))
                 [(spelled (concat [host] segments [name]))]))))

(defn- fixed-name [session]
  (let [session (walk/stringify-keys session)]
    (or (get session "session_name") (get session "name"))))

(defn- distinct-names
  "The `sessions` of `repositories` with every generated session_name unique
  among themselves and the names in `fixed`, the names the config sets: tmux
  opens one session per name, so a second session of a name attaches to the
  first. A generated name that clashes grows by the next namespace segment,
  and finally the host, until it no longer does."
  [repositories sessions fixed]
  (let [generated? (mapv #(not (contains? (walk/stringify-keys (:session %)) "session_name")) repositories)
        candidates (mapv name-candidates repositories)
        named (fn [levels]
                (mapv (fn [i session]
                        (if (generated? i) (get-in candidates [i (levels i)]) (get session "session_name")))
                      (range) sessions))
        levels (loop [levels (vec (repeat (count sessions) 0))]
                 (let [names (named levels)
                       taken (frequencies (concat names fixed))
                       bumped (mapv (fn [i level]
                                      (if (and (generated? i)
                                               (< 1 (taken (names i)))
                                               (< (inc level) (count (candidates i))))
                                        (inc level)
                                        level))
                                    (range) levels)]
                   (if (= bumped levels) levels (recur bumped))))
        names (named levels)
        clashes (sort (for [[n c] (frequencies (concat names fixed)) :when (< 1 c)] n))]
    (when (seq clashes)
      (throw (ex-info (str "several sessions are named " (str/join ", " clashes)
                           ", set a distinct :session_name for all but one")
                      {:aro/problem :invalid-config})))
    (map (fn [session n] (assoc session "session_name" n)) sessions names)))

(defn plan
  "Every generated session and workspace as {:sessions {relative-path
  session} :ttt {path workspace}}. The relative path is the session's file
  below the airmux directory, which also identifies it for other session
  managers."
  [config repositories]
  (let [groups (for [group (:groups config)]
                 [group (model/group-members group repositories) (model/group-dirs config group)])]
    {:sessions (merge (let [rs (remove #(false? (:session %)) repositories)
                            group-sessions (keep (fn [[group members dirs]] (group-session config group members dirs)) groups)
                            fixed (keep fixed-name (concat group-sessions (vals (:sessions config))))]
                      (when-let [shared (seq (for [[file same] (group-by :session-file rs) :when (< 1 (count same))]
                                               (str file " (" (str/join ", " (sort (map :id same))) ")")))]
                        (throw (ex-info (str "several repositories have the same session file, set :session-file or :session false for all but one: "
                                             (str/join "; " shared))
                                        {:aro/problem :invalid-config})))
                      (zipmap (map :session-file rs) (distinct-names rs (map repository-session rs) fixed)))
                    (into {} (for [[group members dirs] groups
                                   :let [session (group-session config group members dirs)]
                                   :when session]
                               [(get-in group [:session :file] (str (:id group) ".yml")) session]))
                    (update-vals (:sessions config) #(ordered (walk/stringify-keys %))))
     :ttt (into {} (for [[group members dirs] groups
                         :when (or (seq members) (seq dirs))]
                     [(ttt-file config group) (ttt-workspace members dirs)]))}))

(defn render-workspace [workspace]
  (str (json/generate-string workspace {:pretty true}) "\n"))

(def legacy-manifest
  "The manifest's name in the airmux directory, where aro kept it before it
  generated for other session managers."
  ".aro-generated")

(defn migrate-manifest!
  "Moves a manifest from the airmux directory `airmux-dir` to `manifest`,
  unless one is there already, so the next write still deletes what the
  earlier runs wrote."
  [airmux-dir manifest]
  (let [legacy (fs/path airmux-dir legacy-manifest)]
    (when (and (fs/exists? legacy) (not (fs/exists? manifest)))
      (fs/create-dirs (fs/parent manifest))
      (fs/move legacy manifest))))

(defn workspace-files
  "{path text} of the workspaces of `plan`."
  [plan]
  (into {} (for [[path workspace] (:ttt plan)] [path (render-workspace workspace)])))

(defn- write-if-changed! [path text]
  (when (not= text (when (fs/exists? path) (slurp (str path))))
    (fs/create-dirs (fs/parent path))
    (spit (str path) text)
    true))

(defn- canonical [path]
  (str (fs/canonicalize path)))

(defn write!
  "Writes `files`, {path text}, and deletes the files a previous run wrote
  that `files` no longer holds; the file `manifest` records what aro wrote.
  Paths compare by the file they name, so a run that spells a directory
  through a symlink keeps what a run under the other spelling wrote.
  Returns the written and deleted paths."
  [files manifest]
  (let [manifest-path (fs/path manifest)
        previous (if (fs/exists? manifest-path) (str/split-lines (slurp (str manifest-path))) [])
        written (vec (for [[path text] (sort-by key files)
                           :when (write-if-changed! path text)]
                       path))
        kept (into (sorted-set) (map canonical) (keys files))
        stale (vec (for [path previous
                         :when (and (not (str/blank? path)) (not (contains? kept (canonical path))) (fs/exists? path))]
                     (do (fs/delete path) path)))]
    (fs/create-dirs (fs/parent manifest-path))
    (spit (str manifest-path) (str (str/join "\n" kept) "\n"))
    {:written written :deleted stale}))
