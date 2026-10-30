;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.managers.sesh
  "sesh sessions: one TOML file, :sesh-file, that the user's sesh.toml
  imports. A sesh session opens its first window in its path and runs its
  startup_command there, or none with disable_startup_command, which keeps
  the startup_command of sesh's [default_session] out of a first window
  without commands; every further window is a [[window]] that sesh
  opens in the session's path and types its startup_script into. sesh has no
  panes, so a window keeps the first pane of the airmux window it comes from.
  Windows that are written the same in several sessions, such as the main
  worktree of every clone, are one [[window]]; a name that stands for
  different windows gets the session name in front."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(def ^:private pane-keys
  #{"name" "title" "working_dir" "root" "split" "split_from" "split_size" "clear"
    "on_create" "post_create" "commands" "command" "send_keys"})

(def ^:private window-keys
  #{"working_dir" "root" "layout" "panes" "pane_commands" "pane_command" "pre_window" "clear_panes"
    "on_create" "post_create" "on_pane_create" "post_pane_create"})

(def ^:private translated-window-keys
  #{"working_dir" "root" "panes" "pane_commands" "pane_command" "pre_window" "clear_panes"})

(def ^:private translated-pane-keys
  #{"working_dir" "root" "clear" "commands" "command"})

(def ^:private session-keys
  #{"session_name" "name" "working_dir" "root" "pane_commands" "pane_command" "pre_window" "clear_panes" "windows" "window"})

(defn- command-list [x]
  (cond (nil? x) [] (string? x) [x] :else (vec x)))

(defn- expand [dir home]
  (some-> dir (str/replace #"^(\$\{HOME\}|\$HOME|~)(?=/|$)" home)))

(defn- home-relative [dir home]
  (if (and (str/ends-with? dir "/") (< 1 (count dir)))
    (recur (subs dir 0 (dec (count dir))) home)
    (if (or (= dir home) (str/starts-with? dir (str home "/")))
      (str "~" (subs dir (count home)))
      dir)))

(defn- definition
  "The keys of an airmux pane or window `entry`, a map or the single entry
  {name definition} airmux also reads, with the name under \"name\"."
  [entry known]
  (if (and (map? entry) (= 1 (count entry)) (not (known (key (first entry)))))
    (let [[k v] (first entry)]
      (cond (map? v) (assoc v "name" k)
            (nil? v) {"name" k}
            :else {"name" k "commands" v}))
    (cond (map? entry) entry
          (nil? entry) {}
          :else {"commands" entry})))

(defn- pane-commands [pane]
  (into (command-list (get pane "commands")) (command-list (get pane "command"))))

(defn- unsupported [where m known]
  (for [k (sort (remove known (keys m)))]
    (str where " has " k ", which sesh cannot express")))

(defn- window
  "The window `entry` of a session in `session-dir`, as {:name :dir :script}."
  [entry {:keys [session-dir session-commands clear home where]}]
  (let [w (definition entry window-keys)
        where (str where " window " (get w "name"))
        [pane & others] (map #(definition % pane-keys) (get w "panes"))
        dir (or (expand (or (get pane "working_dir") (get pane "root") (get w "working_dir") (get w "root")) home) session-dir)
        commands (concat session-commands
                         (command-list (or (get w "pane_commands") (get w "pane_command") (get w "pre_window")))
                         (pane-commands pane)
                         (when (or (get pane "clear") (get w "clear_panes") clear) ["clear"]))]
    {:name (get w "name")
     :dir dir
     :commands (vec commands)
     :warnings (concat (unsupported where (dissoc w "name") translated-window-keys)
                       (unsupported (str where " pane") (dissoc (or pane {}) "name" "title") translated-pane-keys)
                       (when (seq others)
                         [(str where " has " (inc (count others)) " panes, and sesh opens one; it keeps the first")]))}))

(defn- script [{:keys [dir commands]} session-dir]
  (str/join "; " (cond->> commands
                   (not= dir session-dir) (cons (str "cd " (fs/relativize session-dir dir))))))

(defn session
  "The sesh session of the airmux `session` at `file`: {:session {...}
  :windows [{:name :script}] :warnings [...]}."
  [file session home]
  (let [where (str "session " file)
        session-name (or (get session "session_name") (get session "name"))
        root (or (expand (or (get session "working_dir") (get session "root")) home) home)
        shared {:session-commands (command-list (or (get session "pane_commands") (get session "pane_command") (get session "pre_window")))
                :clear (get session "clear_panes")
                :home home
                :where where}
        entries (or (get session "windows") (get session "window") [nil])
        [first-window & rest-windows] (map #(window % (assoc shared :session-dir root)) entries)
        session-dir (:dir first-window)
        others (for [w rest-windows] {:name (:name w) :script (script w session-dir)})
        startup (script (assoc first-window :dir session-dir) session-dir)]
    {:session (cond-> {"name" session-name "path" (home-relative session-dir home)}
                (seq startup) (assoc "startup_command" startup)
                (empty? startup) (assoc "disable_startup_command" true)
                (seq others) (assoc "windows" (mapv :name others)))
     :windows (vec others)
     :warnings (vec (concat (unsupported where session session-keys)
                            (mapcat :warnings (cons first-window rest-windows))))}))

(defn- named-windows
  "`translated` sessions with every window name standing for one window:
  where windows of one name differ, the most common of them keeps the name,
  and the others get the session name in front."
  [translated]
  (let [keeper (update-vals (group-by :name (mapcat :windows translated))
                            #(key (first (sort-by (juxt (comp - val) key) (frequencies (map :script %))))))]
    (for [t translated]
      (let [rename (into {} (for [w (:windows t) :when (not= (:script w) (keeper (:name w)))]
                              [(:name w) (str (get-in t [:session "name"]) "-" (:name w))]))]
        (-> t
            (update :windows #(mapv (fn [w] (update w :name (fn [n] (get rename n n)))) %))
            (update-in [:session "windows"] #(some->> % (mapv (fn [n] (get rename n n))))))))))

(defn- toml-string [s]
  (str "\""
       (str/escape s (fn [c]
                       (case c
                         \\ "\\\\" \" "\\\"" \newline "\\n" \tab "\\t" \return "\\r"
                         (when (< (int c) 0x20) (format "\\u%04x" (int c))))))
       "\""))

(defn- toml-value [v]
  (cond (string? v) (toml-string v)
        (boolean? v) (str v)
        :else (str "[" (str/join ", " (map toml-value v)) "]")))

(defn- table [heading entries]
  (str "[[" heading "]]\n"
       (apply str (for [[k v] entries] (str k " = " (toml-value v) "\n")))))

(defn render
  "The TOML of `sessions`, {relative-path session}."
  [sessions home]
  (let [files (sort (keys sessions))
        translated (named-windows (for [file files] (session file (get sessions file) home)))
        windows (sort-by :name (distinct (mapcat :windows translated)))
        shared (for [[n same] (group-by (comp #(get % "name") :session second) (map vector files translated))
                     :when (< 1 (count same))]
                 (str "sessions " (str/join ", " (map first same)) " are all named " n ", and sesh connects to the first"))]
    {:text (str "# Generated by aro. Import it from sesh.toml with import = [\"<this file>\"].\n"
                (apply str (for [t translated]
                             (str "\n" (table "session" (for [k ["name" "path" "startup_command" "disable_startup_command" "windows"]
                                                             :let [v (get (:session t) k)] :when (some? v)]
                                                         [k v])))))
                (apply str (for [{:keys [name script]} windows]
                             (str "\n" (table "window" (cond-> [["name" name]] (seq script) (conj ["startup_script" script])))))))
     :warnings (vec (concat (mapcat :warnings translated) (sort shared)))}))

(defn files
  "{path text} of `sessions` in :sesh-file, and what sesh cannot express."
  [{:keys [sesh-file]} sessions]
  (let [{:keys [text warnings]} (render sessions (System/getProperty "user.home"))]
    {:files {sesh-file text} :warnings warnings}))
