;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.model
  "The repositories of this machine: the inventory's records plus the added
  ones, minus the excluded, each resolved with the overrides of its id into a
  clone directory, layout, branch and session file."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(defn id [{:keys [host path]}]
  (str host ":" path))

(defn parse-id
  "{:host :path} of an id like github.com:metio/aro, accepting the
  git@ and https:// spellings, a port in a URL with a scheme and a trailing
  .git or slash."
  [s]
  (let [url? (re-find #"^(https?|ssh)://" s)
        s (cond-> (-> s
                      (str/replace #"^(https?|ssh)://" "")
                      (str/replace #"^[^@/:]+@" "")
                      (str/replace #"/+$" "")
                      (str/replace #"\.git$" ""))
            url? (str/replace #"^([^/:]+):\d+/" "$1/"))
        [_ host path] (or (re-matches #"([^:/]+)[:/](.+)" s)
                          (throw (ex-info (str s " names no host:path") {:aro/problem :invalid-id})))]
    {:host host :path path}))

(defn canonical-id
  "The id `s` names in the host:path spelling, or `s` itself when it names
  none, so that config keys may use any spelling parse-id accepts."
  [s]
  (try (id (parse-id s)) (catch clojure.lang.ExceptionInfo _ s)))

(defn mirrors
  "The mirror ids of the repository of `record` and `override`: those of
  the custom property `mirrors`, separated by whitespace, followed by the
  override's :mirrors, without a second spelling of an id already listed."
  [record override]
  (let [property (get-in record [:properties :mirrors])
        values (cond (string? property) [property]
                     (sequential? property) (filter string? property))]
    (->> (concat (mapcat #(remove str/blank? (str/split (str/trim %) #"\s+")) values)
                 (:mirrors override))
         (reduce (fn [[seen ids :as acc] mirror]
                   (let [k (canonical-id mirror)]
                     (if (seen k) acc [(conj seen k) (conj ids mirror)])))
                 [#{} []])
         second)))

(defn- canonical-overrides
  "`config` with its :repositories keyed by canonical id, overrides of two
  spellings of one id merged."
  [config]
  (update config :repositories
          #(reduce-kv (fn [m k v] (update m (canonical-id k) merge v)) {} %)))

(defn- record-of-addition
  "The record of `addition`, over the `fetched` record of the same id when a
  source lists it too, so its default branch and topics remain."
  [{:keys [id] :as addition} fetched]
  (let [{:keys [host path]} (parse-id id)
        segments (str/split path #"/")]
    (merge {:topics []}
           fetched
           {:host host
            :path path
            :name (last segments)
            :namespace (str/join "/" (butlast segments))}
           (select-keys addition [:default-branch :topics]))))

(defn- source-root
  "The namespace a source's session files are relative to."
  [{:keys [root github gitlab forgejo]}]
  (or root (:group gitlab) (:user gitlab) (:org github) (:user github) (:org forgejo) (:user forgejo) ""))

(defn- relative-namespace [root namespace]
  (cond
    (str/blank? root) namespace
    (= root namespace) ""
    (str/starts-with? namespace (str root "/")) (subs namespace (inc (count root)))
    :else namespace))

(defn expand-dir [config dir]
  (cond
    (str/blank? dir) nil
    (str/starts-with? dir "~") (str/replace-first dir #"^~" (System/getProperty "user.home"))
    (fs/absolute? dir) dir
    :else (str (fs/path (:git-root config) dir))))

(defn session-name [name]
  (str/replace name "." "-"))

(defn- resolve-repository [config source record]
  (let [rid (id record)
        override (get-in config [:repositories rid])
        root (source-root source)
        session-dir (:session-dir source (:id source))]
    (merge record
           {:id rid
            :source (:id source)
            :branch (or (:branch override) (:default-branch record) "main")
            :layout (or (:layout override) (:layout source) (:layout config))
            :switch-merged (true? (first (some #(when (contains? % :switch-merged) [(:switch-merged %)]) [override source config])))
            :dir (or (expand-dir config (:dir override))
                     (str (fs/path (:git-root config) (:host record) (:path record))))
            :session-file (or (:session-file override)
                              (str/join "/" (remove str/blank? [session-dir (relative-namespace root (:namespace record)) (str (:name record) ".yml")])))
            :mirrors (mirrors record override)
            :session (get override :session {})})))

(defn- kept? [config source record]
  (or (get-in config [:repositories (id record) :include])
      (and (or (:archived source) (not (:archived record)))
           (or (:forks source) (not (:fork record))))))

(defn repositories
  "The resolved repositories of `sources`, the inventory's records by source
  id, sorted by id. A repository another one names as a mirror is left out,
  because the clone of that one pushes to it."
  [config sources]
  (let [config (canonical-overrides config)
        by-source (into {} (map (juxt :id identity)) (:sources config))
        fetched (for [source (:sources config)
                      record (get sources (:id source))
                      :when (kept? config source record)]
                  [source record])
        listed (into {} (for [records (vals sources) record records] [(id record) record]))
        mirror-ids (set (for [rid (distinct (concat (keys (:repositories config)) (keys listed)))
                              mirror (mirrors (get listed rid) (get-in config [:repositories rid]))]
                          (canonical-id mirror)))
        added (for [addition (:add config)]
                [(get by-source (:source addition) {:id (:source addition)})
                 (record-of-addition addition (get listed (id (parse-id (:id addition)))))])
        resolved (for [[source record] (concat fetched added)
                       :when (not (get-in config [:repositories (id record) :exclude]))
                       :when (not (mirror-ids (id record)))]
                   (resolve-repository config source record))]
    (->> resolved
         (reduce (fn [by-id repository] (assoc by-id (:id repository) repository)) {})
         vals
         (sort-by :id)
         vec)))

(def matcher-keys #{:namespace :namespace-prefix :topic :property :host :source})

(defn- property-holds?
  "Whether `repository` has every custom property `wanted` names with its
  value, which for a multi-select property is one of the values it holds."
  [repository wanted]
  (every? (fn [[k v]]
            (let [actual (get-in repository [:properties (keyword (name k))])]
              (if (sequential? actual) (some #{v} actual) (= v actual))))
          wanted))

(defn- check-matcher!
  "Throws for a :match map of `group` that is empty, which would match every
  repository, names a key no repository has, which would match none, or
  holds a :property that is no map of properties to values."
  [group matcher]
  (when (empty? matcher)
    (throw (ex-info (str "the group " (:id group) " has an empty :match map") {:aro/problem :invalid-config})))
  (when-let [unknown (seq (remove matcher-keys (keys matcher)))]
    (throw (ex-info (str "the group " (:id group) " matches on " (str/join ", " unknown) ", which is none of " (str/join ", " (sort matcher-keys)))
                    {:aro/problem :invalid-config})))
  (when (and (contains? matcher :property) (not (and (map? (:property matcher)) (seq (:property matcher)))))
    (throw (ex-info (str "the group " (:id group) " has a :property that is no map of properties to values") {:aro/problem :invalid-config}))))

(defn- matches? [repository matcher]
  (every? (fn [[k v]]
            (case k
              :namespace (= v (:namespace repository))
              :namespace-prefix (or (= v (:namespace repository)) (str/starts-with? (:namespace repository) (str v "/")))
              :topic (some #{v} (:topics repository))
              :property (property-holds? repository v)
              :host (= v (:host repository))
              :source (= v (:source repository))
              false))
          matcher))

(defn group-members
  "The repositories of `group`: those matching any of its :match maps, every
  key of a map holding, plus its :include ids, minus its :exclude ids."
  [group repositories]
  (run! (partial check-matcher! group) (:match group))
  (let [include (set (map canonical-id (:include group)))
        exclude (set (map canonical-id (:exclude group)))]
    (filterv #(and (not (exclude (:id %)))
                   (or (include (:id %)) (some (partial matches? %) (:match group))))
             repositories)))

(defn group-dirs
  "The directories `group` lists in :dirs, for folders of its workspace that
  are no repository, resolved like a repository's :dir."
  [config group]
  (keep (partial expand-dir config) (:dirs group)))

(defn find-repositories
  "The repositories `args` name, by id or by a directory inside their clone;
  every repository without args."
  [repositories args]
  (if (empty? args)
    repositories
    (vec (for [arg args]
           (let [dir (when (fs/exists? arg) (str (fs/canonicalize arg)))
                 inside? (fn [repository]
                           (let [root (str (fs/canonicalize (:dir repository)))]
                             (or (= dir root) (str/starts-with? dir (str root "/")))))
                 found (or (first (filter #(= arg (:id %)) repositories))
                           (when dir (last (sort-by #(count (str (fs/canonicalize (:dir %)))) (filter inside? repositories))))
                           (let [{:keys [host path]} (try (parse-id arg) (catch Exception _ nil))]
                             (first (filter #(= (str host ":" path) (:id %)) repositories))))]
             (or found (throw (ex-info (str arg " names no known repository") {:aro/problem :unknown-repository}))))))))
