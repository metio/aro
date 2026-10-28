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
  git@ and https:// spellings and a trailing .git."
  [s]
  (let [s (-> s
              (str/replace #"^(https?|ssh)://" "")
              (str/replace #"^[^@/:]+@" "")
              (str/replace #"\.git$" "")
              (str/replace #"/+$" ""))
        [_ host path] (or (re-matches #"([^:/]+)[:/](.+)" s)
                          (throw (ex-info (str s " names no host:path") {:aro/problem :invalid-id})))]
    {:host host :path path}))

(defn- record-of-addition [{:keys [id] :as addition}]
  (let [{:keys [host path]} (parse-id id)
        segments (str/split path #"/")]
    (merge {:host host
            :path path
            :name (last segments)
            :namespace (str/join "/" (butlast segments))
            :topics []}
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
            :dir (or (expand-dir config (:dir override))
                     (str (fs/path (:git-root config) (:host record) (:path record))))
            :session-file (or (:session-file override)
                              (str/join "/" (remove str/blank? [session-dir (relative-namespace root (:namespace record)) (str (:name record) ".yml")])))
            :mirrors (vec (:mirrors override))
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
  (let [mirrors (set (mapcat :mirrors (vals (:repositories config))))
        by-source (into {} (map (juxt :id identity)) (:sources config))
        fetched (for [source (:sources config)
                      record (get sources (:id source))
                      :when (kept? config source record)]
                  [source record])
        added (for [addition (:add config)]
                [(get by-source (:source addition) {:id (:source addition)}) (record-of-addition addition)])
        resolved (for [[source record] (concat fetched added)
                       :when (not (get-in config [:repositories (id record) :exclude]))
                       :when (not (mirrors (id record)))]
                   (resolve-repository config source record))]
    (->> resolved
         (reduce (fn [by-id repository] (assoc by-id (:id repository) repository)) {})
         vals
         (sort-by :id)
         vec)))

(defn- matches? [repository matcher]
  (every? (fn [[k v]]
            (case k
              :namespace (= v (:namespace repository))
              :namespace-prefix (or (= v (:namespace repository)) (str/starts-with? (:namespace repository) (str v "/")))
              :topic (some #{v} (:topics repository))
              :host (= v (:host repository))
              :source (= v (:source repository))
              false))
          matcher))

(defn group-members
  "The repositories of `group`: those matching any of its :match maps, every
  key of a map holding, plus its :include ids, minus its :exclude ids."
  [group repositories]
  (let [include (set (:include group))
        exclude (set (:exclude group))]
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
                           (when dir (first (filter inside? repositories)))
                           (let [{:keys [host path]} (try (parse-id arg) (catch Exception _ nil))]
                             (first (filter #(= (str host ":" path) (:id %)) repositories))))]
             (or found (throw (ex-info (str arg " names no known repository") {:aro/problem :unknown-repository}))))))))
