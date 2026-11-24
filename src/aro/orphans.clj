;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.orphans
  "Clones below the git root that no repository of the config names, and
  removing them when they hold nothing that exists only locally."
  (:require [aro.git :as git]
            [aro.model :as model]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(defn- clone? [dir]
  (fs/exists? (fs/path dir ".git") {:nofollow-links true}))

(defn clones
  "Every clone below `root`, without descending into a clone or following
  symbolic links."
  [root]
  (letfn [(subdirs [dir]
            (->> (fs/list-dir dir)
                 (filter #(fs/directory? % {:nofollow-links true}))
                 (sort-by str)))
          (walk [dir]
            (if (clone? dir)
              [(str dir)]
              (mapcat walk (subdirs dir))))]
    (if (fs/directory? root) (vec (mapcat walk (subdirs root))) [])))

(defn- nested-clones
  "The clones inside the clone at `dir`, other than its own worktrees, whose
  insides are searched in turn."
  [dir]
  (let [canonical #(str (fs/canonicalize %))
        own (if (= :worktrees (git/layout-on-disk dir))
              (set (map (comp canonical :path) (git/worktrees dir)))
              #{})]
    (mapcat #(if (own (canonical %)) (clones %) [%]) (clones dir))))

(defn- inside? [dir known]
  (or (= dir known) (str/starts-with? dir (str known "/"))))

(defn orphans
  "The clones below the git root of `config` that are neither the clone of
  one of `repositories`, nor inside a group's :dirs, nor holding either of
  them below their own directory, nor a linked worktree of a repository
  inside one of them."
  [config repositories]
  (let [canonical #(str (fs/canonicalize %))
        known (map canonical (concat (map :dir repositories)
                                     (mapcat (partial model/group-dirs config) (:groups config))))]
    (vec (remove (fn [dir]
                   (let [dir (canonical dir)
                         common (git/common-dir dir)]
                     (some #(or (inside? dir %) (inside? % dir)
                                (and common (inside? common %)))
                           known)))
                 (clones (:git-root config))))))

(defn- remove-empty-parents! [dir root]
  (loop [parent (fs/parent dir)]
    (when (and parent (not= (str (fs/canonicalize parent)) (str (fs/canonicalize root)))
               (inside? (str parent) (str root)) (empty? (fs/list-dir parent)))
      (fs/delete parent)
      (recur (fs/parent parent)))))

(defn remove!
  "Deletes the clone at `dir` unless it holds local work, then the
  directories it leaves empty below `root`. Returns {:status :removed} or
  {:status :kept :reasons [...]}."
  [dir root]
  ;; local-work reads the outer clone only, and deleting it takes every
  ;; clone nested inside along, whatever it holds
  (let [reasons (concat (map #(str "it holds the clone " %) (nested-clones dir))
                        (git/local-work dir))]
    (if (seq reasons)
      {:status :kept :reasons reasons}
      (do (fs/delete-tree dir)
          (remove-empty-parents! dir root)
          {:status :removed}))))
