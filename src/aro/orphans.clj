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
  (letfn [(walk [dir]
            (if (clone? dir)
              [(str dir)]
              (->> (fs/list-dir dir)
                   (filter #(fs/directory? % {:nofollow-links true}))
                   (sort-by str)
                   (mapcat walk))))]
    (if (fs/directory? root) (vec (mapcat walk (sort-by str (fs/list-dir root)))) [])))

(defn- inside? [dir known]
  (or (= dir known) (str/starts-with? dir (str known "/"))))

(defn orphans
  "The clones below the git root of `config` that are neither the clone of
  one of `repositories` nor inside a group's :dirs."
  [config repositories]
  (let [canonical #(str (fs/canonicalize %))
        known (map canonical (concat (map :dir repositories)
                                     (mapcat (partial model/group-dirs config) (:groups config))))]
    (vec (remove (fn [dir] (some (partial inside? (canonical dir)) known))
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
  (let [reasons (git/local-work dir)]
    (if (seq reasons)
      {:status :kept :reasons reasons}
      (do (fs/delete-tree dir)
          (remove-empty-parents! dir root)
          {:status :removed}))))
