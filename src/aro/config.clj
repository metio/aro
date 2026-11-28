;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.config
  "The configuration: where repositories come from, what to change about
  them, how to group them, and where the generated files go.

  Every function takes briko.paths's environment options, {:getenv f
  :getprop f}, so the home directory and the XDG base directories can be
  given instead of read from the process."
  (:require [babashka.fs :as fs]
            [briko.inventory :as inventory]
            [briko.paths :as paths]
            [clojure.string :as str]))

(defn home
  ([] (home nil))
  ([opts] (paths/home opts)))

(defn default-path
  ([] (default-path nil))
  ([opts] (str (fs/path (paths/config-home opts) "aro" "config.edn"))))

(defn defaults
  ([] (defaults nil))
  ([opts]
   {:sources []
    :repositories {}
    :add []
    :groups []
    :sessions {}
    :git-root (str (fs/path (paths/home opts) "git"))
    :session-managers [:airmux]
    :airmux-dir (str (fs/path (paths/config-home opts) "airmux"))
    :sesh-file (str (fs/path (paths/config-home opts) "sesh" "aro.toml"))
    :manifest (str (fs/path (paths/state-home opts) "aro" "generated"))
    :ttt-dir (str (fs/path (paths/data-home opts) "ttt"))
    :inventory (inventory/default-path opts)
    :layout :plain
    :push-mirrors false
    :switch-merged false
    :quiet-hours 24
    :hooks {}}))

(defn expand-home
  ([s] (expand-home s nil))
  ([s opts] (paths/expand-home s opts)))

(def ^:private home-paths
  [[:git-root] [:airmux-dir] [:sesh-file] [:manifest] [:ttt-dir] [:inventory]])

(defn- check-sources!
  "Refuses sources whose :id is not a non-blank string: the id is the key of
  the source's records in the inventory, which holds only such keys."
  [{:keys [sources] :as config}]
  (doseq [source sources
          :let [id (:id source)]
          :when (not (and (string? id) (not (str/blank? id))))]
    (throw (ex-info (str "the :id of every source must be a non-blank string, not " (pr-str id))
                    {:aro/problem :invalid-config})))
  config)

(defn load-config
  "Reads the EDN config at `path` over the defaults. A missing file means all
  defaults."
  ([path] (load-config path nil))
  ([path opts]
   (-> (paths/load-edn-config path (defaults opts) (merge opts {:home-paths home-paths}))
       check-sources!)))
