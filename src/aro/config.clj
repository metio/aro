;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.config
  "The configuration: where repositories come from, what to change about
  them, how to group them, and where the generated files go."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn home []
  (System/getProperty "user.home"))

(defn- env-dir [variable fallback]
  (let [value (System/getenv variable)]
    (if (str/blank? value) (str (fs/path (home) fallback)) value)))

(defn default-path []
  (str (fs/path (env-dir "XDG_CONFIG_HOME" ".config") "aro" "config.edn")))

(defn defaults []
  {:sources []
   :repositories {}
   :add []
   :groups []
   :sessions {}
   :git-root (str (fs/path (home) "git"))
   :airmux-dir (str (fs/path (env-dir "XDG_CONFIG_HOME" ".config") "airmux"))
   :ttt-dir (str (fs/path (env-dir "XDG_DATA_HOME" ".local/share") "ttt"))
   :inventory (str (fs/path (env-dir "XDG_STATE_HOME" ".local/state") "aro" "inventory.edn"))
   :layout :plain
   :hooks {}})

(defn expand-home [s]
  (some-> s (str/replace-first #"^~(?=/|$)" (home))))

(defn load-config
  "Reads the EDN config at `path` over the defaults. A missing file means all
  defaults."
  [path]
  (let [user (when (fs/exists? path) (edn/read-string (slurp (str path))))]
    (-> (merge (defaults) user)
        (update :git-root expand-home)
        (update :airmux-dir expand-home)
        (update :ttt-dir expand-home)
        (update :inventory expand-home))))
