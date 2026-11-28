;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.compatibility-test
  "The files in test/aro/golden were written by aro 2338c84, before the
  inventory, the paths and the config reader came from briko: an inventory,
  a config with `~` paths and what that config loaded to with the home
  /home/golden and no XDG variable set, and the `list` output and generated
  sessions of a config with absolute paths. aro must still read and render
  them byte for byte."
  (:require [aro.config :as config]
            [aro.main :as main]
            [aro.model :as model]
            [aro.support :as s :refer [path]]
            [babashka.fs :as fs]
            [briko.inventory :as inventory]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]))

(use-fixtures :each s/with-root)

(defn- golden [file]
  (str (fs/path "test" "aro" "golden" file)))

(def ^:private golden-home
  {:getenv (fn [variable] (get {"HOME" "/home/golden"} variable))
   :getprop (constantly nil)})

(deftest an-inventory-written-before-briko-renders-byte-for-byte
  (let [text (slurp (golden "inventory.edn"))
        inv (inventory/read-file (golden "inventory.edn"))]
    (is (= text (inventory/render inv)))
    (is (= 6 (count (mapcat val (:sources inv)))))))

(deftest rewriting-an-inventory-written-before-briko-changes-nothing
  (let [file (path "state" "inventory.edn")]
    (fs/create-dirs (path "state"))
    (fs/copy (golden "inventory.edn") file)
    (is (false? (inventory/write! file (inventory/read-file file))))
    (fs/delete file)
    (is (true? (inventory/write! file (inventory/read-file (golden "inventory.edn")))))
    (is (= (slurp (golden "inventory.edn")) (slurp file)))))

(deftest a-config-written-before-briko-loads-to-the-same-map
  (is (= (edn/read-string (slurp (golden "config-loaded.edn")))
         (config/load-config (golden "config.edn") golden-home))))

(defn- list-config []
  (assoc (config/load-config (golden "list-config.edn")) :inventory (golden "inventory.edn")))

(deftest the-list-of-an-inventory-written-before-briko-is-unchanged
  (let [config (list-config)
        sources (:sources (inventory/read-file (:inventory config)))]
    (is (= (slurp (golden "list.txt"))
           (with-out-str (#'main/list! (model/repositories config sources)))))))

(deftest the-sessions-generated-from-an-inventory-written-before-briko-are-unchanged
  (let [out (path "generated")]
    (with-out-str (#'main/generate! (list-config) {:out out}))
    (is (= (edn/read-string (slurp (golden "generated.edn")))
           (into (sorted-map)
                 (for [file (fs/glob out "**")
                       :when (fs/regular-file? file)
                       :let [relative (str (fs/relativize out file))]
                       :when (not= "generated" relative)]
                   [relative (slurp (str file))]))))))
