;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.model-test
  (:require [aro.model :as model]
            [clojure.string]
            [clojure.test :refer [deftest is testing]]))

(defn- record [host path & {:as more}]
  (let [segments (clojure.string/split path #"/")]
    (merge {:host host :path path :name (last segments) :namespace (clojure.string/join "/" (butlast segments))
            :default-branch "main" :archived false :fork false :topics []}
           more)))

(def ^:private config
  {:git-root "/home/me/git"
   :layout :plain
   :sources [{:id "infra" :gitlab {:url "https://git.example.org" :group "infra.run"}}
             {:id "metio" :github {:org "metio"} :layout :worktrees}]
   :repositories {"git.example.org:infra.run/tools/old" {:exclude true}
                  "github.com:metio/aro" {:branch "trunk" :dir "~/src/aro" :session {:pane_commands nil}}}
   :add [{:id "git@github.com:sebhoss/dotfiles.git" :source "metio"}]})

(def ^:private inventory
  {"infra" [(record "git.example.org" "infra.run/tools/fahrplan")
            (record "git.example.org" "infra.run/tools/old")
            (record "git.example.org" "infra.run/attic" :archived true)
            (record "git.example.org" "infra.run/fork" :fork true)]
   "metio" [(record "github.com" "metio/aro" :topics ["tmux"])]})

(deftest repositories-resolve-their-overrides
  (let [rs (model/repositories config inventory)
        by-id (into {} (map (juxt :id identity)) rs)]
    (is (= ["git.example.org:infra.run/tools/fahrplan" "github.com:metio/aro" "github.com:sebhoss/dotfiles"] (map :id rs)))
    (testing "source layout, session path below the source root"
      (is (= {:layout :plain :dir "/home/me/git/git.example.org/infra.run/tools/fahrplan" :session-file "infra/tools/fahrplan.yml" :branch "main"}
             (select-keys (by-id "git.example.org:infra.run/tools/fahrplan") [:layout :dir :session-file :branch]))))
    (testing "overrides"
      (let [aro (by-id "github.com:metio/aro")]
        (is (= [:worktrees "trunk" (str (System/getProperty "user.home") "/src/aro") "metio/aro.yml" {:pane_commands nil}]
               ((juxt :layout :branch :dir :session-file :session) aro)))))
    (testing "an addition spelled as a clone URL"
      (is (= "metio/sebhoss/dotfiles.yml" (:session-file (by-id "github.com:sebhoss/dotfiles")))))))

(deftest archived-and-forks-are-opt-in
  (let [config (update config :sources (fn [ss] (mapv #(if (= "infra" (:id %)) (assoc % :archived true :forks true) %) ss)))]
    (is (= 5 (count (model/repositories config inventory))))))

(deftest groups-match-namespace-topic-and-ids
  (let [rs (model/repositories config inventory)]
    (is (= ["github.com:metio/aro"] (map :id (model/group-members {:match [{:topic "tmux"}]} rs))))
    (is (= ["git.example.org:infra.run/tools/fahrplan"] (map :id (model/group-members {:match [{:namespace-prefix "infra.run"}]} rs))))
    (is (= ["git.example.org:infra.run/tools/fahrplan" "github.com:sebhoss/dotfiles"]
           (map :id (model/group-members {:match [{:namespace "infra.run/tools"}] :include ["github.com:sebhoss/dotfiles"]} rs))))
    (is (empty? (model/group-members {:match [{:host "github.com"}] :exclude ["github.com:metio/aro" "github.com:sebhoss/dotfiles"]} rs)))))

(deftest ids-parse-every-spelling
  (doseq [s ["github.com:metio/aro" "git@github.com:metio/aro.git" "https://github.com/metio/aro" "ssh://git@github.com/metio/aro.git"]]
    (is (= {:host "github.com" :path "metio/aro"} (model/parse-id s)) s)))

(deftest repositories-are-found-by-id
  (let [rs (model/repositories config inventory)]
    (is (= ["github.com:metio/aro"] (map :id (model/find-repositories rs ["https://github.com/metio/aro"]))))
    (is (thrown-with-msg? Exception #"names no known repository" (model/find-repositories rs ["github.com:x/y"])))))
