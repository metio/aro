;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.model-test
  (:require [aro.model :as model]
            [babashka.fs]
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

(deftest an-addition-the-inventory-holds-keeps-the-fetched-record
  (let [config (update config :add conj {:id "git.example.org:infra.run/tools/fahrplan" :source "metio"})
        inventory (assoc inventory "infra" [(record "git.example.org" "infra.run/tools/fahrplan" :default-branch "trunk" :topics ["helm"])])
        fahrplan (first (filter #(= "git.example.org:infra.run/tools/fahrplan" (:id %)) (model/repositories config inventory)))]
    (is (= ["trunk" ["helm"] "metio"] ((juxt :branch :topics :source) fahrplan)))))

(deftest mirrors-are-no-repositories-of-their-own
  (let [config (assoc-in config [:repositories "github.com:metio/aro" :mirrors] ["git.example.org:infra.run/tools/fahrplan"])
        repositories (model/repositories config inventory)]
    (is (= ["git.example.org:infra.run/tools/fahrplan"] (:mirrors (first (filter #(= "github.com:metio/aro" (:id %)) repositories)))))
    (is (not-any? #(= "git.example.org:infra.run/tools/fahrplan" (:id %)) repositories))))

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

(deftest ids-parse-ports-and-trailing-slashes
  (is (= {:host "github.com" :path "metio/aro"} (model/parse-id "https://github.com/metio/aro.git/")))
  (is (= {:host "git.example.org" :path "infra.run/x"} (model/parse-id "ssh://git@git.example.org:2222/infra.run/x.git")))
  (is (= {:host "git.example.org" :path "infra.run/x"} (model/parse-id "https://git.example.org:8443/infra.run/x")))
  (is (= {:host "github.com" :path "metio/aro"} (model/parse-id "github.com:metio/aro"))))

(deftest repositories-are-found-by-id
  (let [rs (model/repositories config inventory)]
    (is (= ["github.com:metio/aro"] (map :id (model/find-repositories rs ["https://github.com/metio/aro"]))))
    (is (thrown-with-msg? Exception #"names no known repository" (model/find-repositories rs ["github.com:x/y"])))))

(deftest a-directory-names-the-innermost-clone
  (let [root (str (babashka.fs/create-temp-dir))
        outer {:id "a.org:x/outer" :dir (str root "/src")}
        inner {:id "b.org:y/inner" :dir (str root "/src/inner")}]
    (babashka.fs/create-dirs (str root "/src/inner/lib"))
    (try
      (is (= ["b.org:y/inner"] (map :id (model/find-repositories [outer inner] [(str root "/src/inner/lib")]))))
      (is (= ["a.org:x/outer"] (map :id (model/find-repositories [outer inner] [(str root "/src")]))))
      (finally (babashka.fs/delete-tree root)))))

(deftest ids-in-the-config-may-use-any-spelling
  (let [config (-> config
                   (assoc-in [:repositories "git@github.com:metio/aro.git"] {:branch "trunk"})
                   (update :repositories dissoc "github.com:metio/aro")
                   (assoc-in [:repositories "https://github.com/sebhoss/dotfiles"] {:mirrors ["https://git.example.org/infra.run/tools/fahrplan.git"]}))
        rs (model/repositories config inventory)
        by-id (into {} (map (juxt :id identity)) rs)]
    (is (= "trunk" (:branch (by-id "github.com:metio/aro"))))
    (is (not (contains? by-id "git.example.org:infra.run/tools/fahrplan")))
    (is (= ["https://git.example.org/infra.run/tools/fahrplan.git"] (:mirrors (by-id "github.com:sebhoss/dotfiles"))))
    (is (= ["github.com:metio/aro"]
           (map :id (model/group-members {:match [{:host "github.com"}] :exclude ["git@github.com:sebhoss/dotfiles.git"]} rs))))))

(deftest group-matchers-with-unknown-keys-or-none-are-refused
  (let [rs (model/repositories config inventory)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #":namespace_prefix" (model/group-members {:id "g" :match [{:namespace_prefix "infra.run"}]} rs)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"empty" (model/group-members {:id "g" :match [{}]} rs)))))
