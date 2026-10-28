;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.main-test
  (:require [aro.inventory :as inventory]
            [aro.main :as main]
            [aro.support :as s :refer [path]]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is use-fixtures]]))

(use-fixtures :each s/with-root)

(defn- getter [pages]
  (fn [_curl-args]
    (fn [url _headers]
      (if-let [body (get pages url)]
        (if (instance? Exception body) (throw body) body)
        []))))

(deftest cloning-stops-at-the-maximum
  (let [upstreams (mapv #(s/upstream! % {"README" "hello\n"}) ["a" "b" "c"])
        repositories (mapv (fn [name upstream] {:id name :url (:url upstream) :dir (path "clones" name) :layout :plain :branch "main"})
                           ["a" "b" "c"] upstreams)
        cloned #(set (filter (fn [name] (fs/exists? (path "clones" name ".git"))) ["a" "b" "c"]))]
    (is (main/clone-all! repositories 2))
    (is (= #{"a" "b"} (cloned)))
    (is (main/clone-all! repositories 2))
    (is (= #{"a" "b" "c"} (cloned)))))

(deftest a-failing-source-keeps-its-previous-records
  (let [config {:inventory (path "inventory.edn")
                :hooks {:fetched ["sh" "-c" (str "echo {inventory} >> " (path "hook"))]}
                :sources [{:id "a" :github {:org "a"}} {:id "b" :github {:org "b"}}]}
        old {:host "github.com" :path "b/old" :name "old" :namespace "b" :default-branch "main" :archived false :fork false :topics []}]
    (inventory/write-inventory! (:inventory config) {:header "" :sources {"b" [old]}})
    (is (false? (main/fetch! config [] :get-json (getter {"https://api.github.com/orgs/a/repos?type=all&per_page=100&page=1"
                                                         [{:name "x" :full_name "a/x" :default_branch "main" :topics []}]
                                                         "https://api.github.com/orgs/b/repos?type=all&per_page=100&page=1"
                                                         (ex-info "down" {})}))))
    (is (= {"a" ["a/x"] "b" ["b/old"]} (update-vals (:sources (inventory/read-inventory (:inventory config))) #(map :path %))))
    (is (= (str (path "inventory.edn") "\n") (slurp (path "hook"))))))

(deftest unknown-sources-are-refused
  (is (thrown-with-msg? Exception #"unknown sources: c" (main/fetch! {:sources [{:id "a"}]} ["c"]))))

(deftest options-follow-the-command-and-its-arguments
  (is (= {:args ["generate"] :opts {:config "c" :out "o" :jobs 4}} (main/parse ["--config" "c" "generate" "--out" "o"])))
  (is (= {:args ["update" "a" "b"] :opts {:jobs 2}} (main/parse ["update" "a" "--jobs" "2" "b"])))
  (is (= {:args ["compare"] :opts {:toml ["x" "y"] :jobs 4}} (main/parse ["compare" "--toml" "x" "--toml" "y"])))
  (is (= {:args ["prune" "x"] :opts {:dry-run true :jobs 4}} (main/parse ["prune" "--dry-run" "x"])))
  (is (= {:args ["maintain"] :opts {:schedule "daily" :jobs 4}} (main/parse ["maintain" "--schedule" "daily"])))
  (is (= {:args ["clone" "--odd"] :opts {:help true :jobs 4}} (main/parse ["clone" "-h" "--" "--odd"]))))
