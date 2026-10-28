;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.main-test
  (:require [aro.inventory :as inventory]
            [aro.main :as main]
            [aro.support :as s :refer [path]]
            [clojure.test :refer [deftest is use-fixtures]]))

(use-fixtures :each s/with-root)

(defn- getter [pages]
  (fn [_curl-args]
    (fn [url _headers]
      (if-let [body (get pages url)]
        (if (instance? Exception body) (throw body) body)
        []))))

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
  (is (= {:args ["clone" "--odd"] :opts {:help true :jobs 4}} (main/parse ["clone" "-h" "--" "--odd"]))))
