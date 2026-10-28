;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.forge-test
  (:require [aro.forge :as forge]
            [clojure.test :refer [deftest is testing]]))

(defn- fake
  "A get-json answering from `pages`, {url body}, and recording each call."
  [calls pages]
  (fn [url headers]
    (swap! calls conj [url headers])
    (get pages url [])))

(deftest github-organizations-are-paged-until-a-short-page
  (let [calls (atom [])
        full (vec (for [i (range 100)] {:name (str "r" i) :full_name (str "metio/r" i) :default_branch "main" :topics []}))
        pages {"https://api.github.com/orgs/metio/repos?type=all&per_page=100&page=1" full
               "https://api.github.com/orgs/metio/repos?type=all&per_page=100&page=2"
               [{:name "aro" :full_name "metio/aro" :default_branch "main" :archived false :fork false :topics ["tmux" "clojure"]}]}
        records (forge/list-repositories {:id "metio" :github {:org "metio"}} (fake calls pages) "secret")]
    (is (= 101 (count records)))
    (is (= {:host "github.com" :path "metio/aro" :name "aro" :namespace "metio" :default-branch "main"
            :archived false :fork false :topics ["clojure" "tmux"]}
           (last records)))
    (is (= 2 (count @calls)))
    (is (= "Bearer secret" (get-in @calls [0 1 "Authorization"])))))

(deftest github-users-list-their-own-repositories
  (let [calls (atom [])]
    (forge/list-repositories {:id "me" :github {:user "sebhoss"}} (fake calls {}) nil)
    (is (= [["https://api.github.com/users/sebhoss/repos?type=owner&per_page=100&page=1" {"Accept" "application/vnd.github+json"}]]
           @calls))))

(deftest gitlab-groups-include-subgroups
  (let [calls (atom [])
        pages {"https://git.example.org/api/v4/groups/infra.run/projects?include_subgroups=true&order_by=path&sort=asc&per_page=100&page=1"
               [{:path "fahrplan" :path_with_namespace "infra.run/tools/fahrplan" :namespace {:full_path "infra.run/tools"}
                 :default_branch "main" :archived true :topics ["go"] :forked_from_project {:id 1}}]}
        records (forge/list-repositories {:id "infra" :gitlab {:url "https://git.example.org/" :group "infra.run"}} (fake calls pages) "t")]
    (is (= [{:host "git.example.org" :path "infra.run/tools/fahrplan" :name "fahrplan" :namespace "infra.run/tools"
             :default-branch "main" :archived true :fork true :topics ["go"]}]
           records))
    (is (= "t" (get-in @calls [0 1 "PRIVATE-TOKEN"])))))

(deftest forgejo-pages-by-fifty
  (let [calls (atom [])]
    (forge/list-repositories {:id "cb" :forgejo {:url "https://codeberg.org" :user "sebhoss"}} (fake calls {}) nil)
    (is (= "https://codeberg.org/api/v1/users/sebhoss/repos?limit=50&page=1" (ffirst @calls)))))

(deftest a-host-override-names-the-records
  (testing "a forge reached at another address keeps its own host"
    (let [pages {"https://localhost:8443/api/v4/groups/g/projects?include_subgroups=true&order_by=path&sort=asc&per_page=100&page=1"
                 [{:path "a" :path_with_namespace "g/a" :namespace {:full_path "g"} :topics []}]}]
      (is (= "git.example.org"
             (:host (first (forge/list-repositories {:id "x" :host "git.example.org" :gitlab {:url "https://localhost:8443" :group "g"}}
                                                    (fake (atom []) pages) nil))))))))

(deftest a-source-without-forge-is-refused
  (is (thrown-with-msg? Exception #"names none of" (forge/list-repositories {:id "x"} (fake (atom []) {}) nil))))

(deftest a-token-command-prints-the-token
  (is (= "abc" (forge/token {:id "x" :token-command ["printf" "abc\\nrest"]})))
  (is (nil? (forge/token {:id "x"})))
  (is (thrown-with-msg? Exception #"printed no token" (forge/token {:id "x" :token-command ["false"]}))))
