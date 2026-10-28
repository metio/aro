;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.compare-test
  (:require [aro.compare :as compare]
            [aro.git]
            [aro.support :as s :refer [path]]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :each s/with-root)

(defn- record [path]
  {:host "github.com" :path path :name (fs/file-name path) :namespace "metio" :default-branch "main" :topics []})

(defn- config []
  {:git-root (path "git") :layout :plain :airmux-dir (path "airmux") :ttt-dir (path "ttt")
   :sources [{:id "metio" :github {:org "metio"}}]
   :repositories {} :groups [{:id "all" :match [{:namespace "metio"}]}] :sessions {}})

(defn- run [config toml]
  (compare/compare-all config {"metio" [(record "metio/ilo") (record "metio/aro") (record "metio/new")
                                        (assoc (record "metio/old") :archived true)]}
                       toml))

(defn- write! [file text]
  (fs/create-dirs (fs/parent (path "airmux" file)))
  (spit (path "airmux" file) text))

(deftest differences-become-overrides
  (let [git (path "git" "github.com" "metio")]
    (write! "metio/ilo.yml" (str "session_name: ilo\nworking_dir: " git "/ilo\npane_commands:\n  - alias p='git pull origin main'\n  - alias s='ilo @dev/env'\nwindows:\n  - edit:\n      panes:\n        - clear: true\n"))
    (write! "elsewhere/aro.yml" (str "session_name: aro\nworking_dir: " git "/aro/development\npane_commands:\n  - alias p='git pull origin main'\nwindows:\n  - development:\n      panes:\n        - clear: true\n  - main:\n      working_dir: " git "/aro/main\n      panes:\n        - clear: true\n"))
    (write! "metio/jaas.yml" "session_name: jaas\nwindows:\n  - edit:\n      working_dir: /somewhere\n")
    (write! "metio/old.yml" (str "session_name: old\nworking_dir: " git "/old/development\n"))
    (fs/create-dirs (path "ttt"))
    (spit (path "ttt" "old.ttt") "{\"folders\": []}")
    (let [result (run (config) {"github.com:metio/aro" {:upstream "github.com:metio/aro"}
                                "github.com:metio/gone" {:upstream "github.com:metio/gone" :worktrees false :branch "master"}})]
      (is (= {"github.com:metio/ilo" {:session {"pane_commands" ["alias p='git pull origin main'" "alias s='ilo @dev/env'"]}}
              "github.com:metio/aro" {:layout :worktrees :session-file "elsewhere/aro.yml"}
              "github.com:metio/old" {:include true :layout :worktrees
                                      :session {"pane_commands" nil "windows" nil}}}
             (:repositories result)))
      (is (= ["metio/jaas.yml"] (keys (:sessions result))))
      (is (= [{:id "github.com:metio/gone" :source "metio" :default-branch "master"}] (:add result)))
      (testing "the report names every case"
        (is (= #{:differs :same :custom :new :missing :hidden :layout} (set (map first (:report result)))))))))

(deftest sessions-naming-missing-directories-keep-the-generated-keys
  (let [git (path "git" "github.com" "metio")
        upstream (s/upstream! "ilo" {"README" "x\n"})]
    (aro.git/clone! {:url (:url upstream) :dir (str git "/ilo") :layout :plain :branch "main"})
    (write! "metio/ilo.yml" (str "session_name: ilo\nworking_dir: " git "/ilo\npane_commands:\n  - alias p='git pull origin main'\nwindows:\n  - edit:\n      working_dir: " git "/ilo/gone\n      panes:\n        - clear: true\n"))
    (let [result (run (config) {})]
      (is (nil? (get-in result [:repositories "github.com:metio/ilo"])))
      (is (some #(= :stale (first %)) (:report result))))))

(deftest group-workspaces-become-includes-and-excludes
  (let [git (path "git" "github.com" "metio")]
    (fs/create-dirs (path "ttt"))
    (spit (path "ttt" "all.ttt") (str "{\"folders\": [{\"path\": \"" git "/ilo\"}, {\"path\": \"" git "/old\"}, {\"path\": \"/elsewhere\"}]}"))
    (let [result (run (config) {})]
      (is (= {"all" {:include ["github.com:metio/old"] :exclude ["github.com:metio/aro" "github.com:metio/new"]}} (:groups result)))
      (is (= {:include true} (get-in result [:repositories "github.com:metio/old"])))
      (is (some #(= [:unknown (str (path "ttt" "all.ttt") " lists /elsewhere, which is no repository")] %) (:report result))))
    (testing "a folder the group lists in :dirs is known"
      (let [result (run (assoc (config) :groups [{:id "all" :match [{:namespace "metio"}] :dirs ["/elsewhere"]}]) {})]
        (is (not-any? #(= :unknown (first %)) (:report result)))))))

(deftest the-toml-subset-parses
  (let [file (path "repos.toml")]
    (spit file "[[metio_git_repos]]\nupstream = \"github.com:metio/ilo\"\n\n[[metio_git_repos]]\nupstream = \"gitlab.com:genix.net/meta/x.git\"\nworktrees = false\nbranch = \"master\"\nlocal = \"/somewhere\"\n")
    (is (= {"github.com:metio/ilo" {:upstream "github.com:metio/ilo"}
            "gitlab.com:genix.net/meta/x" {:upstream "gitlab.com:genix.net/meta/x.git" :worktrees false :branch "master" :local "/somewhere"}}
           (compare/read-toml-repositories [file])))))
