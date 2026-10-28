;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.sessions-test
  (:require [aro.sessions :as sessions]
            [aro.support :as s :refer [path]]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clj-yaml.core :as yaml]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :each s/with-root)

(def ^:private home (System/getProperty "user.home"))

(defn- repository [name layout & {:as more}]
  (merge {:id (str "github.com:metio/" name) :name name :namespace "metio" :topics []
          :dir (str home "/git/github.com/metio/" name) :layout layout :branch "main"
          :session-file (str "metio/" name ".yml") :session {}}
         more))

(deftest a-plain-clone-gets-an-edit-window
  (is (= {"session_name" "devcontainer-java"
          "working_dir" "${HOME}/git/github.com/metio/devcontainer.java"
          "pane_commands" ["alias p='git pull origin main'"]
          "windows" [{"edit" {"panes" [{"clear" true}]}}]}
         (sessions/repository-session (repository "devcontainer.java" :plain)))))

(deftest a-worktrees-clone-gets-a-window-per-worktree
  (is (= {"session_name" "aro"
          "working_dir" "${HOME}/git/github.com/metio/aro/development"
          "pane_commands" ["alias p='git pull origin master'"]
          "windows" [{"development" {"panes" [{"clear" true}]}}
                     {"master" {"working_dir" "${HOME}/git/github.com/metio/aro/master" "panes" [{"clear" true}]}}]}
         (sessions/repository-session (repository "aro" :worktrees :branch "master")))))

(deftest overrides-replace-and-remove-keys
  (let [session (sessions/repository-session (repository "ilo" :plain :session {:pane_commands ["alias s='ilo @dev/env'"] :windows nil}))]
    (is (= ["session_name" "working_dir" "pane_commands"] (keys session)))
    (is (= ["alias s='ilo @dev/env'"] (get session "pane_commands")))))

(deftest groups-get-a-workspace-and-a-session-opening-it
  (let [config {:ttt-dir (path "ttt")
                :groups [{:id "charts" :match [{:namespace "metio"}] :session {:file "metio/all.yml" :session_name "metio charts"}}
                         {:id "quiet" :match [{:namespace "metio"}]}]
                :sessions {"metio/jaas.yml" {:session_name "jaas" :windows [{:edit {:panes [{:clear true}]}}]}}}
        rs [(repository "a" :plain) (repository "b" :worktrees :session false)]
        plan (sessions/plan config rs)]
    (is (= #{"metio/a.yml" "metio/all.yml" "metio/jaas.yml"} (set (keys (:airmux plan)))))
    (is (= {"session_name" "metio charts"
            "working_dir" "${HOME}/git/github.com/metio"
            "windows" [{"edit" {"panes" [{"pane_commands" [(str "ttt --workspace " (path "ttt" "charts.ttt"))]}]}}]}
           (get-in plan [:airmux "metio/all.yml"])))
    (testing "a group without :session has only its workspace"
      (is (= #{(path "ttt" "charts.ttt") (path "ttt" "quiet.ttt")} (set (keys (:ttt plan))))))
    (is (= {"folders" [{"path" (str home "/git/github.com/metio/a")} {"path" (str home "/git/github.com/metio/b")}]}
           (get-in plan [:ttt (path "ttt" "charts.ttt")])))))

(deftest group-dirs-add-folders-to-the-workspace
  (let [config {:ttt-dir (path "ttt") :git-root (path "git")
                :groups [{:id "local" :match [{:namespace "metio"}] :dirs ["github.com/metio/scratch" "~/notes" "/opt/x"]}
                         {:id "only-dirs" :match [] :dirs ["/opt/x"]}]}
        plan (sessions/plan config [(repository "a" :plain)])]
    (is (= {"folders" [{"path" (str home "/git/github.com/metio/a")} {"path" (path "git" "github.com/metio/scratch")}
                       {"path" (str home "/notes")} {"path" "/opt/x"}]}
           (get-in plan [:ttt (path "ttt" "local.ttt")])))
    (testing "a group without members still gets a workspace for its dirs"
      (is (= {"folders" [{"path" "/opt/x"}]} (get-in plan [:ttt (path "ttt" "only-dirs.ttt")]))))))

(deftest writing-replaces-what-the-last-run-wrote
  (let [airmux (path "airmux")
        first-plan {:airmux {"metio/a.yml" {"session_name" "a"} "metio/b.yml" {"session_name" "b"}} :ttt {(path "ttt" "x.ttt") {"folders" []}}}]
    (fs/create-dirs (path "airmux" "mine"))
    (spit (path "airmux" "mine" "hand.yml") "session_name: hand\n")
    (is (= 3 (count (:written (sessions/write! first-plan airmux)))))
    (is (= {"session_name" "a"} (yaml/parse-string (slurp (path "airmux" "metio" "a.yml")) :keywords false)))
    (is (= {"folders" []} (json/parse-string (slurp (path "ttt" "x.ttt")))))
    (let [{:keys [written deleted]} (sessions/write! (update first-plan :airmux dissoc "metio/b.yml") airmux)]
      (is (empty? written))
      (is (= [(path "airmux" "metio" "b.yml")] deleted)))
    (is (fs/exists? (path "airmux" "mine" "hand.yml")))))
