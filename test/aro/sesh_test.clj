;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.sesh-test
  (:require [aro.managers.sesh :as sesh]
            [aro.sessions :as sessions]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private home "/home/me")

(defn- repository [name layout & {:as more}]
  (merge {:id (str "github.com:metio/" name) :name name :namespace "metio" :topics []
          :dir (str home "/git/github.com/metio/" name) :layout layout :branch "main"
          :session-file (str "metio/" name ".yml") :session {}}
         more))

(defn- generated
  "The airmux session aro generates for `repository`, with ${HOME} standing
  for `home`."
  [repository]
  (with-redefs [sessions/home-relative #(str/replace-first % home "${HOME}")]
    (sessions/repository-session repository)))

(deftest a-worktrees-clone-opens-in-development-and-cds-into-its-branch
  (let [{:keys [session windows warnings]} (sesh/session "metio/aro.yml" (generated (repository "aro" :worktrees)) home)]
    (is (= {"name" "aro" "path" "~/git/github.com/metio/aro/development"
            "startup_command" "alias p='git pull origin main'; clear" "windows" ["main"]}
           session))
    (is (= [{:name "main" :script "cd ../main; alias p='git pull origin main'; clear"}] windows))
    (is (empty? warnings))))

(deftest a-plain-clone-has-only-its-first-window
  (is (= {"name" "kdk" "path" "~/git/github.com/metio/kdk" "startup_command" "alias p='git pull origin main'; clear"}
         (:session (sesh/session "metio/kdk.yml" (generated (repository "kdk" :plain)) home)))))

(deftest windows-of-other-directories-and-pane-commands
  (let [session {"name" "fiscal-year" "root" "${HOME}/git/github.com/metio/fiscal-year"
                 "windows" [{"shell" {"panes" [{"command" "ilo shell"}]}}
                            {"edit" {"panes" [{"command" "jetbrains-idea ."}]}}
                            {"joi" {"working_dir" "${HOME}/git/github.com/metio/jsonnet-oci-images" "panes" [{"clear" true}]}}]}
        {:keys [session windows]} (sesh/session "metio/fiscal-year.yml" session home)]
    (is (= {"name" "fiscal-year" "path" "~/git/github.com/metio/fiscal-year" "startup_command" "ilo shell" "windows" ["edit" "joi"]}
           session))
    (is (= [{:name "edit" :script "jetbrains-idea ."} {:name "joi" :script "cd ../jsonnet-oci-images; clear"}] windows))))

(deftest group-sessions-run-their-pane-commands
  (is (= "ttt --workspace ~/ttt/x.ttt"
         (get-in (sesh/session "all.yml" {"session_name" "all" "working_dir" "${HOME}/git"
                                           "windows" [{"edit" {"panes" [{"pane_commands" ["ttt --workspace ~/ttt/x.ttt"]}]}}]}
                               home)
                 [:session "startup_command"]))))

(deftest what-sesh-cannot-express-is-reported
  (let [{:keys [warnings]} (sesh/session "x.yml" {"session_name" "x" "on_start" ["make"]
                                                   "windows" [{"w" {"layout" "tiled" "panes" [{"clear" true} {"send_keys" "ls"}]}}]}
                                         home)]
    (is (= ["session x.yml has on_start, which sesh cannot express"
            "session x.yml window w has layout, which sesh cannot express"
            "session x.yml window w has 2 panes, and sesh opens one; it keeps the first"]
           warnings))))

(deftest windows-shared-by-sessions-are-written-once
  (let [sessions {"a.yml" (generated (repository "a" :worktrees))
                  "b.yml" (generated (repository "b" :worktrees))
                  "c.yml" {"session_name" "c" "working_dir" "${HOME}/c" "windows" [{"edit" nil} {"main" {"panes" [{"command" "make"}]}}]}}
        {:keys [text]} (sesh/render sessions home)]
    (is (= 1 (count (re-seq #"name = \"main\"" text))) "a and b share their main window")
    (is (str/includes? text "name = \"c-main\""))
    (is (str/includes? text "windows = [\"c-main\"]"))
    (testing "strings are quoted for TOML"
      (is (str/includes? text "startup_command = \"alias p='git pull origin main'; clear\""))
      (is (str/includes? (:text (sesh/render {"q.yml" {"session_name" "q\"\\" "working_dir" "/q"}} home))
                         "name = \"q\\\"\\\\\"")))))

(deftest sessions-of-one-name-are-reported
  (is (= ["sessions a.yml, b.yml are all named x, and sesh connects to the first"]
         (:warnings (sesh/render {"a.yml" {"session_name" "x" "working_dir" "/a/"} "b.yml" {"name" "x" "root" "/b"}} home))))
  (is (= "/a" (get-in (sesh/session "a.yml" {"session_name" "x" "working_dir" "/a/"} home) [:session "path"]))))
