;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.sessions-test
  (:require [aro.managers :as managers]
            [aro.sessions :as sessions]
            [aro.support :as s :refer [path]]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
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
    (is (= #{"metio/a.yml" "metio/all.yml" "metio/jaas.yml"} (set (keys (:sessions plan)))))
    (is (= {"session_name" "metio charts"
            "working_dir" "${HOME}/git/github.com/metio"
            "windows" [{"edit" {"panes" [{"pane_commands" [(str "ttt --workspace " (path "ttt" "charts.ttt"))]}]}}]}
           (get-in plan [:sessions "metio/all.yml"])))
    (testing "a group without :session has only its workspace"
      (is (= #{(path "ttt" "charts.ttt") (path "ttt" "quiet.ttt")} (set (keys (:ttt plan))))))
    (is (= {"folders" [{"path" (str home "/git/github.com/metio/a")} {"path" (str home "/git/github.com/metio/b")}]}
           (get-in plan [:ttt (path "ttt" "charts.ttt")])))))

(deftest repositories-of-the-same-name-get-distinct-session-names
  (let [a (repository "grafana" :plain :id "x.org:team/dashboards/grafana" :namespace "team/dashboards" :session-file "a.yml")
        b (repository "grafana" :plain :id "x.org:team/charts/grafana" :namespace "team/charts" :session-file "b.yml")
        c (repository "other" :plain :session-file "c.yml")
        named (repository "grafana" :plain :id "x.org:team/named/grafana" :namespace "team/named" :session-file "d.yml"
                          :session {:session_name "mine"})
        airmux (:sessions (sessions/plan {} [a b c named]))]
    (is (= ["dashboards-grafana" "charts-grafana" "other" "mine"]
           (map #(get-in airmux [% "session_name"]) ["a.yml" "b.yml" "c.yml" "d.yml"])))))

(deftest two-repositories-with-one-session-file-are-refused
  (let [a (repository "dotfiles" :plain :id "github.com:sebhoss/dotfiles" :session-file "sebhoss/dotfiles.yml")
        b (repository "dotfiles" :plain :id "codeberg.org:sebhoss/dotfiles" :session-file "sebhoss/dotfiles.yml")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"codeberg.org:sebhoss/dotfiles.*github.com:sebhoss/dotfiles"
                          (sessions/plan {} [a b])))))

(deftest a-group-without-members-gets-no-session
  (let [config {:ttt-dir (path "ttt") :groups [{:id "g" :session {} :match [{:host "nowhere"}]}]}]
    (is (= {:sessions {"metio/aro.yml" (sessions/repository-session (repository "aro" :plain))} :ttt {}}
           (sessions/plan config [(repository "aro" :plain)])))))

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

(deftest workspaces-are-json-whatever-their-name
  (let [file (path "ttt" "work.code-workspace")]
    (sessions/write! (sessions/workspace-files {:sessions {} :ttt {file {"folders" [{"path" "/x"}]}}}) (path "generated"))
    (is (= {"folders" [{"path" "/x"}]} (json/parse-string (slurp file))))))

(defn- generated [plan config]
  (merge (:files (managers/files config (:sessions plan))) (sessions/workspace-files plan)))

(deftest writing-replaces-what-the-last-run-wrote
  (let [config {:session-managers [:airmux] :airmux-dir (path "airmux")}
        manifest (path "state" "generated")
        first-plan {:sessions {"metio/a.yml" {"session_name" "a"} "metio/b.yml" {"session_name" "b"}} :ttt {(path "ttt" "x.ttt") {"folders" []}}}]
    (fs/create-dirs (path "airmux" "mine"))
    (spit (path "airmux" "mine" "hand.yml") "session_name: hand\n")
    (is (= 3 (count (:written (sessions/write! (generated first-plan config) manifest)))))
    (is (= {"session_name" "a"} (yaml/parse-string (slurp (path "airmux" "metio" "a.yml")) :keywords false)))
    (is (= {"folders" []} (json/parse-string (slurp (path "ttt" "x.ttt")))))
    (let [{:keys [written deleted]} (sessions/write! (generated (update first-plan :sessions dissoc "metio/b.yml") config) manifest)]
      (is (empty? written))
      (is (= [(path "airmux" "metio" "b.yml")] deleted)))
    (is (fs/exists? (path "airmux" "mine" "hand.yml")))
    (testing "switching to sesh deletes the airmux files and writes the sesh file"
      (let [{:keys [written deleted]} (sessions/write! (generated first-plan (assoc config :session-managers [:sesh] :sesh-file (path "sesh" "aro.toml"))) manifest)]
        (is (= [(path "sesh" "aro.toml")] written))
        (is (= [(path "airmux" "metio" "a.yml")] deleted))))))

(deftest writing-keeps-what-another-spelling-of-the-same-directory-wrote
  (let [real (path "real")
        link (path "link")
        manifest (path "state" "generated")]
    (fs/create-dirs real)
    (fs/create-sym-link link real)
    (sessions/write! {(str (fs/path link "a.yml")) "session_name: a\n"} manifest)
    (let [{:keys [deleted]} (sessions/write! {(str (fs/path real "a.yml")) "session_name: a\n"} manifest)]
      (is (empty? deleted))
      (is (fs/exists? (fs/path real "a.yml"))))
    (testing "the manifest names each file once, whatever spelling wrote it"
      (is (= [(str (fs/canonicalize (fs/path real "a.yml")))] (str/split-lines (slurp manifest)))))))

(deftest the-manifest-moves-out-of-the-airmux-directory
  (fs/create-dirs (path "airmux"))
  (spit (path "airmux" ".aro-generated") (str (path "airmux" "old.yml") "\n"))
  (spit (path "airmux" "old.yml") "session_name: old\n")
  (sessions/migrate-manifest! (path "airmux") (path "state" "generated"))
  (is (not (fs/exists? (path "airmux" ".aro-generated"))))
  (is (= [(path "airmux" "old.yml")] (:deleted (sessions/write! {} (path "state" "generated"))))))

(deftest unknown-session-managers-are-refused
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #":tmuxinator, which aro does not know; it knows :airmux, :sesh"
                        (managers/files {:session-managers [:tmuxinator]} {}))))

(deftest every-session-name-in-the-plan-is-unique
  (let [names (fn [config repositories]
                (sort (map #(get % "session_name") (vals (:sessions (sessions/plan (merge {:ttt-dir (path "ttt")} config) repositories))))))]
    (testing "ids differing only in host"
      (is (= ["codeberg-org-metio-aro" "github-com-metio-aro"]
             (sort (names {} [(repository "aro" :plain)
                              (repository "aro" :plain :id "codeberg.org:metio/aro" :session-file "cb/aro.yml")])))
          "the namespace both share does not separate them; the host does"))
    (testing "ids differing in an earlier namespace segment"
      (is (= ["a-x-grafana" "b-x-grafana"]
             (names {} [(repository "grafana" :plain :id "github.com:a/x/grafana" :namespace "a/x" :session-file "a.yml")
                        (repository "grafana" :plain :id "github.com:b/x/grafana" :namespace "b/x" :session-file "b.yml")]))))
    (testing "a prefixed name meeting another repository's plain name"
      (let [ns (names {} [(repository "tools" :plain :id "github.com:metio/tools" :session-file "a.yml")
                          (repository "tools" :plain :id "github.com:other/tools" :namespace "other" :session-file "b.yml")
                          (repository "metio-tools" :plain :id "github.com:x/metio-tools" :namespace "x" :session-file "c.yml")])]
        (is (= (count ns) (count (distinct ns))) (pr-str ns))))
    (testing "a group session of a repository's name"
      (is (= ["aro" "metio-aro"]
             (names {:groups [{:id "aro" :match [{:namespace "metio"}] :session {}}]} [(repository "aro" :plain)]))))
    (testing "two names the config sets are refused"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"several sessions are named aro"
                            (names {:sessions {"x.yml" {:session_name "aro"}}}
                                   [(repository "aro" :plain :session {:session_name "aro"})]))))))
