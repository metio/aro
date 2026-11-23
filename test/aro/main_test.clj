;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.main-test
  (:require [aro.inventory :as inventory]
            [aro.git :as git]
            [aro.main :as main]
            [aro.support :as s :refer [path]]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :each s/with-root)

(defn- getter [pages]
  (fn [_curl-args]
    (fn [url _headers]
      (if-let [body (get pages url)]
        (if (instance? Exception body) (throw body) body)
        []))))

(deftest unknown-options-are-refused
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"--dryrun" (main/parse ["prune" "--dryrun" "github.com:metio/aro"])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"--verbose" (main/parse ["--verbose" "fetch"]))))

(deftest cloning-stops-at-the-maximum
  (let [upstreams (mapv #(s/upstream! % {"README" "hello\n"}) ["a" "b" "c"])
        repositories (mapv (fn [name upstream] {:id name :url (:url upstream) :dir (path "clones" name) :layout :plain :branch "main"})
                           ["a" "b" "c"] upstreams)
        cloned #(set (filter (fn [name] (fs/exists? (path "clones" name ".git"))) ["a" "b" "c"]))]
    (is (main/clone-all! repositories 2 nil))
    (is (= #{"a" "b"} (cloned)))
    (is (main/clone-all! repositories 2 nil))
    (is (= #{"a" "b" "c"} (cloned)))))

(deftest cloning-waits-the-delay-between-two-clones
  (let [repositories (mapv (fn [name] {:id name :url (:url (s/upstream! name {"README" "hello\n"})) :dir (path "clones" name) :layout :plain :branch "main"})
                           ["a" "b" "c"])
        sleeps (atom [])]
    (is (main/clone-all! repositories nil 30 :sleep! #(swap! sleeps conj %)))
    (is (= [30 30] @sleeps))))

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
  (is (= {:args ["prune" "x"] :opts {:switch-merged true :jobs 4}} (main/parse ["prune" "--switch-merged" "x"])))
  (is (= {:args ["maintain"] :opts {:schedule "daily" :jobs 4}} (main/parse ["maintain" "--schedule" "daily"])))
  (is (= {:args ["clone" "--odd"] :opts {:help true :jobs 4}} (main/parse ["clone" "-h" "--" "--odd"]))))

(defn- with-mirror [name]
  (let [upstream (s/upstream! name {"README" "hello\n"})
        mirror (path (str name "-mirror.git"))]
    (s/git s/*root* "init" "--quiet" "--bare" mirror)
    {:upstream upstream
     :mirror mirror
     :repository {:id name :url (:url upstream) :dir (path "clones" name) :layout :plain :branch "main" :mirrors [mirror]}}))

(defn- branches-of [bare]
  (s/git bare "for-each-ref" "--format=%(refname)" "refs/heads"))

(deftest updating-syncs-mirrors-only-with-push-mirrors
  (let [{:keys [upstream mirror repository]} (with-mirror "app")]
    (git/clone! repository)
    (is (main/update-all! {} [repository] 1))
    (is (= "" (branches-of mirror)))
    (is (main/update-all! {:push-mirrors false} [repository] 1))
    (is (= "" (branches-of mirror)))
    (s/push! upstream "second" {"README" "two\n"})
    (is (main/update-all! {:push-mirrors true} [repository] 1))
    (is (= (s/git (:url upstream) "rev-parse" "main") (s/git mirror "rev-parse" "main")))))

(deftest the-mirror-command-fetches-and-syncs
  (let [{:keys [upstream mirror repository]} (with-mirror "app")
        plain {:id "plain" :url (:url (s/upstream! "plain" {"README" "x\n"})) :dir (path "clones" "plain") :layout :plain :branch "main" :mirrors []}
        missing (assoc repository :id "missing" :dir (path "clones" "missing"))]
    (git/clone! repository)
    (git/clone! plain)
    (s/push! upstream "second" {"README" "two\n"})
    (is (main/mirror-all! [repository plain missing] 2))
    (is (= (s/git (:url upstream) "rev-parse" "main") (s/git mirror "rev-parse" "main")))
    (testing "a mirror that cannot be synced is a warning"
      (is (main/mirror-all! [(update repository :mirrors conj (path "nowhere.git"))] 1)))
    (testing "a failed fetch fails"
      (s/git (path "clones" "plain") "remote" "set-url" "origin" (path "gone.git"))
      (is (false? (main/mirror-all! [(assoc repository :dir (path "clones" "plain"))] 1))))))

(deftest listing-directories-prints-every-checkout-once
  (let [plain {:id "plain" :url (:url (s/upstream! "plain" {"README" "x\n"})) :dir (path "clones" "plain") :layout :plain :branch "main"}
        trees {:id "trees" :url (:url (s/upstream! "trees" {"README" "y\n"})) :dir (path "clones" "trees") :layout :worktrees :branch "main"}
        missing {:id "missing" :url "nowhere" :dir (path "clones" "missing") :layout :plain :branch "main"}
        canonical #(str (fs/canonicalize %))]
    (git/clone! plain)
    (git/clone! trees)
    (s/git (:dir plain) "worktree" "add" "--quiet" "-b" "topic" (path "clones" "plain-topic"))
    (fs/create-sym-link (path "link") (path "clones"))
    (let [out (with-out-str (is (main/list-dirs! [plain trees missing (assoc plain :id "again" :dir (path "link" "plain"))])))]
      (is (= (sort (map canonical [(path "clones" "plain") (path "clones" "plain-topic")
                                   (path "clones" "trees" "main") (path "clones" "trees" git/development)]))
             (str/split-lines out))))))

(deftest a-config-file-that-does-not-exist-is-refused
  ;; generate! is stubbed: on the fallback to defaults it would rewrite the
  ;; sessions of this machine's real manifest
  (let [ran (atom false)]
    (with-redefs [aro.main/generate! (fn [& _] (reset! ran true) true)]
      (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
        (is (= 1 (main/run ["--config" (path "missing.edn") "generate"])))
        (is (= 1 (main/run ["generate" "--config"])))))
    (is (false? @ran) "no command runs on the built-in defaults"))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"--config needs a value" (main/parse ["generate" "--config"]))))

(deftest an-existing-config-file-is-read
  (spit (path "config.edn") (pr-str {:git-root (path "git")}))
  (let [seen (atom nil)]
    (with-redefs [aro.main/generate! (fn [config _] (reset! seen (:git-root config)) true)]
      (is (= 0 (main/run ["--config" (path "config.edn") "generate"]))))
    (is (= (path "git") @seen))))

(deftest a-failing-fetched-hook-fails-the-fetch
  (doseq [hook [["false"] [(path "no-such-hook")]]]
    (fs/delete-if-exists (path "inventory.edn"))
    (let [config {:inventory (path "inventory.edn") :hooks {:fetched hook}
                  :sources [{:id "a" :github {:org "a"}}]}]
      (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
        (is (false? (main/fetch! config [] :get-json (getter {"https://api.github.com/orgs/a/repos?type=all&per_page=100&page=1"
                                                             [{:name "x" :full_name "a/x" :default_branch "main" :topics []}]})))
            (pr-str hook))))))
