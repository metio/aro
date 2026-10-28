;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.orphans-test
  (:require [aro.git :as git]
            [aro.orphans :as orphans]
            [aro.support :as s :refer [path]]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :each s/with-root)

(defn- clone! [upstream & segments]
  (let [dir (apply path "git" segments)]
    (fs/create-dirs (fs/parent dir))
    (s/git s/*root* "clone" "--quiet" (:url upstream) dir)
    dir))

(deftest orphans-are-the-clones-no-repository-or-group-names
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        managed (clone! upstream "example.org" "team" "app")
        _ (clone! upstream "example.org" "notes" "inside")
        orphan (clone! upstream "example.org" "old" "app")
        config {:git-root (path "git") :groups [{:id "g" :dirs [(path "git" "example.org" "notes")]}]}]
    (fs/create-dirs (path "git" "example.org" "empty"))
    (is (= [orphan] (orphans/orphans config [{:dir managed}])))))

(deftest removing-keeps-clones-with-local-work
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        root (path "git")]
    (testing "a clone origin holds entirely goes, with the directories it leaves empty"
      (let [dir (clone! upstream "example.org" "old" "deep" "app")]
        (is (= {:status :removed} (orphans/remove! dir root)))
        (is (not (fs/exists? (path "git" "example.org"))))
        (is (fs/exists? root))))
    (testing "untracked files"
      (let [dir (clone! upstream "a" "untracked")]
        (spit (path "git" "a" "untracked" "notes") "mine\n")
        (is (= :kept (:status (orphans/remove! dir root))))
        (is (fs/exists? dir))))
    (testing "commits on no remote"
      (let [dir (clone! upstream "a" "ahead")]
        (spit (path "git" "a" "ahead" "README") "changed\n")
        (s/git dir "commit" "--quiet" "--all" "--message" "mine")
        (is (= ["1 commit on no remote-tracking branch"] (:reasons (orphans/remove! dir root))))))
    (testing "stash entries"
      (let [dir (clone! upstream "a" "stashed")]
        (spit (path "git" "a" "stashed" "README") "changed\n")
        (s/git dir "stash" "--quiet")
        (is (= ["1 stash entry"] (:reasons (orphans/remove! dir root))))))
    (testing "a repository without remote"
      (let [dir (path "git" "a" "local")]
        (fs/create-dirs dir)
        (s/git dir "init" "--quiet" "--initial-branch" "main")
        (spit (path "git" "a" "local" "README") "x\n")
        (s/git dir "add" "README")
        (s/git dir "commit" "--quiet" "--message" "first")
        (is (= :kept (:status (orphans/remove! dir root))))))
    (testing "a clean clone of the worktrees layout goes"
      (let [r {:id "example.org:team/clean" :url (:url upstream) :dir (path "git" "c" "clean") :layout :worktrees :branch "main"}]
        (git/clone! r)
        (is (= {:status :removed} (orphans/remove! (:dir r) root)))))
    (testing "a stash of the worktrees layout"
      (let [r {:id "example.org:team/stashed" :url (:url upstream) :dir (path "git" "c" "stashed") :layout :worktrees :branch "main"}]
        (git/clone! r)
        (spit (path "git" "c" "stashed" "main" "README") "changed\n")
        (s/git (path "git" "c" "stashed" "main") "stash" "--quiet")
        (is (= ["1 stash entry"] (:reasons (orphans/remove! (:dir r) root))))))
    (testing "a changed worktree of the worktrees layout"
      (let [r {:id "example.org:team/app" :url (:url upstream) :dir (path "git" "b" "app") :layout :worktrees :branch "main"}]
        (git/clone! r)
        (spit (path "git" "b" "app" "development" "README") "changed\n")
        (is (= :kept (:status (orphans/remove! (:dir r) root))))))))
