;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.git-test
  (:require [aro.git :as git]
            [aro.support :as s :refer [path]]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :each s/with-root)

(defn- repository [upstream layout]
  {:id "example.org:team/app" :url (:url upstream) :dir (path "clones" "app") :layout layout :branch "main"})

(defn- statuses [result]
  (mapv (juxt #(str (fs/file-name (:checkout %))) :status) (:checkouts result)))

(defn- head-message [dir]
  (s/git dir "log" "-1" "--format=%s"))

(deftest a-plain-clone-is-an-ordinary-clone
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)]
    (is (= :cloned (git/clone! r)))
    (is (= :plain (git/layout-on-disk (:dir r))))
    (is (= "hello\n" (slurp (path "clones" "app" "README"))))
    (is (= :exists (git/clone! r)))))

(deftest a-worktrees-clone-has-main-and-development
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :worktrees)]
    (git/clone! r)
    (is (= :worktrees (git/layout-on-disk (:dir r))))
    (is (= #{".git" "main" "development"} (set (map #(str (fs/file-name %)) (fs/list-dir (:dir r))))))
    (is (= "origin/main" (s/git (path "clones" "app" "main") "rev-parse" "--abbrev-ref" "@{upstream}")))
    (testing "development has no upstream"
      (is (thrown? Exception (s/git (path "clones" "app" "development") "rev-parse" "--abbrev-ref" "@{upstream}"))))
    (is (= "hello\n" (slurp (path "clones" "app" "development" "README"))))))

(deftest updating-removes-the-push-urls-of-origin
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (assoc (repository upstream :plain) :mirrors [(path "mirror.git")])
        dir (path "clones" "app")]
    (git/clone! r)
    (s/git dir "config" "--add" "remote.origin.pushurl" (:url upstream))
    (s/git dir "config" "--add" "remote.origin.pushurl" (path "mirror.git"))
    (git/update! r)
    (is (thrown? Exception (s/git dir "config" "--get-all" "remote.origin.pushurl")))
    (is (= (:url upstream) (s/git dir "remote" "get-url" "--push" "origin")))))

(defn- bare! [name]
  (let [dir (path name)]
    (s/git s/*root* "init" "--quiet" "--bare" dir)
    dir))

(defn- head-of [bare ref]
  (s/git bare "rev-parse" ref))

(defn- mirrored [upstream layout & mirrors]
  (let [r (assoc (repository upstream layout) :mirrors (vec mirrors))]
    (git/clone! r)
    r))

(defn- mirror-statuses [result]
  (mapv :status (:mirrors result)))

(deftest a-mirror-without-the-branch-gets-it
  (doseq [layout [:plain :worktrees]]
    (let [upstream (s/upstream! (name layout) {"README" "hello\n"})
          mirror (bare! (str (name layout) "-mirror.git"))
          r (assoc (mirrored upstream layout mirror) :dir (path "clones" "app"))
          result (git/sync-mirrors! r)]
      (is (= [:updated] (mirror-statuses result)))
      (is (str/includes? (:message (first (:mirrors result))) "created main"))
      (is (= (head-of (:url upstream) "main") (head-of mirror "main")))
      (fs/delete-tree (path "clones" "app")))))

(deftest a-mirror-behind-origin-is-fast-forwarded
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        mirror (bare! "mirror.git")
        r (mirrored upstream :plain mirror)]
    (git/sync-mirrors! r)
    (let [before (head-of mirror "main")]
      (s/push! upstream "second" {"README" "two\n"})
      (git/update! r)
      (let [result (git/sync-mirrors! r)]
        (is (= [:updated] (mirror-statuses result)))
        (is (str/includes? (:message (first (:mirrors result))) (str "from " before " to " (head-of (:url upstream) "main"))))
        (is (= (head-of (:url upstream) "main") (head-of mirror "main")))))))

(deftest a-mirror-equal-to-origin-is-left-alone
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (mirrored upstream :plain (bare! "mirror.git"))]
    (git/sync-mirrors! r)
    (is (= {:status :done :mirrors []} (git/sync-mirrors! r)))))

(defn- commit-on-mirror! [mirror base message]
  (let [work (path (str "mirror-work-" message))]
    (s/git s/*root* "clone" "--quiet" mirror work)
    (when base (s/git work "reset" "--quiet" "--hard" base))
    (s/commit! work message {"README" (str message "\n")})
    (s/git work "push" "--quiet" "--force" "origin" "HEAD:main")))

(deftest a-mirror-ahead-of-origin-is-left-alone-and-reported
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        mirror (bare! "mirror.git")
        r (mirrored upstream :plain mirror)
        origin (head-of (:url upstream) "main")]
    (git/sync-mirrors! r)
    (commit-on-mirror! mirror nil "extra")
    (let [ahead (head-of mirror "main")
          [result] (:mirrors (git/sync-mirrors! r))]
      (is (= :warned (:status result)))
      (is (str/includes? (:message result) (str "its " ahead " is ahead of origin's " origin)))
      (is (= mirror (:mirror result)))
      (is (= ahead (head-of mirror "main")))
      (is (= "" (s/git (path "clones" "app") "for-each-ref" "refs/aro"))))))

(deftest a-mirror-diverged-from-origin-is-left-alone-and-reported
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        mirror (bare! "mirror.git")
        r (mirrored upstream :plain mirror)]
    (git/sync-mirrors! r)
    (commit-on-mirror! mirror nil "theirs")
    (s/push! upstream "ours" {"src" "x\n"})
    (git/update! r)
    (let [theirs (head-of mirror "main")
          [result] (:mirrors (git/sync-mirrors! r))]
      (is (= :warned (:status result)))
      (is (str/includes? (:message result) "have diverged"))
      (is (str/includes? (:message result) theirs))
      (is (str/includes? (:message result) (head-of (:url upstream) "main")))
      (is (= theirs (head-of mirror "main"))))))

(deftest an-unreachable-mirror-is-reported-and-the-others-synced
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        good (bare! "mirror.git")
        r (mirrored upstream :plain (path "nowhere.git") good)
        result (git/sync-mirrors! r)]
    (is (= :done (:status result)))
    (is (= [:warned :updated] (mirror-statuses result)))
    (is (str/includes? (:message (first (:mirrors result))) "cannot be reached"))
    (is (= (head-of (:url upstream) "main") (head-of good "main")))))

(deftest a-rejected-push-is-reported
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        mirror (bare! "mirror.git")
        hook (path "mirror.git" "hooks" "pre-receive")
        r (mirrored upstream :plain mirror)]
    (spit hook "#!/bin/sh\nexit 1\n")
    (.setExecutable (fs/file hook) true)
    (let [[result] (:mirrors (git/sync-mirrors! r))]
      (is (= :warned (:status result)))
      (is (str/includes? (:message result) "pushing main to")))))

(deftest tags-reach-the-mirror-and-differing-ones-are-reported
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        work (:work upstream)
        mirror (bare! "mirror.git")
        r (mirrored upstream :plain mirror)]
    (s/git work "tag" "--annotate" "--message" "one" "v1")
    (s/git work "tag" "v2")
    (s/push! upstream "second" {"README" "two\n"})
    (s/git work "tag" "v3" "HEAD")
    (s/git work "push" "--quiet" "origin" "v1" "v2" "v3")
    (commit-on-mirror! mirror nil "other")
    (s/git (path "mirror-work-other") "tag" "v2")
    (s/git (path "mirror-work-other") "push" "--quiet" "origin" "v2")
    (s/git mirror "update-ref" "-d" "refs/heads/main")
    (let [mirror-v2 (head-of mirror "refs/tags/v2")
          result (git/sync-mirrors! r)
          messages (map :message (:mirrors result))]
      (is (= [:updated :updated :warned] (mirror-statuses result)))
      (is (some #(str/includes? % "pushed 2 tags") messages))
      (is (some #(str/includes? % (str "tag v2 on " mirror " left as it is: it points at " mirror-v2)) messages))
      (is (= (head-of (:url upstream) "refs/tags/v1") (head-of mirror "refs/tags/v1")))
      (is (= "tag" (s/git mirror "cat-file" "-t" "refs/tags/v1")))
      (is (= (head-of (:url upstream) "refs/tags/v3") (head-of mirror "refs/tags/v3")))
      (is (= mirror-v2 (head-of mirror "refs/tags/v2"))))))

(deftest a-repository-without-mirrors-syncs-nothing
  (is (= {:status :done :mirrors []} (git/sync-mirrors! {:dir (path "nowhere") :branch "main" :mirrors []})))
  (is (= :missing (:status (git/sync-mirrors! {:dir (path "nowhere") :branch "main" :mirrors [(path "m.git")]})))))

(deftest a-failed-clone-leaves-nothing-behind
  (let [upstream (s/upstream! "app" {"README" "hello\n"})]
    (doseq [layout [:plain :worktrees]]
      (let [r (assoc (repository upstream layout) :branch "master" :dir (path "clones" (name layout)))]
        (is (thrown? Exception (git/clone! r)))
        (is (nil? (git/layout-on-disk (:dir r))))
        (is (thrown? Exception (git/clone! r)))))))

(deftest a-clone-refuses-a-directory-with-files
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)]
    (fs/create-dirs (:dir r))
    (spit (path "clones" "app" "notes") "mine")
    (is (thrown-with-msg? Exception #"is not empty" (git/clone! r)))))

(deftest updating-fast-forwards-around-unrelated-local-changes
  (let [upstream (s/upstream! "app" {"README" "hello\n" "src" "one\n"})
        r (repository upstream :plain)]
    (git/clone! r)
    (spit (path "clones" "app" "README") "edited\n")
    (spit (path "clones" "app" "scratch") "untracked\n")
    (s/push! upstream "second" {"src" "two\n"})
    (is (= [["app" :updated]] (statuses (git/update! r))))
    (is (= "two\n" (slurp (path "clones" "app" "src"))))
    (is (= "edited\n" (slurp (path "clones" "app" "README"))))
    (is (= "untracked\n" (slurp (path "clones" "app" "scratch"))))))

(deftest updating-rolls-back-when-local-changes-conflict-with-incoming-ones
  (let [upstream (s/upstream! "app" {"README" "hello\n" "src" "one\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (spit (path "clones" "app" "README") "edited\n")
    (spit (path "clones" "app" "src") "staged\n")
    (s/git dir "add" "src")
    (spit (path "clones" "app" "scratch") "untracked\n")
    (s/push! upstream "second" {"README" "upstream\n" "scratch" "theirs\n"})
    (let [[result] (:checkouts (git/update! r))]
      (is (= :skipped (:status result)))
      (is (str/includes? (:message result) "README")))
    (is (= "edited\n" (slurp (path "clones" "app" "README"))))
    (is (= "untracked\n" (slurp (path "clones" "app" "scratch"))))
    (is (= "initial" (head-message dir)))
    (testing "staged changes stay staged"
      (is (= "src" (s/git dir "diff" "--cached" "--name-only"))))
    (is (empty? (s/git dir "stash" "list")))))

(deftest updating-merges-local-changes-with-incoming-ones-to-the-same-file
  (let [upstream (s/upstream! "app" {"README" "a\nb\nc\nd\ne\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (spit (path "clones" "app" "README") "mine\nb\nc\nd\ne\n")
    (s/push! upstream "second" {"README" "a\nb\nc\nd\ntheirs\n"})
    (is (= [["app" :updated]] (statuses (git/update! r))))
    (is (= "second" (head-message dir)))
    (is (= "mine\nb\nc\nd\ntheirs\n" (slurp (path "clones" "app" "README"))))
    (is (empty? (s/git dir "stash" "list")))))

(deftest updating-keeps-older-stash-entries
  (let [upstream (s/upstream! "app" {"README" "hello\n" "src" "one\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (spit (path "clones" "app" "README") "stashed\n")
    (s/git dir "stash" "push" "--quiet")
    (spit (path "clones" "app" "README") "edited\n")
    (s/push! upstream "second" {"src" "two\n"})
    (is (= [["app" :updated]] (statuses (git/update! r))))
    (is (= "edited\n" (slurp (path "clones" "app" "README"))))
    (is (= 1 (count (str/split-lines (s/git dir "stash" "list")))))))

(deftest updating-keeps-ignored-files-the-update-would-overwrite
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (spit (path "clones" "app" ".git" "info" "exclude") ".env\n")
    (spit (path "clones" "app" ".env") "secret\n")
    (s/push! upstream "second" {".env" "upstream\n"})
    (testing "a fast-forward"
      (is (= [["app" :skipped]] (statuses (git/update! r))))
      (is (= "secret\n" (slurp (path "clones" "app" ".env")))))
    (testing "a rebase of own commits"
      (spit (path "clones" "app" "README") "mine\n")
      (s/git dir "commit" "--quiet" "--all" "--message" "mine")
      (is (= [["app" :skipped]] (statuses (git/update! r))))
      (is (= "secret\n" (slurp (path "clones" "app" ".env")))))))

(deftest updating-rebases-own-commits-and-keeps-local-changes
  (let [upstream (s/upstream! "app" {"README" "hello\n" "src" "one\n" "notes" "a\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (s/commit! dir "mine" {"notes" "b\n"})
    (spit (path "clones" "app" "README") "edited\n")
    (s/push! upstream "theirs" {"src" "two\n"})
    (is (= [["app" :updated]] (statuses (git/update! r))))
    (is (= ["mine" "theirs" "initial"] (str/split-lines (s/git dir "log" "--format=%s"))))
    (is (= "edited\n" (slurp (path "clones" "app" "README"))))
    (is (empty? (s/git dir "stash" "list")))))

(deftest updating-leaves-a-conflicting-rebase-undone
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (s/commit! dir "mine" {"README" "mine\n"})
    (s/push! upstream "theirs" {"README" "theirs\n"})
    (is (= [["app" :skipped]] (statuses (git/update! r))))
    (is (= "mine" (head-message dir)))
    (is (= "mine\n" (slurp (path "clones" "app" "README"))))
    (is (not (fs/exists? (path "clones" "app" ".git" "rebase-merge"))))))

(deftest updating-rebases-diverged-branches-whose-changes-merge-with-incoming-ones
  (let [upstream (s/upstream! "app" {"README" "a\nb\nc\nd\ne\n" "notes" "a\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (s/commit! dir "mine" {"notes" "b\n"})
    (spit (path "clones" "app" "README") "mine\nb\nc\nd\ne\n")
    (s/push! upstream "theirs" {"README" "a\nb\nc\nd\ntheirs\n"})
    (is (= [["app" :updated]] (statuses (git/update! r))))
    (is (= ["mine" "theirs" "initial"] (str/split-lines (s/git dir "log" "--format=%s"))))
    (is (= "mine\nb\nc\nd\ntheirs\n" (slurp (path "clones" "app" "README"))))))

(deftest updating-rolls-back-diverged-branches-whose-changes-conflict
  (let [upstream (s/upstream! "app" {"README" "hello\n" "notes" "a\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (s/commit! dir "mine" {"notes" "b\n"})
    (spit (path "clones" "app" "README") "edited\n")
    (s/push! upstream "theirs" {"README" "theirs\n"})
    (is (= [["app" :skipped]] (statuses (git/update! r))))
    (is (= "edited\n" (slurp (path "clones" "app" "README"))))
    (is (= "mine" (head-message dir)))))

(deftest updating-skips-branches-without-upstream
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (s/git dir "switch" "--quiet" "--create" "feature")
    (s/push! upstream "theirs" {"src" "x\n"})
    (is (= [["app" :skipped]] (statuses (git/update! r))))))

(deftest updating-a-worktrees-clone-moves-both-worktrees
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :worktrees)]
    (git/clone! r)
    (s/push! upstream "theirs" {"src" "x\n"})
    (is (= #{["main" :updated] ["development" :updated]} (set (statuses (git/update! r)))))
    (is (= "theirs" (head-message (path "clones" "app" "development"))))
    (is (= "theirs" (head-message (path "clones" "app" "main"))))))

(deftest updating-an-up-to-date-clone-is-current
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)]
    (git/clone! r)
    (is (= [["app" :current]] (statuses (git/update! r))))))

(deftest updating-a-missing-clone-reports-it
  (is (= :missing (:status (git/update! {:dir (path "nowhere") :branch "main"})))))

(deftest a-failing-fetch-fails
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)]
    (git/clone! r)
    (fs/delete-tree (:url upstream))
    (is (= :failed (:status (git/update! r))))))

(deftest updating-skips-submodules-the-clone-never-initialized
  (let [lib (s/upstream! "lib" {"README" "lib\n"})
        upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)]
    (s/git (:work upstream) "-c" "protocol.file.allow=always" "submodule" "add" "--quiet" (:url lib) "plugin")
    (s/push! upstream "add the plugin" {})
    (git/clone! r)
    (s/git (:dir r) "config" "fetch.recurseSubmodules" "on-demand")
    (s/push! lib "change the plugin" {"README" "lib 2\n"})
    (s/git (path "app-work" "plugin") "pull" "--quiet" "origin" "main")
    (s/push! upstream "move the plugin" {})
    (is (= :done (:status (git/update! r))))
    (is (= "move the plugin" (head-message (:dir r))))))

(deftest switching-to-worktrees-moves-the-checkout-as-it-is
  (let [upstream (s/upstream! "app" {"README" "hello\n" ".gitignore" "build/\n" "src" "one\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (spit (path "clones" "app" "README") "edited\n")
    (spit (path "clones" "app" "src") "staged\n")
    (s/git dir "add" "src")
    (spit (path "clones" "app" "scratch") "untracked\n")
    (fs/create-dirs (path "clones" "app" "build"))
    (spit (path "clones" "app" "build" "out") "ignored\n")
    (is (= (path "clones" "app" "main") (git/to-worktrees! dir "main")))
    (is (= :worktrees (git/layout-on-disk dir)))
    (let [main (path "clones" "app" "main")]
      (is (= "edited\n" (slurp (path "clones" "app" "main" "README"))))
      (is (= "untracked\n" (slurp (path "clones" "app" "main" "scratch"))))
      (is (= "ignored\n" (slurp (path "clones" "app" "main" "build" "out"))))
      (is (= #{"M  src" " M README" "?? scratch"} (set (str/split-lines (s/git main "status" "--porcelain"))))))
    (is (fs/exists? (path "clones" "app" "development" "README")))
    (is (not (fs/exists? (str dir ".aro-layout"))))))

(deftest switching-to-plain-keeps-the-chosen-worktree
  (let [upstream (s/upstream! "app" {"README" "hello\n" "src" "one\n"})
        r (repository upstream :worktrees)
        dir (path "clones" "app")]
    (git/clone! r)
    (spit (path "clones" "app" "main" "README") "edited\n")
    (spit (path "clones" "app" "main" "src") "staged\n")
    (s/git (path "clones" "app" "main") "add" "src")
    (is (= dir (git/to-plain! dir "main")))
    (is (= :plain (git/layout-on-disk dir)))
    (is (= "main" (s/git dir "branch" "--show-current")))
    (is (= #{"M  src" " M README"} (set (str/split-lines (s/git dir "status" "--porcelain")))))
    (is (= "development" (s/git dir "branch" "--list" "development" "--format=%(refname:short)")))
    (is (= [dir] (map #(first (str/split % #"\s+")) (str/split-lines (s/git dir "worktree" "list")))))))

(deftest switching-to-plain-refuses-other-worktrees-with-changes
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :worktrees)
        dir (path "clones" "app")]
    (git/clone! r)
    (spit (path "clones" "app" "development" "README") "work in progress\n")
    (is (thrown? Exception (git/to-plain! dir "main")))
    (is (= "work in progress\n" (slurp (path "clones" "app" "development" "README"))))))

(deftest switching-to-plain-refuses-detached-worktrees-with-their-own-commits
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :worktrees)
        dir (path "clones" "app")
        development (path "clones" "app" "development")]
    (git/clone! r)
    (s/git development "checkout" "--quiet" "--detach")
    (spit (path "clones" "app" "development" "README") "mine\n")
    (s/git development "commit" "--quiet" "--all" "--message" "mine")
    (let [commit (s/git development "rev-parse" "HEAD")]
      (is (thrown? Exception (git/to-plain! dir "main")))
      (is (= commit (s/git development "rev-parse" "HEAD"))))))

(deftest switching-to-plain-refuses-other-worktrees-with-ignored-files
  (let [upstream (s/upstream! "app" {"README" "hello\n" ".gitignore" "secret\n"})
        r (repository upstream :worktrees)
        dir (path "clones" "app")]
    (git/clone! r)
    (spit (path "clones" "app" "main" "secret") "kept\n")
    (spit (path "clones" "app" "development" "secret") "only copy\n")
    (is (thrown-with-msg? Exception #"development: !! secret" (git/to-plain! dir "main")))
    (is (= :worktrees (git/layout-on-disk dir)))
    (is (= "only copy\n" (slurp (path "clones" "app" "development" "secret"))))
    (testing "the kept worktree's ignored files stay with the plain clone"
      (fs/delete (path "clones" "app" "development" "secret"))
      (git/to-plain! dir "main")
      (is (= "kept\n" (slurp (path "clones" "app" "secret")))))))

(deftest switching-back-and-forth-round-trips
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (git/to-worktrees! dir "main")
    (git/to-plain! dir "main")
    (git/to-worktrees! dir "main")
    (is (= :worktrees (git/layout-on-disk dir)))
    (s/push! upstream "theirs" {"src" "x\n"})
    (is (= #{["main" :updated] ["development" :updated]} (set (statuses (git/update! (assoc r :layout :worktrees))))))))

(deftest maintenance-runs-the-tasks-of-the-schedule
  (let [upstream (s/upstream! "app" {"README" "hello\n"})]
    (doseq [layout [:plain :worktrees]]
      (let [r (assoc (repository upstream layout) :dir (path "clones" (name layout)))]
        (git/clone! r)
        (is (= :done (:status (git/maintain! r "hourly"))))
        (testing (str "the hourly commit-graph task ran in the " (name layout) " layout")
          (is (fs/exists? (path "clones" (name layout) ".git" "objects" "info" "commit-graphs"))))
        (testing (str "the hourly run fetches nothing in the " (name layout) " layout")
          (is (= "" (s/git (:dir r) "for-each-ref" "refs/prefetch"))))))
    (is (= :missing (:status (git/maintain! (assoc (repository upstream :plain) :dir (path "nowhere")) "daily"))))))

(defn- branches [dir]
  (set (str/split-lines (s/git dir "for-each-ref" "--format=%(refname:short)" "refs/heads"))))

(deftest pruning-deletes-branches-origin-holds
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")
        work (:work upstream)]
    (git/clone! r)
    (s/git dir "branch" "merged")
    (s/git dir "branch" "development")
    (s/git dir "switch" "--quiet" "-c" "wip")
    (s/commit! dir "wip" {"wip" "x\n"})
    (doseq [[branch file] [["landed" "landed"] ["abandoned" "abandoned"]]]
      (s/git dir "switch" "--quiet" "-c" branch "main")
      (s/commit! dir branch {file "x\n"})
      (s/git dir "push" "--quiet" "--set-upstream" "origin" branch))
    (s/git work "fetch" "--quiet" "origin")
    (s/git work "merge" "--quiet" "--ff-only" "origin/landed")
    (s/git work "push" "--quiet" "origin" "main" ":landed" ":abandoned")
    (s/git dir "switch" "--quiet" "-c" "current" "main")
    (s/git dir "fetch" "--quiet" "--prune" "origin")
    (testing "a dry run deletes nothing"
      (let [before (branches dir)
            result (git/prune-branches! r true)]
        (is (= #{"merged" "landed"} (set (:deleted result))))
        (is (= before (branches dir)))))
    (let [result (git/prune-branches! r false)]
      (is (= #{"merged" "landed"} (set (:deleted result))))
      (is (= [{:branch "abandoned" :message "its upstream is gone and it holds 1 commit origin/main lacks"}] (:kept result)))
      (is (= #{"main" "development" "current" "wip" "abandoned"} (branches dir))))))

(deftest pruning-a-plain-clone-keeps-the-branches-of-its-linked-worktrees
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")]
    (git/clone! r)
    (s/git dir "branch" "merged")
    (s/git dir "worktree" "add" "--quiet" "-b" "feat" (path "clones" "app-feat"))
    (let [result (git/prune-branches! r false)]
      (is (= ["merged"] (:deleted result)))
      (is (= #{"main" "feat"} (branches dir))))))

(deftest pruning-deletes-merged-branches-of-worktrees-whose-directory-is-gone
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)
        dir (path "clones" "app")
        gone (path "elsewhere" "app-merged")]
    (git/clone! r)
    (s/git dir "worktree" "add" "--quiet" "-b" "merged" gone)
    (fs/delete-tree gone)
    (is (= ["merged"] (:deleted (git/prune-branches! r false))))
    (is (= #{"main"} (branches dir)))))

(deftest pruning-keeps-the-branches-of-every-worktree
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :worktrees)
        gd (path "clones" "app" ".git")]
    (git/clone! r)
    (s/git (path "clones" "app" "main") "worktree" "add" "--quiet" "-b" "feature" (path "clones" "app" "feature"))
    (s/git (path "clones" "app" "main") "branch" "stale")
    (is (= ["stale"] (:deleted (git/prune-branches! r false))))
    (is (= #{"main" "development" "feature"} (branches gd)))))

(defn- current-branch [dir]
  (s/git dir "symbolic-ref" "--short" "HEAD"))

(defn- switched [result]
  (mapv #(select-keys % [:status :branch :message]) (:switches result)))

(defn- prune-switching [r & {:as opts}]
  (binding [git/*processes* (constantly [])]
    (apply git/prune-branches! r (:dry-run opts false) (mapcat identity (merge {:switch-merged true} (dissoc opts :dry-run))))))

(defn- merged-feature!
  "A clone in `layout` whose checkout `at` is on `feat`, which origin merged
  and deleted while the clone's main stayed behind. `merge` is :merge-commit
  or :fast-forward; with `upstream`, feat tracked origin/feat."
  [& {:keys [layout at merge upstream] :or {layout :plain merge :merge-commit}}]
  (let [up (s/upstream! "app" {"README" "hello\n"})
        r (repository up layout)
        _ (git/clone! r)
        dir (if at (path "clones" "app" at) (:dir r))
        work (:work up)]
    (s/git dir "switch" "--quiet" "-c" "feat")
    (s/commit! dir "feat" {"feat" "x\n"})
    (if upstream
      (s/git dir "push" "--quiet" "--set-upstream" "origin" "feat")
      (s/git dir "push" "--quiet" "origin" "feat"))
    (s/git work "fetch" "--quiet" "origin")
    (case merge
      :merge-commit (s/git work "merge" "--quiet" "--no-ff" "--no-edit" "origin/feat")
      :fast-forward (s/git work "merge" "--quiet" "--ff-only" "origin/feat"))
    (s/push! up "after" {"after" "y\n"})
    (s/git work "push" "--quiet" "origin" ":feat")
    (s/git dir "fetch" "--quiet" "--prune" "origin")
    (assoc r :checkout dir)))

(deftest switching-moves-a-plain-clone-off-a-branch-merged-by-a-merge-commit
  (let [r (merged-feature!)
        dir (:dir r)]
    (spit (path "clones" "app" "notes") "mine\n")
    (testing "without the option the checked-out branch stays"
      (is (= [] (:switches (git/prune-branches! r false))))
      (is (= "feat" (current-branch dir))))
    (testing "a dry run changes nothing"
      (let [result (prune-switching r :dry-run true)]
        (is (= [{:status :switched :branch "feat" :message "would switch app from feat to main"}] (switched result)))
        (is (= ["feat"] (:deleted result)))
        (is (= "feat" (current-branch dir)))))
    (let [result (prune-switching r)]
      (is (= [:switched] (map :status (:switches result))))
      (is (str/starts-with? (:message (first (:switches result))) "switched app from feat to main; main fast-forwarded"))
      (is (= ["feat"] (:deleted result)))
      (is (= 12 (count (get (:tips result) "feat"))))
      (is (= "main" (current-branch dir)))
      (is (= (s/git dir "rev-parse" "origin/main") (s/git dir "rev-parse" "main")))
      (is (= #{"main"} (branches dir)))
      (testing "untracked files carry over"
        (is (= "mine\n" (slurp (path "clones" "app" "notes"))))))))

(deftest switching-detects-a-fast-forward-merge-from-the-reflog
  (let [r (merged-feature! :merge :fast-forward)]
    (is (= [:switched] (map :status (:switches (prune-switching r)))))
    (is (= "main" (current-branch (:dir r))))
    (is (= #{"main"} (branches (:dir r))))))

(deftest switching-detects-a-fast-forward-merge-whose-upstream-is-gone
  (let [r (merged-feature! :merge :fast-forward :upstream true)]
    (s/git (:dir r) "reflog" "expire" "--expire=now" "--all")
    (is (= [:switched] (map :status (:switches (prune-switching r)))))
    (is (= "main" (current-branch (:dir r))))))

(deftest switching-leaves-a-branch-that-only-followed-origin
  (let [up (s/upstream! "app" {"README" "hello\n"})
        r (repository up :plain)
        dir (:dir r)]
    (git/clone! r)
    (s/git dir "switch" "--quiet" "-c" "fresh" "--track" "origin/main")
    (s/push! up "after" {"after" "y\n"})
    (git/update! r)
    (is (= (s/git dir "rev-parse" "origin/main") (s/git dir "rev-parse" "fresh")))
    (let [result (prune-switching r)]
      (is (= [] (:switches result)))
      (is (= "fresh" (current-branch dir)))
      (is (= #{"main" "fresh"} (branches dir))))))

(deftest switching-returns-the-development-worktree-to-development
  (let [r (merged-feature! :layout :worktrees :at "development")
        dev (:checkout r)
        gd (path "clones" "app" ".git")]
    (s/git dev "switch" "--quiet" "development")
    (s/commit! dev "own" {"own" "z\n"})
    (s/git dev "switch" "--quiet" "feat")
    (s/git (path "clones" "app" "main") "worktree" "add" "--quiet" "-b" "extra" (path "clones" "app" "extra"))
    (let [result (prune-switching r)]
      (is (= [{:status :switched :branch "feat"}] (mapv #(select-keys % [:status :branch]) (:switches result))))
      (is (str/includes? (:message (first (:switches result))) "switched development from feat to development; development rebased 1 commits onto origin/main"))
      (is (= "development" (current-branch dev)))
      (is (= "main" (current-branch (path "clones" "app" "main"))))
      (is (= "extra" (current-branch (path "clones" "app" "extra"))))
      (is (= "own" (head-message dev)))
      (is (= (s/git dev "rev-parse" "origin/main") (s/git dev "rev-parse" "development~1")))
      (is (= #{"main" "development" "extra"} (branches gd))))))

(deftest switching-skips-a-merged-branch-with-uncommitted-changes
  (let [r (merged-feature!)
        dir (:dir r)]
    (spit (path "clones" "app" "feat") "changed\n")
    (let [result (prune-switching r)]
      (is (= [{:status :skipped :branch "feat" :message "app stays on feat, which origin merged: it has uncommitted changes"}] (switched result)))
      (is (= "feat" (current-branch dir)))
      (is (= "changed\n" (slurp (path "clones" "app" "feat"))))
      (is (= #{"main" "feat"} (branches dir))))))

(deftest switching-rebases-local-commits-of-the-home-branch
  (let [r (merged-feature!)
        dir (:dir r)]
    (s/git dir "switch" "--quiet" "main")
    (s/commit! dir "local" {"local" "z\n"})
    (s/git dir "switch" "--quiet" "feat")
    (let [result (prune-switching r)]
      (is (= [:switched] (map :status (:switches result))))
      (is (= "main" (current-branch dir)))
      (is (= "local" (head-message dir)))
      (is (= (s/git dir "rev-parse" "origin/main") (s/git dir "rev-parse" "main~1"))))))

(deftest switching-skips-when-another-worktree-has-the-home-branch-checked-out
  (let [r (merged-feature!)
        dir (:dir r)]
    (s/git dir "worktree" "add" "--quiet" (path "clones" "app-main") "main")
    (is (= [{:status :skipped :branch "feat" :message "app stays on feat, which origin merged: main is checked out in another worktree"}]
           (switched (prune-switching r))))
    (is (= "feat" (current-branch dir)))))

(deftest switching-skips-a-checkout-a-process-works-in
  (let [r (merged-feature!)
        dir (:dir r)
        processes [{:pid 1 :command "bash" :cwd dir}
                   {:pid 2 :command "nvim" :cwd (path "clones" "app" "sub")}
                   {:pid 3 :command "hugo" :cwd (path "clones" "app-other")}]]
    (binding [git/*processes* (constantly processes)]
      (is (= [{:status :skipped :branch "feat" :message "app stays on feat, which origin merged: nvim (2) runs in it"}]
             (switched (git/prune-branches! r false :switch-merged true)))))
    (testing "without a process list the checkout counts as in use"
      (binding [git/*processes* (constantly nil)]
        (is (= "app stays on feat, which origin merged: the running processes cannot be listed"
               (:message (first (:switches (git/prune-branches! r false :switch-merged true))))))))
    (is (= "feat" (current-branch dir)))))

(deftest switching-waits-for-the-quiet-period
  (let [r (merged-feature!)
        result (prune-switching r :quiet-seconds (* 24 3600))]
    (is (= [{:status :skipped :branch "feat" :message "app stays on feat, which origin merged: its HEAD moved 0 hours ago, within the last 24"}]
           (switched result)))
    (is (= "feat" (current-branch (:dir r))))))

(deftest repositories-clone-over-ssh
  (is (= "github.com:metio/aro" (git/clone-url {:host "github.com" :path "metio/aro"}))))
