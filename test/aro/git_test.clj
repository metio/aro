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

(defn- push-urls [dir]
  (str/split-lines (s/git dir "config" "--get-all" "remote.origin.pushurl")))

(deftest a-push-reaches-the-repository-and-its-mirrors
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        mirror (path "mirror.git")
        _ (s/git s/*root* "init" "--quiet" "--bare" mirror)
        r (assoc (repository upstream :plain) :mirrors [mirror])
        dir (path "clones" "app")]
    (git/clone! r)
    (is (= [(:url upstream) mirror] (push-urls dir)))
    (spit (path "clones" "app" "README") "changed\n")
    (s/git dir "commit" "--quiet" "--all" "--message" "change")
    (s/git dir "push" "--quiet" "origin" "main")
    (is (= (s/git dir "rev-parse" "HEAD")
           (s/git (:url upstream) "rev-parse" "main")
           (s/git mirror "rev-parse" "main")))
    (testing "updating follows the mirrors of the config"
      (git/update! (assoc r :mirrors []))
      (is (thrown? Exception (push-urls dir))))))

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

(deftest updating-skips-when-local-changes-touch-incoming-files
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :plain)]
    (git/clone! r)
    (spit (path "clones" "app" "README") "edited\n")
    (s/push! upstream "second" {"README" "upstream\n"})
    (is (= [["app" :skipped]] (statuses (git/update! r))))
    (is (= "edited\n" (slurp (path "clones" "app" "README"))))
    (is (= "initial" (head-message (path "clones" "app"))))))

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

(deftest updating-skips-diverged-branches-whose-changes-touch-incoming-files
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

(deftest pruning-keeps-the-branches-of-every-worktree
  (let [upstream (s/upstream! "app" {"README" "hello\n"})
        r (repository upstream :worktrees)
        gd (path "clones" "app" ".git")]
    (git/clone! r)
    (s/git (path "clones" "app" "main") "worktree" "add" "--quiet" "-b" "feature" (path "clones" "app" "feature"))
    (s/git (path "clones" "app" "main") "branch" "stale")
    (is (= ["stale"] (:deleted (git/prune-branches! r false))))
    (is (= #{"main" "development" "feature"} (branches gd)))))
