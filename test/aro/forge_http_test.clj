;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.forge-http-test
  "Listings against loopback servers that page the way GitHub, Gitea or
  Forgejo (Codeberg) and GitLab do: a page size the server caps, a page
  number, Link headers and, past the end, an empty list."
  (:require [aro.forge :as forge]
            [aro.http-server :as server]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private token
  "A token assembled at run time, so no credential-shaped literal sits in
  the source."
  (apply str "tok" (repeat 3 "-x9")))

(defn- page-of
  "The items of 1-based `page` of `items` in pages of `size`."
  [items page size]
  (vec (take size (drop (* (dec page) size) items))))

(defn- link-header
  "A Link header in the shape the three forges send, next and last."
  [port path params page last-page]
  (str/join ", " (cond-> []
                   (< page last-page) (conj (str "<http://127.0.0.1:" port path "?" params "&page=" (inc page) ">; rel=\"next\""))
                   true (conj (str "<http://127.0.0.1:" port path "?" params "&page=" last-page ">; rel=\"last\"")))))

(defn- json-array [items]
  (str "[" (str/join "," items) "]"))

(defn- github-repo [i]
  (str "{\"id\":" i ",\"name\":\"r" i "\",\"full_name\":\"metio/r" i "\",\"default_branch\":\"main\","
       "\"archived\":" (= 0 (mod i 7)) ",\"fork\":false,\"disabled\":" (= 3 i) ",\"topics\":[\"b\",\"a\"],"
       "\"custom_properties\":{\"release\":\"maven-central\",\"unset\":null},"
       "\"owner\":{\"login\":\"metio\",\"id\":1},\"description\":\"Größe \\\"quoted\\\" \\u00e9\"}"))

(defn- github
  "GitHub: per_page up to 100, page from 1, Link with next and last, an
  empty list after the last page."
  [total]
  (let [items (mapv github-repo (range total))]
    (fn [{:keys [target port]}]
      (let [q (server/query target)
            size (min 100 (parse-long (get q "per_page" "30")))
            page (parse-long (get q "page" "1"))
            last-page (max 1 (long (Math/ceil (/ total (double size)))))]
        (if (= "/orgs/metio/repos" (server/path-of target))
          {:status 200
           :headers {"Content-Type" "application/json; charset=utf-8"
                     "Link" (link-header port "/orgs/metio/repos" (str "type=all&per_page=" size) page last-page)}
           :body (json-array (page-of items page size))}
          {:status 404 :body "{\"message\":\"Not Found\"}"})))))

(defn- forgejo-repo [i]
  (str "{\"id\":" i ",\"name\":\"r" i "\",\"full_name\":\"o/r" i "\",\"default_branch\":\"main\","
       "\"archived\":false,\"fork\":" (= 1 i) ",\"empty\":" (= 2 i) ",\"topics\":[],\"owner\":{\"login\":\"o\"}}"))

(defn- forgejo
  "Gitea and Forgejo: limit capped at the server's MAX_RESPONSE_ITEMS
  without saying so (`cap`), page from 1, Link and X-Total-Count."
  [total cap]
  (let [items (mapv forgejo-repo (range total))]
    (fn [{:keys [target port]}]
      (let [q (server/query target)
            size (min cap (parse-long (get q "limit" "30")))
            page (parse-long (get q "page" "1"))
            last-page (max 1 (long (Math/ceil (/ total (double size)))))]
        {:status 200
         :headers {"Content-Type" "application/json;charset=utf-8"
                   "X-Total-Count" (str total)
                   "Link" (link-header port (server/path-of target) (str "limit=" size) page last-page)}
         :body (json-array (page-of items page size))}))))

(defn- gitlab-project [i]
  (str "{\"id\":" i ",\"path\":\"p" i "\",\"path_with_namespace\":\"g/sub/p" i "\","
       "\"namespace\":{\"id\":9,\"full_path\":\"g/sub\",\"kind\":\"group\"},\"default_branch\":\"main\","
       "\"archived\":false,\"topics\":[\"z\"],\"empty_repo\":" (= 5 i) ","
       "\"repository_access_level\":\"" (if (= 6 i) "disabled" "enabled") "\""
       (when (= 7 i) ",\"forked_from_project\":{\"id\":1}") "}"))

(defn- gitlab
  "GitLab: per_page up to 100, page from 1, Link, X-Next-Page, X-Total and
  X-Total-Pages."
  [total]
  (let [items (mapv gitlab-project (range total))]
    (fn [{:keys [target port]}]
      (let [q (server/query target)
            size (min 100 (parse-long (get q "per_page" "20")))
            page (parse-long (get q "page" "1"))
            last-page (max 1 (long (Math/ceil (/ total (double size)))))]
        {:status 200
         :headers {"Content-Type" "application/json"
                   "X-Total" (str total)
                   "X-Total-Pages" (str last-page)
                   "X-Next-Page" (if (< page last-page) (str (inc page)) "")
                   "Link" (link-header port (server/path-of target) (str "per_page=" size) page last-page)}
         :body (json-array (page-of items page size))}))))

(defn- list-through [source token]
  (forge/list-repositories source (forge/json-getter []) token))

(deftest github-pages-to-the-end-over-http
  (server/with-server (github 250)
    (fn [{:keys [port requests]}]
      (let [records (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "metio"}} token)]
        (is (= 249 (count records)) "every repository but the disabled one")
        (is (= {:host "127.0.0.1" :path "metio/r0" :name "r0" :namespace "metio" :default-branch "main"
                :archived true :fork false :topics ["a" "b"] :properties {:release "maven-central"}}
               (first records)))
        (is (= ["/orgs/metio/repos?type=all&per_page=100&page=1"
                "/orgs/metio/repos?type=all&per_page=100&page=2"
                "/orgs/metio/repos?type=all&per_page=100&page=3"]
               (map :target @requests)))
        (is (every? #(= (str "Bearer " token) (get-in % [:headers "authorization"])) @requests))
        (is (every? #(= "application/vnd.github+json" (get-in % [:headers "accept"])) @requests))
        (is (not-any? #(str/includes? (:target %) token) @requests))))))

(deftest forgejo-pages-past-a-capped-limit-over-http
  (server/with-server (forgejo 75 30)
    (fn [{:keys [port requests]}]
      (let [records (list-through {:id "cb" :forgejo {:url (str "http://127.0.0.1:" port "/") :org "o"}} token)]
        (is (= 74 (count records)) "every repository but the empty one")
        (is (true? (:fork (second records))))
        (is (= ["/api/v1/orgs/o/repos?limit=50&page=1"
                "/api/v1/orgs/o/repos?limit=50&page=2"
                "/api/v1/orgs/o/repos?limit=50&page=3"]
               (map :target @requests)))
        (is (every? #(= (str "token " token) (get-in % [:headers "authorization"])) @requests))))))

(deftest gitlab-pages-until-an-empty-page-over-http
  (server/with-server (gitlab 200)
    (fn [{:keys [port requests]}]
      (let [records (list-through {:id "gl" :gitlab {:url (str "http://127.0.0.1:" port) :group "g"}} token)]
        (is (= 198 (count records)) "every project but the empty and the disabled one")
        (is (= {:host "127.0.0.1" :path "g/sub/p7" :name "p7" :namespace "g/sub" :default-branch "main"
                :archived false :fork true :topics ["z"]}
               (nth records 5)))
        (is (= 3 (count @requests)) "two full pages, then the empty third")
        (is (every? #(= (str "Bearer " token) (get-in % [:headers "authorization"])) @requests))))))

(deftest a-listing-without-token-sends-no-authorization
  (server/with-server (github 3)
    (fn [{:keys [port requests]}]
      (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "metio"}} nil)
      (is (= 2 (count @requests)) "a first page shorter than asked sets the page size, so an empty second page ends it")
      (is (not-any? #(contains? (:headers %) "authorization") @requests)))))

(deftest a-failed-request-names-the-status-and-the-forge-message-but-not-the-token
  (server/with-server (constantly {:status 401 :headers {"Content-Type" "application/json"}
                                   :body "{\"message\":\"Bad credentials\",\"documentation_url\":\"https://docs.github.com/rest\"}"})
    (fn [{:keys [port]}]
      (let [e (try (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "metio"}} token)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (some? e))
        (is (= :http (:aro/problem (ex-data e))))
        (is (str/includes? (ex-message e) "401"))
        (is (str/includes? (ex-message e) "Bad credentials"))
        (is (not (str/includes? (pr-str (ex-message e) (ex-data e)) token)))))))

(deftest a-body-that-is-no-json-fails-the-listing
  (server/with-server (constantly {:status 200 :headers {"Content-Type" "text/html"} :body "<html>sign in</html>"})
    (fn [{:keys [port]}]
      (is (thrown? Exception (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "metio"}} nil))))))

(deftest a-tunnel-reaches-the-forge-through-connect-to
  (testing "the forge's host name stays in the URL; the connection goes to the local port"
    (server/with-server (gitlab 150)
      (fn [{:keys [port requests]}]
        (let [get-json (forge/json-getter ["--connect-to" (str "forge.test:80:127.0.0.1:" port)])
              records (forge/list-repositories {:id "gl" :gitlab {:url "http://forge.test" :group "g"}} get-json token)]
          (is (= 148 (count records)))
          (is (= "forge.test" (:host (first records))))
          (is (= 2 (count @requests)))
          (is (every? #(= "forge.test" (get-in % [:headers "host"])) @requests))
          (is (every? #(= (str "Bearer " token) (get-in % [:headers "authorization"])) @requests)))))))
