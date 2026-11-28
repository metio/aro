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

(deftest a-request-without-tunnel-starts-no-process
  (testing "the token travels in this process only, never on a command line or a child's stdin"
    (server/with-server (github 3)
      (fn [{:keys [port]}]
        (with-redefs [babashka.process/process (fn [& _] (throw (ex-info "a process was started" {})))
                      briko.process/run (fn [& _] (throw (ex-info "a process was started" {})))]
          (is (= 3 (count (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "metio"}} token)))))))))

(deftest plain-http-to-a-remote-forge-is-refused-before-connecting
  (let [e (try (list-through {:id "m" :github {:url "http://forge.invalid" :org "metio"}} token)
               nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (= :http (:aro/problem (ex-data e))))
    (is (re-find #"plain http is refused" (ex-message e)))
    (is (not (str/includes? (ex-message e) token)))))

(deftest an-answer-over-the-limit-is-refused
  (with-redefs [forge/limits (assoc forge/limits :max-bytes 1000)]
    (testing "through briko.http, by its declared length"
      (server/with-server (constantly {:status 200 :length 5000})
        (fn [{:keys [port]}]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"larger than 1000 bytes"
                                (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "metio"}} nil))))))
    (testing "through curl, by what arrives, before the JSON reader sees it"
      (server/with-server (constantly {:status 200 :body (json-array (map github-repo (range 20)))})
        (fn [{:keys [port]}]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"failed: the answer is larger than 1000 bytes"
                                (forge/list-repositories {:id "m" :github {:url "http://forge.test" :org "metio"}}
                                                         (forge/json-getter ["--connect-to" (str "forge.test:80:127.0.0.1:" port)])
                                                         nil))))))
    (testing "a negative control: the same page within the limit lists"
      (server/with-server (github 1)
        (fn [{:keys [port]}]
          (is (= 1 (count (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "metio"}} nil)))))))))

(deftest a-renamed-organization-is-followed-with-its-token
  (server/with-server (fn [{:keys [target] :as request}]
                        (if (str/starts-with? target "/orgs/old/")
                          {:status 301 :headers {"Location" (str/replace target "/orgs/old/" "/orgs/metio/")}}
                          ((github 2) request)))
    (fn [{:keys [port requests]}]
      (is (= 2 (count (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "old"}} token))))
      (is (every? #(= (str "Bearer " token) (get-in % [:headers "authorization"])) @requests)))))

(deftest a-redirect-to-another-origin-drops-the-token
  (server/with-server (github 2)
    (fn [other]
      (server/with-server (fn [{:keys [target]}]
                            {:status 302 :headers {"Location" (str "http://127.0.0.1:" (:port other) target)}})
        (fn [{:keys [port]}]
          (is (= 2 (count (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "metio"}} token))))
          (is (seq @(:requests other)))
          (is (not-any? #(contains? (:headers %) "authorization") @(:requests other))))))))

(deftest endless-redirects-end
  (server/with-server (fn [{:keys [target]}] {:status 302 :headers {"Location" target}})
    (fn [{:keys [port requests]}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"more than 5 redirects"
                            (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "metio"}} nil)))
      (is (= 6 (count @requests))))))

(deftest a-page-naming-a-key-twice-is-refused
  (server/with-server (constantly {:status 200 :body "[{\"name\":\"a\",\"name\":\"b\",\"full_name\":\"m/a\"}]"})
    (fn [{:keys [port]}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no JSON aro reads"
                            (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "metio"}} nil))))))

(deftest a-forge-message-reaches-the-error-on-one-line
  (server/with-server (constantly {:status 500 :body (str "line one\n\u001b[31mred\u001b[0m " (apply str (repeat 400 "x")))})
    (fn [{:keys [port]}]
      (let [message (try (list-through {:id "m" :github {:url (str "http://127.0.0.1:" port) :org "metio"}} nil)
                         (catch clojure.lang.ExceptionInfo e (ex-message e)))]
        (is (str/includes? message "HTTP 500: line one"))
        (is (not (re-find #"[\p{Cc}]" message)))
        (is (< (count message) 450))))))

(def ^:private forge-url (str "https://" server/forge-host))

(defn- tunnel-getter
  "The JSON getter of a tunnel whose forwarded port is `port`, trusting
  the test certificate unless `trust?` is false."
  [port & {:keys [trust?] :or {trust? true}}]
  (forge/json-getter (cond-> ["--connect-to" (str server/forge-host ":443:127.0.0.1:" port)
                              "--connect-to" (str "git.aro.test:443:127.0.0.1:" port)]
                       trust? (into ["--cacert" "test/aro/forge.pem"]))))

(defn- refusal-of [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(deftest a-tunnel-reaches-an-https-forge-by-its-own-name
  (testing "the request goes to the forwarded port, while TLS, SNI and Host use the forge's name"
    (server/with-server (gitlab 150) {:tls? true}
      (fn [{:keys [port requests]}]
        (let [records (forge/list-repositories {:id "gl" :gitlab {:url forge-url :group "g"}} (tunnel-getter port) token)]
          (is (= 148 (count records)))
          (is (= server/forge-host (:host (first records))))
          (is (= ["/api/v4/groups/g/projects?include_subgroups=true&with_shared=false&order_by=path&sort=asc&per_page=100&page=1"
                  "/api/v4/groups/g/projects?include_subgroups=true&with_shared=false&order_by=path&sort=asc&per_page=100&page=2"]
                 (map :target @requests)))
          (is (every? #(= server/forge-host (get-in % [:headers "host"])) @requests))
          (is (every? #(= (str "Bearer " token) (get-in % [:headers "authorization"])) @requests)))))))

(deftest a-tunnel-refuses-a-certificate-for-another-host
  (server/with-server (github 2) {:tls? true}
    (fn [{:keys [port requests]}]
      (testing "the forge's URL names a host the certificate does not"
        (let [e (refusal-of #(forge/list-repositories {:id "m" :github {:url "https://git.aro.test" :org "metio"}}
                                                      (tunnel-getter port) token))]
          (is (= :http (:aro/problem (ex-data e))))
          (is (not (str/includes? (pr-str (ex-message e) (ex-data e)) token)))))
      (testing "the system's trust store does not know the test certificate"
        (is (= :http (:aro/problem (ex-data (refusal-of #(forge/list-repositories {:id "m" :github {:url forge-url :org "metio"}}
                                                                                  (tunnel-getter port :trust? false) token)))))))
      (is (empty? @requests) "no refused handshake reached the forge")
      (testing "a negative control: the forge's own name lists through the same server"
        (is (= 2 (count (forge/list-repositories {:id "m" :github {:url forge-url :org "metio"}} (tunnel-getter port) token))))))))

(deftest a-tunnel-follows-a-same-origin-redirect-with-its-token
  (server/with-server (fn [{:keys [target] :as request}]
                        (if (str/starts-with? target "/orgs/old/")
                          {:status 301 :headers {"Location" (str forge-url (str/replace target "/orgs/old/" "/orgs/metio/"))}}
                          ((github 2) request)))
    {:tls? true}
    (fn [{:keys [port requests]}]
      (is (= 2 (count (forge/list-repositories {:id "m" :github {:url forge-url :org "old"}} (tunnel-getter port) token))))
      (is (every? #(= (str "Bearer " token) (get-in % [:headers "authorization"])) @requests)))))

(deftest a-tunnel-failure-names-the-status-and-the-forge-message
  (server/with-server (constantly {:status 401 :body "{\"message\":\"Bad credentials\"}"}) {:tls? true}
    (fn [{:keys [port]}]
      (let [e (refusal-of #(forge/list-repositories {:id "m" :github {:url forge-url :org "metio"}} (tunnel-getter port) token))]
        (is (= :http (:aro/problem (ex-data e))))
        (is (str/includes? (ex-message e) "Bad credentials"))
        (is (not (str/includes? (pr-str (ex-message e) (ex-data e)) token)))))))
