;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.forge
  "Listing the repositories of a source from its forge: a GitHub or Forgejo
  organization or user, a GitLab group with its subgroups, a GitLab user, or
  every GitLab project the token's account is a member of. Each repository
  becomes a
  record of what the rest of aro needs: host, path, name, namespace, default
  branch, whether it is archived or a fork, its topics, and on GitHub the
  values of its custom properties."
  (:require [babashka.process :as process]
            [briko.http :as http]
            [briko.json :as json]
            [briko.process :as proc]
            [clojure.string :as str])
  (:import (java.net URI URLEncoder)
           (java.nio.charset StandardCharsets)))


(defn- encode [s]
  (URLEncoder/encode (str s) StandardCharsets/UTF_8))

(defn host-of [url]
  (.getHost (URI. url)))

(defn source-host
  "The host `source` records for its repositories: its :host, else the host
  of its forge's URL, which for GitHub is github.com unless the URL names a
  GitHub Enterprise server."
  [{:keys [host github gitlab forgejo]}]
  (or host
      (when github
        (let [api (some-> (:url github) host-of)]
          (if (or (nil? api) (= "api.github.com" api)) "github.com" api)))
      (some-> (or (:url gitlab) (:url forgejo)) host-of)))

(defn token
  "The token of `source`: the first line its :token-command prints, or nil
  without one."
  [{:keys [token-command id]}]
  (when (seq token-command)
    (let [{:keys [exit out err]} (apply process/shell {:out :string :err :string :continue true} token-command)
          line (first (str/split-lines (or out "")))]
      (when (or (not= 0 exit) (str/blank? line))
        (throw (ex-info (str "the :token-command of " id " printed no token" (when-not (str/blank? err) (str ": " (str/trim err))))
                        {:aro/problem :token :source id})))
      (str/trim line))))

(defn- pages
  "Every item of a paginated JSON list, requesting page 1, 2, … of `url-of`
  with `page-size` items until a page is empty or shorter than the first
  one. The first page's length is the size the server pages by, because a
  forge may cap the requested size without saying so. `get-json` returns
  the parsed body of a URL."
  [get-json page-size url-of]
  (loop [page 1 size nil items []]
    (let [batch (get-json (url-of page page-size))
          size (or size (count batch))]
      (if (or (empty? batch) (< (count batch) size))
        (into items batch)
        (recur (inc page) size (into items batch))))))

(defn- github-record [host {:keys [name full_name default_branch archived fork topics custom_properties]}]
  (let [properties (into (sorted-map) (remove (comp nil? val)) custom_properties)]
    (cond-> {:host host
             :path full_name
             :name name
             :namespace (first (str/split full_name #"/"))
             :default-branch default_branch
             :archived (boolean archived)
             :fork (boolean fork)
             :topics (vec (sort topics))}
      (seq properties) (assoc :properties properties))))

(defn- gitlab-record [host {:keys [path path_with_namespace namespace default_branch archived topics forked_from_project]}]
  {:host host
   :path path_with_namespace
   :name path
   :namespace (:full_path namespace)
   :default-branch default_branch
   :archived (boolean archived)
   :fork (some? forked_from_project)
   :topics (vec (sort topics))})

(defn- base-url [url]
  (str/replace url #"/+$" ""))

(defn- owned-by
  "The records of `records` in the namespace of `user`. The listing of the
  token's own repositories names nobody, so a token of another account
  yields none instead of that account's repositories."
  [user records]
  (filterv #(= (str/lower-case user) (str/lower-case (:namespace %))) records))

(declare listed-repositories)

(defn list-repositories
  "The repository records of `source`, each path once, since a repository
  created or deleted during the listing shifts the pages under it. `get-json` takes a URL and the request
  headers and returns the parsed JSON body. A source's :host replaces the host
  of its URL in the records, for a forge reached through another address.
  A GitHub, Forgejo or GitLab user with a token lists the repositories the
  token's account owns, private ones included; without one, only the public
  ones.
  Disabled repositories, and those without commits, cannot be cloned and
  are left out; a later fetch lists them once they have some. GitLab only
  reports an empty repository to a token."
  [source get-json token]
  (loop [[record & more :as records] (listed-repositories source get-json token) seen #{} kept []]
    (cond
      (empty? records) kept
      (seen (:path record)) (recur more seen kept)
      :else (recur more (conj seen (:path record)) (conj kept record)))))

(defn- listed-repositories
  [{:keys [github gitlab forgejo] :as source} get-json token]
  (cond
    github
    (let [headers (cond-> {"Accept" "application/vnd.github+json"}
                    token (assoc "Authorization" (str "Bearer " token)))
          url (base-url (:url github "https://api.github.com"))
          base (cond
                 (:org github) (str url "/orgs/" (encode (:org github)) "/repos?type=all")
                 token (str url "/user/repos?affiliation=owner")
                 :else (str url "/users/" (encode (:user github)) "/repos?type=owner"))]
      (cond->> (mapv #(github-record (source-host source) %)
                     (remove :disabled (pages #(get-json % headers) 100 #(str base "&per_page=" %2 "&page=" %1))))
        (and token (not (:org github))) (owned-by (:user github))))

    forgejo
    (let [headers (cond-> {} token (assoc "Authorization" (str "token " token)))
          url (base-url (:url forgejo))
          base (cond
                 (:org forgejo) (str url "/api/v1/orgs/" (encode (:org forgejo)) "/repos?")
                 token (str url "/api/v1/user/repos?")
                 :else (str url "/api/v1/users/" (encode (:user forgejo)) "/repos?"))]
      (cond->> (mapv #(github-record (source-host source) %)
                     (remove :empty (pages #(get-json % headers) 50 #(str base "limit=" %2 "&page=" %1))))
        (and token (not (:org forgejo))) (owned-by (:user forgejo))))

    gitlab
    (let [headers (cond-> {} token (assoc "Authorization" (str "Bearer " token)))
          url (base-url (:url gitlab))
          base (cond
                 (:group gitlab) (str url "/api/v4/groups/" (encode (:group gitlab)) "/projects?include_subgroups=true&with_shared=false&order_by=path&sort=asc")
                 (:membership gitlab) (if token
                                        (str url "/api/v4/projects?membership=true&order_by=path&sort=asc")
                                        (throw (ex-info (str "the source " (:id source) " lists the projects of its token's account and needs a :token-command")
                                                        {:aro/problem :invalid-source :source (:id source)})))
                 token (str url "/api/v4/projects?owned=true&order_by=path&sort=asc")
                 :else (str url "/api/v4/users/" (encode (:user gitlab)) "/projects?order_by=path&sort=asc"))]
      (cond->> (mapv #(gitlab-record (source-host source) %)
                     (remove #(or (:empty_repo %) (= "disabled" (:repository_access_level %)))
                             (pages #(get-json % headers) 100 #(str base "&per_page=" %2 "&page=" %1))))
        (and token (:user gitlab)) (owned-by (:user gitlab))))

    :else
    (throw (ex-info (str "the source " (:id source) " names none of :github, :gitlab or :forgejo")
                    {:aro/problem :invalid-source :source (:id source)}))))

(def limits
  "The bounds of one forge request. A page of 100 repositories measured
  298 KB to 559 KB on GitHub and gitlab.com (2026-11-28), its largest
  repository 7 KB, so 8 MiB leaves room for repositories with long
  descriptions and many topics or properties while keeping a runaway
  answer out of memory. A forge that redirects, as GitHub does for a
  renamed organization, gets a few hops."
  {:max-bytes (* 8 1024 1024)
   :connect-timeout-ms 10000
   :timeout-ms 60000
   :max-redirects 5})

(defn- printable
  "The start of a forge's answer for an error message, on one line and
  without control characters."
  [text]
  (let [text (str/trim (str/replace (str text) #"[\p{Cc}\p{Cf}\s]+" " "))]
    (if (< 300 (count text)) (str (subs text 0 300) "…") text)))

(defn- failed! [url reason]
  (throw (ex-info (str "GET " url " failed: " reason) {:aro/problem :http :url url})))

(defn- parse-json
  "The JSON page `text`, keys as keywords."
  [url text]
  (try
    (json/parse-string text {:keys :keyword :max-bytes (:max-bytes limits)})
    (catch clojure.lang.ExceptionInfo e
      (if (json/refusal? e)
        (failed! url (str "the answer is no JSON aro reads: " (ex-message e)))
        (throw e)))))

(defn- http-get
  "The JSON body of `url`, requested in this process, so the headers and
  the token in them never reach a command line."
  [url headers]
  (let [{:keys [status body]} (try
                                (http/request :get url (assoc limits :headers headers :as :text))
                                (catch clojure.lang.ExceptionInfo e
                                  (if (http/refusal? e)
                                    (failed! url (ex-message e))
                                    (throw e))))]
    (when-not (<= 200 status 299)
      (failed! url (str "HTTP " status (when-not (str/blank? body) (str ": " (printable body))))))
    (parse-json url body)))

(defn curl-request
  "The curl call GETting `url` through a tunnel: {:args :in}. The headers
  go to curl's stdin so that a token in them never shows in the process
  list. curl's --connect-to sends the request to the tunnel's port while
  TLS checks the forge's own name, which briko.http cannot do."
  [curl-args url headers]
  {:args (concat ["curl" "--silent" "--show-error" "--location" "--fail-with-body"
                  "--max-redirs" (str (:max-redirects limits))
                  "--connect-timeout" (str (quot (:connect-timeout-ms limits) 1000))
                  "--max-time" (str (quot (:timeout-ms limits) 1000))]
                 curl-args
                 (when (seq headers) ["--header" "@-"])
                 [url])
   :in (str/join (map (fn [[k v]] (str k ": " v "\n")) headers))})

(defn- curl-get
  "The JSON body of `url`, requested by curl with `curl-args`."
  [curl-args url headers]
  (let [{:keys [args in]} (curl-request curl-args url headers)
        {:keys [exit out err out-truncated? timed-out?]} (proc/run (vec args) {:in in
                                                                    :timeout-ms (+ (:timeout-ms limits) 5000)
                                                                    :max-out-bytes (inc (:max-bytes limits))})]
    (cond
      timed-out? (failed! url (str "curl did not finish within " (:timeout-ms limits) " ms"))
      out-truncated? (failed! url (str "the answer is larger than " (:max-bytes limits) " bytes"))
      (not (zero? exit)) (failed! url (printable (str err " " out)))
      :else (parse-json url out))))

(defn json-getter
  "A function GETting a URL with headers and parsing the JSON body; a
  failed request throws. Without `curl-args` the request runs through
  briko.http; with them, such as the --connect-to of a tunnel, through
  curl."
  [curl-args]
  (if (seq curl-args)
    (partial curl-get curl-args)
    http-get))
