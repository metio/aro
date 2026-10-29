;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.forge
  "Listing the repositories of a source from its forge: a GitHub or Forgejo
  organization or user, a GitLab group with its subgroups, a GitLab user, or
  every GitLab project the token's account is a member of. Each repository
  becomes a
  record of what the rest of aro needs: host, path, name, namespace, default
  branch, whether it is archived or a fork, and its topics."
  (:require [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import (java.net URI URLEncoder)
           (java.nio.charset StandardCharsets)))


(defn- encode [s]
  (URLEncoder/encode (str s) StandardCharsets/UTF_8))

(defn host-of [url]
  (.getHost (URI. url)))

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

(defn- github-record [host {:keys [name full_name default_branch archived fork topics]}]
  {:host host
   :path full_name
   :name name
   :namespace (first (str/split full_name #"/"))
   :default-branch default_branch
   :archived (boolean archived)
   :fork (boolean fork)
   :topics (vec (sort topics))})

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
  [{:keys [github gitlab forgejo host] :as source} get-json token]
  (cond
    github
    (let [headers (cond-> {"Accept" "application/vnd.github+json"}
                    token (assoc "Authorization" (str "Bearer " token)))
          url (base-url (:url github "https://api.github.com"))
          base (cond
                 (:org github) (str url "/orgs/" (encode (:org github)) "/repos?type=all")
                 token (str url "/user/repos?affiliation=owner")
                 :else (str url "/users/" (encode (:user github)) "/repos?type=owner"))]
      (cond->> (mapv #(github-record (or host "github.com") %)
                     (remove :disabled (pages #(get-json % headers) 100 #(str base "&per_page=" %2 "&page=" %1))))
        (and token (not (:org github))) (owned-by (:user github))))

    forgejo
    (let [headers (cond-> {} token (assoc "Authorization" (str "token " token)))
          url (base-url (:url forgejo))
          base (cond
                 (:org forgejo) (str url "/api/v1/orgs/" (encode (:org forgejo)) "/repos?")
                 token (str url "/api/v1/user/repos?")
                 :else (str url "/api/v1/users/" (encode (:user forgejo)) "/repos?"))]
      (cond->> (mapv #(github-record (or host (host-of url)) %)
                     (remove :empty (pages #(get-json % headers) 50 #(str base "limit=" %2 "&page=" %1))))
        (and token (not (:org forgejo))) (owned-by (:user forgejo))))

    gitlab
    (let [headers (cond-> {} token (assoc "Authorization" (str "Bearer " token)))
          url (base-url (:url gitlab))
          base (cond
                 (:group gitlab) (str url "/api/v4/groups/" (encode (:group gitlab)) "/projects?include_subgroups=true&order_by=path&sort=asc")
                 (:membership gitlab) (if token
                                        (str url "/api/v4/projects?membership=true&order_by=path&sort=asc")
                                        (throw (ex-info (str "the source " (:id source) " lists the projects of its token's account and needs a :token-command")
                                                        {:aro/problem :invalid-source :source (:id source)})))
                 token (str url "/api/v4/projects?owned=true&order_by=path&sort=asc")
                 :else (str url "/api/v4/users/" (encode (:user gitlab)) "/projects?order_by=path&sort=asc"))]
      (cond->> (mapv #(gitlab-record (or host (host-of url)) %)
                     (remove #(or (:empty_repo %) (= "disabled" (:repository_access_level %)))
                             (pages #(get-json % headers) 100 #(str base "&per_page=" %2 "&page=" %1))))
        (and token (:user gitlab)) (owned-by (:user gitlab))))

    :else
    (throw (ex-info (str "the source " (:id source) " names none of :github, :gitlab or :forgejo")
                    {:aro/problem :invalid-source :source (:id source)}))))

(defn curl-request
  "The curl call GETting `url`: {:args :in}. The headers go to curl's stdin
  so that a token in them never shows in the process list."
  [curl-args url headers]
  {:args (concat ["curl" "--silent" "--show-error" "--location" "--fail-with-body"]
                 curl-args
                 (when (seq headers) ["--header" "@-"])
                 [url])
   :in (str/join (map (fn [[k v]] (str k ": " v "\n")) headers))})

(defn json-getter
  "A function GETting a URL with headers through curl and parsing the JSON
  body; a failed request throws. `curl-args` go before the URL, such as the
  --connect-to of a tunnel."
  [curl-args]
  (fn [url headers]
    (let [{:keys [args in]} (curl-request curl-args url headers)
          {:keys [exit out err]} (apply process/shell {:out :string :err :string :continue true :in in} args)]
      (when-not (zero? exit)
        (throw (ex-info (str "GET " url " failed: " (str/trim (str err " " out)))
                        {:aro/problem :http :url url})))
      (json/parse-string out true))))
