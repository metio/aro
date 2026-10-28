;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.forge
  "Listing the repositories of a source from its forge: a GitHub or Forgejo
  organization or user, or a GitLab group with its subgroups. Each repository
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
  until a page holds fewer than `page-size` items. `get-json` returns the
  parsed body of a URL."
  [get-json page-size url-of]
  (loop [page 1 items []]
    (let [batch (get-json (url-of page page-size))]
      (if (< (count batch) page-size)
        (into items batch)
        (recur (inc page) (into items batch))))))

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

(defn list-repositories
  "The repository records of `source`. `get-json` takes a URL and the request
  headers and returns the parsed JSON body. A source's :host replaces the host
  of its URL in the records, for a forge reached through another address.
  A GitHub or Forgejo user with a token lists the repositories the token's
  account owns, private ones included; without one, only the public ones.
  Disabled repositories cannot be cloned and are left out."
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
                     (pages #(get-json % headers) 50 #(str base "limit=" %2 "&page=" %1)))
        (and token (not (:org forgejo))) (owned-by (:user forgejo))))

    gitlab
    (let [headers (cond-> {} token (assoc "PRIVATE-TOKEN" token))
          url (base-url (:url gitlab))
          base (str url "/api/v4/groups/" (encode (:group gitlab)) "/projects?include_subgroups=true&order_by=path&sort=asc")]
      (mapv #(gitlab-record (or host (host-of url)) %)
            (pages #(get-json % headers) 100 #(str base "&per_page=" %2 "&page=" %1))))

    :else
    (throw (ex-info (str "the source " (:id source) " names none of :github, :gitlab or :forgejo")
                    {:aro/problem :invalid-source :source (:id source)}))))

(defn json-getter
  "A function GETting a URL with headers through curl and parsing the JSON
  body; a failed request throws. `curl-args` go before the URL, such as the
  --connect-to of a tunnel."
  [curl-args]
  (fn [url headers]
    (let [{:keys [exit out err]} (apply process/shell {:out :string :err :string :continue true}
                                        (concat ["curl" "--silent" "--show-error" "--location" "--fail-with-body"]
                                                curl-args
                                                (mapcat (fn [[k v]] ["--header" (str k ": " v)]) headers)
                                                [url]))]
      (when-not (zero? exit)
        (throw (ex-info (str "GET " url " failed: " (str/trim (str err " " out)))
                        {:aro/problem :http :url url})))
      (json/parse-string out true))))
