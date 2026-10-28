;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.inventory
  "The inventory: the repositories each source listed when it was last
  fetched, one source per key. Every other command reads it instead of a
  forge, so a forge that needs a tunnel or a token is only contacted by
  `aro fetch`, and a machine sharing the file never contacts it at all.

  Comment lines at the top of an existing file, such as a license header,
  survive a rewrite."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def record-keys [:host :path :name :namespace :default-branch :archived :fork :topics])

(defn- split-header [text]
  (let [lines (str/split-lines text)
        header (take-while #(or (str/blank? %) (str/starts-with? (str/triml %) ";")) lines)]
    [(str/join "\n" header) (str/join "\n" (drop (count header) lines))]))

(defn parse
  "{:header text :sources {source-id [record]}} from the text of an inventory."
  [text]
  (let [[header body] (split-header text)]
    {:header header
     :sources (or (when-not (str/blank? body) (edn/read-string body)) {})}))

(defn read-inventory [path]
  (if (fs/exists? path)
    (parse (slurp (str path)))
    {:header "" :sources {}}))

(defn- render-record [record]
  (str "{" (str/join " " (for [k record-keys :when (contains? record k)]
                           (str k " " (pr-str (get record k)))))
       "}"))

(defn render
  "The text of an inventory: sources sorted by id, records by path, one per
  line, so a refetch changes only the lines of what changed upstream."
  [{:keys [header sources]}]
  (let [body (for [[id records] (sort-by key sources)]
               (str (pr-str id) "\n ["
                    (str/join "\n  " (map render-record (sort-by :path records)))
                    "]"))]
    (str (when-not (str/blank? header) (str (str/trimr header) "\n\n"))
         "{" (str/join "\n " body) "}\n")))

(defn write-inventory!
  "Writes `inventory` to `path` and returns whether the content changed."
  [path inventory]
  (let [text (render inventory)
        before (when (fs/exists? path) (slurp (str path)))]
    (when (not= text before)
      (fs/create-dirs (fs/parent (fs/absolutize path)))
      (spit (str path) text))
    (not= text before)))
