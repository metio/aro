;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.managers
  "The session managers aro generates sessions for. Each one renders the
  same sessions, {relative-path session}, into {:files {path text}
  :warnings [...]}."
  (:require [aro.managers.airmux :as airmux]
            [aro.managers.sesh :as sesh]
            [clojure.string :as str]))

(def managers
  {:airmux airmux/files
   :sesh sesh/files})

(defn files
  "The files and warnings of every manager :session-managers names."
  [config sessions]
  (let [chosen (:session-managers config)
        unknown (remove managers chosen)]
    (when (seq unknown)
      (throw (ex-info (str ":session-managers names " (str/join ", " (map pr-str unknown))
                           ", which aro does not know; it knows " (str/join ", " (map pr-str (sort (keys managers)))))
                      {:aro/problem :invalid-config})))
    (reduce (fn [acc id]
              (let [{:keys [files warnings]} ((managers id) config sessions)]
                (-> acc (update :files merge files) (update :warnings into warnings))))
            {:files {} :warnings []}
            (distinct chosen))))
