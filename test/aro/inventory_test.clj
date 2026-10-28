;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.inventory-test
  (:require [aro.inventory :as inventory]
            [aro.support :as s :refer [path]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]))

(use-fixtures :each s/with-root)

(def ^:private record
  {:host "github.com" :path "metio/aro" :name "aro" :namespace "metio" :default-branch "main" :archived false :fork false :topics []})

(deftest an-inventory-round-trips-one-record-per-line
  (let [inv {:header "" :sources {"metio" [(assoc record :path "metio/b" :name "b") record] "me" [record]}}
        text (inventory/render inv)]
    (is (= 5 (count (str/split-lines text))))
    (is (str/starts-with? (second (str/split-lines text)) " [{:host \"github.com\" :path \"metio/aro\""))
    (is (= (update-vals (:sources inv) #(sort-by :path %)) (:sources (inventory/parse text))))))

(deftest a-rewrite-keeps-the-header-and-reports-changes
  (let [file (path "inventory.edn")]
    (spit file ";; a license header\n\n{}\n")
    (is (true? (inventory/write-inventory! file (assoc (inventory/read-inventory file) :sources {"metio" [record]}))))
    (is (str/starts-with? (slurp file) ";; a license header\n\n{\"metio\""))
    (is (false? (inventory/write-inventory! file (inventory/read-inventory file))))))

(deftest a-missing-inventory-is-empty
  (is (= {:header "" :sources {}} (inventory/read-inventory (path "none.edn")))))
