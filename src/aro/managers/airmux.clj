;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.managers.airmux
  "airmux sessions: a YAML file per session below the airmux directory."
  (:require [babashka.fs :as fs]
            [clj-yaml.core :as yaml]))

(defn render [session]
  (yaml/generate-string session :dumper-options {:flow-style :block :indent 4 :indicator-indent 2}))

(defn files
  "{path text} of `sessions`, {relative-path session}, below :airmux-dir."
  [{:keys [airmux-dir]} sessions]
  {:files (into {} (for [[path session] sessions] [(str (fs/path airmux-dir path)) (render session)]))})
