;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.tunnel
  "An SSH port forward to a forge API that is only reachable from its own
  host. Requests keep the forge's URL and reach the forwarded port through
  briko.http's :connect-to, so TLS still checks the forge's certificate."
  (:require [briko.process :as process])
  (:import (java.net ServerSocket)))

(defn- free-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn ssh-command
  "The ssh call forwarding local `port` to the tunnel's :target, seen from
  its :ssh destination."
  [{:keys [ssh target ssh-options]} port]
  (concat ["ssh" "-N" "-o" "ExitOnForwardFailure=yes" "-o" "BatchMode=yes"]
          ssh-options
          ["-L" (str "127.0.0.1:" port ":" target) ssh]))

(defn connect-to
  "The briko.http options sending https requests to local `port`."
  [port]
  {:connect-to {:host "127.0.0.1" :port port}})

(defn open
  "Starts the forward of `tunnel` and waits until its port accepts
  connections while ssh still runs `settle-ms` later. Another process may
  bind the free port before ssh does; ssh then exits on the failed forward
  right after authenticating, and the port must not count as the tunnel.
  ssh is stopped when the forward fails, on `close`, and when the JVM
  shuts down. Returns {:process :port}."
  [tunnel & {:keys [timeout-ms settle-ms] :or {timeout-ms 15000 settle-ms 1000}}]
  (let [port (free-port)
        handle (process/start (vec (ssh-command tunnel port)))]
    (try
      (process/wait-ready handle {:port port :settle-ms settle-ms :timeout-ms timeout-ms})
      {:process handle :port port}
      (catch clojure.lang.ExceptionInfo e
        (case (:type (ex-data e))
          ::process/exited-before-ready
          (throw (ex-info (str "ssh " (:ssh tunnel) " ended before forwarding " (:target tunnel) ": "
                               (:err (process/output handle)))
                          {:aro/problem :tunnel}))

          ::process/ready-timeout
          (throw (ex-info (str "ssh " (:ssh tunnel) " did not forward " (:target tunnel) " within " (quot timeout-ms 1000) "s")
                          {:aro/problem :tunnel}))

          (do (process/close handle)
              (throw e)))))))

(defn close [{:keys [process]}]
  (process/close process))

(defn with-tunnel
  "Calls `f` with the briko.http options reaching the forge: none without
  a tunnel, else the :connect-to of a forward opened for the call."
  [tunnel f]
  (if tunnel
    (let [open-tunnel (open tunnel)]
      (try
        (f (connect-to (:port open-tunnel)))
        (finally (close open-tunnel))))
    (f {})))
