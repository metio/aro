;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.tunnel
  "An SSH port forward to a forge API that is only reachable from its own
  host. Requests keep the forge's URL and reach the forwarded port through
  curl's --connect-to, so TLS still checks the forge's certificate."
  (:require [babashka.process :as process])
  (:import (java.net InetSocketAddress ServerSocket Socket URI)))

(defn- free-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn- listening? [port]
  (try
    (with-open [socket (Socket.)]
      (.connect socket (InetSocketAddress. "127.0.0.1" (int port)) 200)
      true)
    (catch Exception _ false)))

(defn ssh-command
  "The ssh call forwarding local `port` to the tunnel's :target, seen from
  its :ssh destination."
  [{:keys [ssh target ssh-options]} port]
  (concat ["ssh" "-N" "-o" "ExitOnForwardFailure=yes" "-o" "BatchMode=yes"]
          ssh-options
          ["-L" (str "127.0.0.1:" port ":" target) ssh]))

(defn connect-to
  "The curl arguments sending requests for `url` to local `port`."
  [url port]
  (let [uri (URI. url)
        url-port (if (neg? (.getPort uri)) (if (= "http" (.getScheme uri)) 80 443) (.getPort uri))]
    ["--connect-to" (str (.getHost uri) ":" url-port ":127.0.0.1:" port)]))

(defn open
  "Starts the forward of `tunnel` and waits until its port accepts
  connections while ssh still runs `settle-ms` later. Another process may
  bind the free port before ssh does; ssh then exits on the failed forward
  right after authenticating, and the port must not count as the tunnel.
  Returns {:process :port}."
  [tunnel & {:keys [timeout-ms settle-ms] :or {timeout-ms 15000 settle-ms 1000}}]
  (let [port (free-port)
        proc (apply process/process {:out :string :err :string} (ssh-command tunnel port))
        deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop [ready-since nil]
      (let [now (System/currentTimeMillis)]
        (cond
          (not (.isAlive ^Process (:proc proc)))
          (throw (ex-info (str "ssh " (:ssh tunnel) " ended before forwarding " (:target tunnel) ": " @(:err proc))
                          {:aro/problem :tunnel}))

          (and ready-since (>= (- now ready-since) settle-ms))
          {:process proc :port port}

          (> now deadline)
          (do (process/destroy-tree proc)
              (throw (ex-info (str "ssh " (:ssh tunnel) " did not forward " (:target tunnel) " within " (quot timeout-ms 1000) "s")
                              {:aro/problem :tunnel})))

          :else
          (do (Thread/sleep 100)
              (recur (or ready-since (when (listening? port) now)))))))))

(defn close [{:keys [process]}]
  (process/destroy-tree process))

(defn with-tunnel
  "Calls `f` with the curl arguments reaching `url`: none without a tunnel,
  else the --connect-to of a forward opened for the call."
  [tunnel url f]
  (if tunnel
    (let [open-tunnel (open tunnel)]
      (try
        (f (connect-to url (:port open-tunnel)))
        (finally (close open-tunnel))))
    (f [])))
