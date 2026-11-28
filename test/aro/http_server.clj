;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.http-server
  "A plain HTTP/1.1 server on a loopback port for tests. babashka carries
  no com.sun.net.httpserver, so it answers on sockets: each connection
  takes one request, the handler's answer and is closed."
  (:require [clojure.string :as str])
  (:import (java.io BufferedReader InputStreamReader OutputStream)
           (java.net InetAddress ServerSocket Socket SocketException)
           (java.nio.charset StandardCharsets)))

(defn- read-request [^BufferedReader in]
  (let [[method target] (str/split (or (.readLine in) "") #" ")]
    (loop [headers {}]
      (let [line (.readLine in)]
        (if (str/blank? line)
          {:method method :target target :headers headers}
          (let [[k v] (str/split line #":\s*" 2)]
            (recur (assoc headers (str/lower-case k) v))))))))

(defn- respond! [^OutputStream out {:keys [status headers body length]}]
  (let [body (.getBytes (str body) StandardCharsets/UTF_8)
        head (str "HTTP/1.1 " status " X\r\n"
                  "Connection: close\r\n"
                  "Content-Length: " (or length (alength body)) "\r\n"
                  (str/join (for [[k v] headers] (str k ": " v "\r\n")))
                  "\r\n")]
    (.write out (.getBytes head StandardCharsets/ISO_8859_1))
    (when-not length
      (.write out body))
    (.flush out)))

(defn start
  "Serves `handler`, a function from {:method :target :headers :port} to
  {:status :headers :body}, on a free port of 127.0.0.1. :length declares
  a Content-Length and sends no body. Returns {:port :requests :socket},
  where :requests is an atom of every request received."
  [handler]
  (let [server (ServerSocket. 0 50 (InetAddress/getByName "127.0.0.1"))
        port (.getLocalPort server)
        requests (atom [])]
    (future
      (loop []
        (when-let [^Socket socket (try (.accept server) (catch SocketException _ nil))]
          (future
            (with-open [socket socket]
              (try
                (let [request (assoc (read-request (BufferedReader. (InputStreamReader. (.getInputStream socket) StandardCharsets/ISO_8859_1)))
                                     :port port)]
                  (swap! requests conj request)
                  (respond! (.getOutputStream socket) (handler request)))
                (catch Exception _ nil))))
          (recur))))
    {:port port :requests requests :socket server}))

(defn stop [{:keys [^ServerSocket socket]}]
  (.close socket))

(defn with-server
  "Calls `f` with a server serving `handler`, stopping it afterwards."
  [handler f]
  (let [server (start handler)]
    (try (f server) (finally (stop server)))))

(defn query
  "The parameters of the query string of `target` as a map."
  [target]
  (into {} (for [pair (some-> (second (str/split target #"\?" 2)) (str/split #"&"))
                 :let [[k v] (str/split pair #"=" 2)]]
             [k v])))

(defn path-of [target]
  (first (str/split target #"\?" 2)))
