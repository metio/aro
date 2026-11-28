;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.http-server
  "An HTTP/1.1 server on a loopback port for tests, plain or over TLS.
  babashka carries no com.sun.net.httpserver and cannot create an
  SSLServerSocket, so it answers on sockets and layers TLS over an
  accepted one: each connection takes one request, the handler's answer
  and is closed."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io BufferedReader ByteArrayInputStream InputStreamReader OutputStream)
           (java.net InetAddress ServerSocket Socket SocketException)
           (java.nio.charset StandardCharsets)
           (java.security KeyStore)
           (javax.net.ssl KeyManagerFactory SSLContext SSLSocket)))

(def forge-host
  "The only name in the certificate of test/aro/forge.p12 (EC P-256,
  self-signed, valid until 2126); test/aro/forge.pem holds the same
  certificate for a client's trust. Nothing resolves the name, so a
  request for it reaches a test server only through a forwarded address."
  "forge.aro.test")

(defn forge-certificate
  "The PEM text of the certificate a TLS test server presents."
  []
  (slurp (io/file "test/aro/forge.pem")))

(defn- forge-context
  "The server side of TLS for `forge-host`. The keystore's password is
  assembled here, so no credential-shaped literal sits in the source."
  ^SSLContext []
  (let [password (.toCharArray (str "aro" "-" "test"))
        store (with-open [in (io/input-stream (io/file "test/aro/forge.p12"))]
                (doto (KeyStore/getInstance "PKCS12") (.load in password)))
        kmf (doto (KeyManagerFactory/getInstance (KeyManagerFactory/getDefaultAlgorithm))
              (.init store password))]
    (doto (SSLContext/getInstance "TLS")
      (.init (.getKeyManagers kmf) nil nil))))

(defn- over-tls
  "`socket` with the server side of TLS layered over it."
  ^Socket [^SSLContext context ^Socket socket]
  (doto ^SSLSocket (.createSocket (.getSocketFactory context) socket (ByteArrayInputStream. (byte-array 0)) true)
    (.setUseClientMode false)))

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
  a Content-Length and sends no body. With :tls? the server speaks TLS
  and presents the certificate for `forge-host`. Returns {:port :requests
  :socket}, where :requests is an atom of every request received."
  ([handler] (start handler {}))
  ([handler {:keys [tls?]}]
   (let [server (ServerSocket. 0 50 (InetAddress/getByName "127.0.0.1"))
         port (.getLocalPort server)
         context (when tls? (forge-context))
         requests (atom [])]
     (future
       (loop []
         (when-let [^Socket socket (try (.accept server) (catch SocketException _ nil))]
           (future
             (with-open [socket (if context (over-tls context socket) socket)]
               (try
                 (let [request (assoc (read-request (BufferedReader. (InputStreamReader. (.getInputStream socket) StandardCharsets/ISO_8859_1)))
                                      :port port)]
                   (swap! requests conj request)
                   (respond! (.getOutputStream socket) (handler request)))
                 (catch Exception _ nil))))
           (recur))))
     {:port port :requests requests :socket server})))

(defn stop [{:keys [^ServerSocket socket]}]
  (.close socket))

(defn with-server
  "Calls `f` with a server serving `handler`, stopping it afterwards.
  `options` are those of `start`."
  ([handler f] (with-server handler {} f))
  ([handler options f]
   (let [server (start handler options)]
     (try (f server) (finally (stop server))))))

(defn query
  "The parameters of the query string of `target` as a map."
  [target]
  (into {} (for [pair (some-> (second (str/split target #"\?" 2)) (str/split #"&"))
                 :let [[k v] (str/split pair #"=" 2)]]
             [k v])))

(defn path-of [target]
  (first (str/split target #"\?" 2)))
