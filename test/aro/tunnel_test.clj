;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.tunnel-test
  (:require [aro.tunnel :as tunnel]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import (java.io BufferedReader InputStreamReader)
           (java.net InetSocketAddress ServerSocket Socket)))

(deftest the-forward-binds-loopback-only
  (is (= ["ssh" "-N" "-o" "ExitOnForwardFailure=yes" "-o" "BatchMode=yes" "-p" "2222"
          "-L" "127.0.0.1:4711:localhost:443" "admin@git.example.org"]
         (tunnel/ssh-command {:ssh "admin@git.example.org" :target "localhost:443" :ssh-options ["-p" "2222"]} 4711))))

(deftest requests-reach-the-local-port
  (is (= {:connect-to {:host "127.0.0.1" :port 4711}} (tunnel/connect-to 4711))))

(deftest no-tunnel-adds-no-options
  (is (= {} (tunnel/with-tunnel nil identity))))

(deftest an-ssh-that-exits-fails-the-tunnel
  (is (thrown-with-msg? Exception #"ended before forwarding"
                        (with-redefs [tunnel/ssh-command (fn [_ _] ["sh" "-c" "echo refused >&2; exit 255"])]
                          (tunnel/open {:ssh "nowhere" :target "localhost:1"})))))

(deftest a-port-another-process-holds-fails-the-tunnel
  (with-open [other (ServerSocket. 0)]
    (is (thrown-with-msg? Exception #"ended before forwarding"
                          (with-redefs [tunnel/free-port (constantly (.getLocalPort other))
                                        tunnel/ssh-command (fn [_ _] ["sh" "-c" "sleep 0.5; echo 'bind: Address already in use' >&2; exit 255"])]
                            (tunnel/open {:ssh "nowhere" :target "localhost:1"}))))))

;; A stand-in ssh: a babashka script that takes the argv aro built and, in
;; its "listen" mode, accepts connections on the forwarded loopback port.

(def ^:private babashka
  (str (fs/which "bb")))

(def ^:private stand-in
  (str (io/file (io/resource "aro/stand_in_ssh.clj"))))

(def ^:private ssh-command tunnel/ssh-command)

(def ^:private source-tunnel {:ssh "admin@git.aro.test" :target "forge.aro.test:443"})

(defn- eventually
  "Whether `check` holds within 5 s."
  [check]
  (let [deadline (+ (System/currentTimeMillis) 5000)]
    (loop []
      (cond (check) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 20) (recur))))))

(defn- alive?
  "Whether the process `pid` runs: it exists and is no zombie. Linux only."
  [pid]
  (when-not (int? pid)
    (throw (ex-info "the stand-in wrote no pid" {:pid pid})))
  (let [stat (str "/proc/" pid "/stat")]
    (boolean (try (not (re-find #"^\d+ \(.*\) Z " (first (fs/read-all-lines stat))))
                  (catch java.io.IOException _ false)))))

(defn- accepts? [port]
  (try
    (with-open [socket (Socket.)]
      (.connect socket (InetSocketAddress. "127.0.0.1" (int port)) 200)
      true)
    (catch Exception _ false)))

(defn- greeting [port]
  (with-open [socket (Socket. "127.0.0.1" (int port))]
    (.readLine (BufferedReader. (InputStreamReader. (.getInputStream socket))))))

(defn- with-stand-in
  "Calls `f` with a function returning the stand-in's pid, while aro starts
  the stand-in in `mode` instead of ssh."
  [mode f]
  (let [dir (fs/create-temp-dir {:prefix "aro-tunnel-test"})
        pid-file (str (fs/path dir "pid"))]
    (try
      (with-redefs [tunnel/ssh-command (fn [t port] (concat [babashka stand-in mode pid-file] (ssh-command t port)))]
        (f (fn []
             (when (eventually #(fs/exists? pid-file))
               (eventually #(parse-long (slurp pid-file)))
               (parse-long (slurp pid-file))))))
      (finally (fs/delete-tree dir)))))

(deftest a-forward-reaches-its-port-for-the-call-and-ends-with-it
  (with-stand-in "listen"
    (fn [pid]
      (let [port (atom nil)
            answer (tunnel/with-tunnel source-tunnel
                     (fn [{{p :port} :connect-to :as options}]
                       (reset! port p)
                       (is (= {:connect-to {:host "127.0.0.1" :port p}} options))
                       (is (alive? (pid)) "control: the forward runs during the call")
                       (greeting p)))]
        (is (= "forwarded" answer))
        (is (eventually #(not (alive? (pid)))))
        (is (eventually #(not (accepts? @port))))))))

(deftest a-call-that-throws-still-ends-the-forward
  (with-stand-in "listen"
    (fn [pid]
      (is (thrown-with-msg? Exception #"listing failed"
                            (tunnel/with-tunnel source-tunnel
                              (fn [_] (throw (ex-info "listing failed" {}))))))
      (is (some? (pid)))
      (is (eventually #(not (alive? (pid))))))))

(deftest open-waits-until-the-port-accepts-and-close-stops-the-forward
  (with-stand-in "listen"
    (fn [pid]
      (let [started (System/currentTimeMillis)
            open-tunnel (tunnel/open source-tunnel :settle-ms 300)]
        (is (>= (- (System/currentTimeMillis) started) 300) "the forward must keep running for settle-ms")
        (is (accepts? (:port open-tunnel)))
        (is (alive? (pid)))
        (tunnel/close open-tunnel)
        (is (eventually #(not (alive? (pid)))))
        (is (eventually #(not (accepts? (:port open-tunnel)))))))))

(deftest an-ssh-that-exits-names-its-error
  (with-stand-in "exit"
    (fn [_]
      (let [e (try (tunnel/open source-tunnel) nil (catch Exception e e))]
        (is (some? e))
        (is (re-find #"^ssh admin@git\.aro\.test ended before forwarding forge\.aro\.test:443: " (ex-message e)))
        (is (re-find #"Permission denied \(publickey\)\." (ex-message e)))
        (is (= {:aro/problem :tunnel} (select-keys (ex-data e) [:aro/problem])))))))

(deftest an-ssh-that-never-forwards-fails-at-the-deadline-and-is-stopped
  (with-stand-in "silent"
    (fn [pid]
      (let [started (System/currentTimeMillis)
            e (try (tunnel/open source-tunnel :timeout-ms 1000 :settle-ms 100) nil (catch Exception e e))]
        (is (some? e))
        (is (= "ssh admin@git.aro.test did not forward forge.aro.test:443 within 1s" (ex-message e)))
        (is (= {:aro/problem :tunnel} (select-keys (ex-data e) [:aro/problem])))
        (is (>= (- (System/currentTimeMillis) started) 1000))
        (is (some? (pid)))
        (is (eventually #(not (alive? (pid)))))))))

(deftest a-missing-ssh-fails-the-tunnel
  (with-redefs [tunnel/ssh-command (fn [_ _] ["/nonexistent/aro-test/ssh" "-N"])]
    (let [e (try (tunnel/open source-tunnel :timeout-ms 1000) nil (catch Exception e e))]
      (testing "the failure names the program"
        (is (some? e))
        (is (re-find #"/nonexistent/aro-test/ssh" (str (ex-message e))))))))
