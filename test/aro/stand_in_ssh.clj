;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

;; A stand-in for `ssh -N -L 127.0.0.1:<port>:<target> <destination>`, run as
;;
;;   bb stand_in_ssh.clj <mode> <pid-file> <the ssh argv aro built>
;;
;; It writes its pid to <pid-file> and reads the forwarded port from the -L
;; argument, then acts as <mode> says:
;;
;; - listen: accepts connections on 127.0.0.1:<port> until it is killed,
;;   answering each with "forwarded\n"
;; - exit: writes a refusal to stderr and exits with 255, as ssh does when
;;   authentication fails
;; - silent: runs until it is killed and never listens
;; - noisy-exit: writes 256 KiB to stderr, then exits with 255
;;
;; It never opens a connection itself and binds loopback only.

(ns aro.stand-in-ssh
  (:require [babashka.fs :as fs])
  (:import (java.net InetAddress ServerSocket)))

(let [[mode pid-file & ssh-argv] *command-line-args*
      forward (second (drop-while #(not= "-L" %) ssh-argv))
      port (parse-long (second (.split ^String forward ":")))]
  (spit pid-file (str (fs/read-link "/proc/self")))
  (case mode
    "listen" (with-open [server (ServerSocket. (int port) 50 (InetAddress/getByName "127.0.0.1"))]
               (while true
                 (with-open [socket (.accept server)]
                   (doto (.getOutputStream socket)
                     (.write (.getBytes "forwarded\n"))
                     (.flush)))))
    "exit" (binding [*out* *err*]
             (println "Permission denied (publickey).")
             (System/exit 255))
    "silent" (while true (Thread/sleep 1000))
    "noisy-exit" (let [line (str (apply str (repeat 1023 "x")) "\n")]
                   (binding [*out* *err*]
                     (dotimes [_ 256] (print line))
                     (println "Permission denied (publickey).")
                     (flush))
                   (System/exit 255))))
