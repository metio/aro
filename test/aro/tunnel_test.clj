;; SPDX-FileCopyrightText: The aro Authors
;; SPDX-License-Identifier: 0BSD

(ns aro.tunnel-test
  (:require [aro.tunnel :as tunnel]
            [clojure.test :refer [deftest is]]))

(deftest the-forward-binds-loopback-only
  (is (= ["ssh" "-N" "-o" "ExitOnForwardFailure=yes" "-o" "BatchMode=yes" "-p" "2222"
          "-L" "127.0.0.1:4711:localhost:443" "admin@git.example.org"]
         (tunnel/ssh-command {:ssh "admin@git.example.org" :target "localhost:443" :ssh-options ["-p" "2222"]} 4711))))

(deftest requests-keep-their-host-and-reach-the-local-port
  (is (= ["--connect-to" "git.example.org:443:127.0.0.1:4711"] (tunnel/connect-to "https://git.example.org" 4711)))
  (is (= ["--connect-to" "git.example.org:80:127.0.0.1:4711"] (tunnel/connect-to "http://git.example.org/" 4711)))
  (is (= ["--connect-to" "git.example.org:8443:127.0.0.1:4711"] (tunnel/connect-to "https://git.example.org:8443" 4711))))

(deftest no-tunnel-adds-no-arguments
  (is (= [] (tunnel/with-tunnel nil "https://x" identity))))

(deftest an-ssh-that-exits-fails-the-tunnel
  (is (thrown-with-msg? Exception #"ended before forwarding"
                        (with-redefs [tunnel/ssh-command (fn [_ _] ["sh" "-c" "echo refused >&2; exit 255"])]
                          (tunnel/open {:ssh "nowhere" :target "localhost:1"})))))
