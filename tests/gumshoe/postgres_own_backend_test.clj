;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.postgres-own-backend-test
  "The connection reading leaves out the session that takes it, so a server
   one slot below its ceiling is not read as at it."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.postgres :as postgres]))

(deftest the-statement-excludes-its-own-backend-test
  (is (str/includes? postgres/connection-sql "FROM pg_stat_activity WHERE pid <> pg_backend_pid() GROUP BY")))

(deftest one-slot-below-the-ceiling-is-below-it-test
  ;; 96 ordinary clients on max_connections 100 with 3 reserved: what the
  ;; statement returns once our own backend is left out
  (let [reading {:max-connections 100 :superuser-reserved-connections 3 :reserved-connections 0
                 :rows [{:backend-type "client backend" :backends 96}
                        {:backend-type "checkpointer" :backends 1}]}]
    (is (= 97 (postgres/connection-ceiling reading)))
    (is (= 96 (postgres/client-backends reading)))
    (is (< (postgres/client-backends reading) (postgres/connection-ceiling reading)))))
