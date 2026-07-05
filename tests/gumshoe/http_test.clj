;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.http-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.http :as http]))

(deftest http-ok-test
  (is (http/ok? {:reachable true :status 200}))
  (is (http/ok? {:reachable true :status 204}))
  (is (not (http/ok? {:reachable true :status 503})))
  (is (not (http/ok? {:reachable false :error "connection refused"}))))
