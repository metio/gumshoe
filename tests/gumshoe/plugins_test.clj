;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.plugins-test
  (:require [clojure.string]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.plugins :as plugins]))

(deftest load!-test
  (testing "an empty plugin list does nothing and never throws"
    (is (nil? (plugins/load! []))))
  (testing "a plugin that can not be loaded is warned about, not thrown - a broken third-party plugin never stops the core"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (is (nil? (plugins/load! ['totally.bogus.plugin.that.does.not.exist]))))
      (is (clojure.string/includes? (str err) "could not load plugin"))))
  (testing "a plugin whose registration trips an assert throws an Error, which must also be contained"
    (let [err (java.io.StringWriter.)]
      (with-redefs [require (fn [& _] (assert false "a :pre condition in a top-level register! call"))]
        (binding [*err* err]
          (is (nil? (plugins/load! ['acme.plugin.with.a.bad.registration])))))
      (is (clojure.string/includes? (str err) "could not load plugin")))))

(deftest example-plugin-extends-every-seam-test
  (testing "loading a real plugin registers into several seams at once"
    (plugins/load! ['example.plugin])
    (is (some? (get-method announce/announce-via :example))
        "a new announcer type is registered")
    (is (some #(= "example-check" (:name %)) (registry/for-scope :workloads))
        "a detective joined the workloads scope, so a workloads scan now includes it")))
