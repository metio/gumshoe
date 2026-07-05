;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.announce-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]))

(deftest changelog-message-test
  (let [message (announce/changelog-message "kube.example.org" "Drain node worker-1" "@seb:example.org")]
    (is (= "kube.example.org: Drain node worker-1 by @seb:example.org" (:plain message)))
    (is (= "<code>kube.example.org</code>: Drain node worker-1 by @seb:example.org" (:html message)))))

(deftest webhook-payload-test
  (testing "Slack/Mattermost take {text}, Discord takes {content}"
    (is (= {:text "kube.example.org: drain by @seb"} (announce/webhook-payload :slack "kube.example.org" "drain" "@seb")))
    (is (= {:content "kube.example.org: drain by @seb"} (announce/webhook-payload :discord "kube.example.org" "drain" "@seb"))))
  (testing "an unknown/absent format defaults to the Slack shape"
    (is (= {:text "x: y by z"} (announce/webhook-payload nil "x" "y" "z")))))

(deftest unknown-announcer-type-test
  (testing "an unknown announcer type is warned about, never a crash"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (is (nil? (announce/announce-via {:type :carrier-pigeon} "sys" {:actor "@a"} "msg"))))
      (is (clojure.string/includes? (str err) "unknown announcer type")))))

(deftest no-announcers-configured-test
  (testing "with nothing configured, a change is not announced - a warning, not a failure"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (announce/announce! "kube.example.org" {:cluster "kube.nowhere"} "some change"))
      (is (clojure.string/includes? (str err) "no announcers configured")))))
