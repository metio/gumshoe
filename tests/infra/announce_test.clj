;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.announce-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.announce :as announce]))

(deftest changelog-message-test
  (let [message (announce/changelog-message "kube.infra.run" "Drain node worker-1" "@seb:infra.run")]
    (is (= "kube.infra.run: Drain node worker-1 by @seb:infra.run" (:plain message)))
    (is (= "<code>kube.infra.run</code>: Drain node worker-1 by @seb:infra.run" (:html message)))))

(deftest webhook-payload-test
  (testing "Slack/Mattermost take {text}, Discord takes {content}"
    (is (= {:text "kube.infra.run: drain by @seb"} (announce/webhook-payload :slack "kube.infra.run" "drain" "@seb")))
    (is (= {:content "kube.infra.run: drain by @seb"} (announce/webhook-payload :discord "kube.infra.run" "drain" "@seb"))))
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
        (announce/announce! "kube.infra.run" {:cluster "kube.nowhere"} "some change"))
      (is (clojure.string/includes? (str err) "no announcers configured")))))
