;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.calico
  "Detectives for calico as managed by the tigera-operator: components that
   are unavailable, degraded, or stuck progressing."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.kubectl :as kubectl]))

(def tigerastatus-type "tigerastatuses.operator.tigera.io")

(defn detect-calico-problems
  [evidence]
  (let [statuses (kubectl/items-of (get evidence tigerastatus-type))]
    (concat
     (for [status statuses
           :let [available (conditions/condition status "Available")]
           :when (= "False" (:status available))]
       {:severity :critical
        :component (kubectl/name-of status)
        :summary "tigera component is not Available"
        :hint (:message available)})
     (for [status statuses
           :let [degraded (conditions/condition status "Degraded")]
           :when (= "True" (:status degraded))]
       {:severity :critical
        :component (kubectl/name-of status)
        :summary (format "tigera component is Degraded (%s)" (or (:reason degraded) "unknown"))
        :hint (:message degraded)})
     (for [status statuses
           :let [progressing (conditions/condition status "Progressing")]
           :when (= "True" (:status progressing))]
       {:severity :info
        :component (kubectl/name-of status)
        :summary "tigera component is Progressing"
        :hint (:message progressing)}))))

(def detectives
  [{:name "calico"
    :description "tigera-operator components that are unavailable, degraded, or progressing"
    :requires [tigerastatus-type]
    :detect detect-calico-problems}])
