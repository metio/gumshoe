;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.cnpg-retention
  "A detective for clusters with no floor under replication slot retention.

   This one reports a configuration, not a symptom. Every other cnpg detective
   fires once the volume is filling or the primary has already stopped; by then
   the cheapest repair is gone. The exposure is readable from the Cluster spec
   alone, on every cluster at once, long before a slot goes inactive."
  (:require [gumshoe.cnpg.retention :as retention]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.kubectl :as kubectl]))

(def ^:private fix
  (format "set %s in the cluster's parameters - small enough to invalidate a slot before the volume fills, and large enough that a standby briefly behind survives"
          retention/parameter))

(defn- finding
  [cluster {:keys [state raw cap volume]}]
  (merge
   {:severity :warning
    :component (kubectl/namespace-name-of cluster)}
   (case state
     :unset {:summary (format "no %s is set, so slot retention is unbounded" retention/parameter)
             :hint (str "an inactive slot retains WAL until the volume is full, which stops "
                        "postgres rather than slowing it: " fix)}
     :unbounded {:summary (format "%s is -1, so slot retention is unbounded" retention/parameter)
                 :hint (str "-1 is the default and means no limit: " fix)}
     :oversized {:summary (format "%s is %s of a %s volume, which is no floor"
                                  retention/parameter
                                  (retention/describe-bytes cap)
                                  (retention/describe-bytes volume))
                 :hint (str "a cap this large invalidates no slot before the volume fills: " fix)}
     :unknown {:summary (format "%s is set to %s, which does not parse as a size"
                                retention/parameter raw)
               :hint "check it against postgres's own units (B, kB, MB, GB, TB)"})))

(defn detect-unbounded-retention
  [evidence]
  (for [cluster (kubectl/items-of (get evidence cnpg/cluster-type))
        :let [assessment (retention/assess cluster)]
        :when (retention/exposed? assessment)]
    (finding cluster assessment)))

(def detectives
  [{:name "cnpg-wal-retention"
    :description "Clusters where an abandoned replication slot can fill the volume"
    :requires [cnpg/cluster-type]
    :detect detect-unbounded-retention}])
