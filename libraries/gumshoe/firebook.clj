;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.firebook
  "Fire drills: a firebook lights a controlled fault, proves that the detective
   meant to find it reports it, and puts it out again on --extinguish, proving
   that the detective no longer reports it.

   A firebook is a spec handed to `book`. A kubernetes drill names its
   manifests, and everything else follows from them: it burns inside its own
   namespace, so a stray drill can never touch production workloads, and
   extinguishing is a single namespace delete.

     {:description \"Firebook: starts a crash-looping deployment ...\"
      :drill       \"crash-loop\"                  ; announced, and the item it confirms
      :fire        \"a crash-looping deployment\"  ; \"start a fire drill: <fire>\"
      :manifests   [(deployment-manifest {...})]   ; applied inside the drill namespace
      :prerequisites {:kubectl-can-create [\"deployments\"]}  ; joined with the drill's own
      :expect {:detectives pods/detectives
               :fire? (fn [finding ctx] -> bool)    ; a finding of the drill namespace
               :timeout 180}}                       ; seconds the burning check waits

   Any other drill declares its parts itself:

     {:description .. :options .. :prerequisites .. :drill .. :fire ..
      :derive (fn [opts] -> map)   ; merged into ctx; names the :target the
                                   ; confirmation shows and the :system the
                                   ; announcement names
      :ignite {:items  (fn [ctx] -> seq)        ; what the confirmation lists
               :effect (fn [ctx] -> effect plan)
               :recovery \"...\" or (fn [ctx] -> string)}  ; optional
      :extinguish {:put-out \"...\"             ; \"put out the fire: <put-out>\"
                   :items .. :effect .. :recovery ..
                   :checks (fn [ctx] -> post-checks)}  ; optional, before the out check
      :expect {:detectives [..]
               :collect (fn [ctx] -> evidence)  ; throws `unread` when it read nothing
               :fire? (fn [finding ctx] -> bool)
               :timeout 30 :interval 5 :out-timeout 60}
      :lit (fn [ctx])}            ; prints what to do once the fire burns

   Both changes run through gumshoe.flow as effect plans, so --dry-run shows
   them and changes nothing. After the ignite the burning check runs the
   expected detectives over freshly read evidence until one reports the fire;
   after the extinguish the out check runs them until none does. Evidence a
   detective read nothing from counts as not yet verified, so neither check
   holds on a read that did not answer. A change that fails records its
   :recovery, which the harness prints under the FAILED banner: the fire may
   still burn."
  (:refer-clojure :exclude [run!])
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.announce :as announce]
            [gumshoe.catalog :as catalog]
            [gumshoe.detective :as detective]
            [gumshoe.effect :as effect]
            [gumshoe.evpn :as evpn]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

(def drill-namespace "fire-drill")

(defn namespace-manifest
  []
  {:apiVersion "v1"
   :kind "Namespace"
   :metadata {:name drill-namespace
              :labels {:app.kubernetes.io/managed-by "gumshoe"}}})

(defn deployment-manifest
  [{:keys [name image command replicas] :or {replicas 1}}]
  {:apiVersion "apps/v1"
   :kind "Deployment"
   :metadata {:name name :namespace drill-namespace}
   :spec {:replicas replicas
          :selector {:matchLabels {:app name}}
          :template {:metadata {:labels {:app name}}
                     :spec {:containers [(cond-> {:name name :image image}
                                           command (assoc :command command))]}}}})

(defn pvc-manifest
  [{:keys [name storage-class size] :or {size "1Gi"}}]
  {:apiVersion "v1"
   :kind "PersistentVolumeClaim"
   :metadata {:name name :namespace drill-namespace}
   :spec {:accessModes ["ReadWriteOnce"]
          :storageClassName storage-class
          :resources {:requests {:storage size}}}})

(def drill-mac
  "The MAC every fabric drill pins.

   A kubernetes drill is contained by burning inside the fire-drill namespace. A
   fabric drill has no namespace - it edits a hypervisor's forwarding table, where
   a real guest's MAC would make a real machine unreachable. So containment comes
   from the value instead: this is OpenNebula's 02:00: prefix followed by
   203.0.113.1 from RFC 5737's documentation range, an address that cannot be
   assigned, so the MAC cannot belong to a guest on any fabric."
  "02:00:cb:00:71:01")

(def drill-vtep
  "Where the drill entry points: RFC 5737 TEST-NET-1, which is not a VTEP anywhere.
   Nothing can be attracted to it, so the entry is inert while still being exactly
   the shape a fabric detective reports."
  "192.0.2.1")

(defn fabric-pin-args
  "The static fdb entry that is the fire. `replace` rather than `add` so relighting
   a drill that is already burning is not an error."
  [vni]
  ["sudo" "bridge" "fdb" "replace" drill-mac "dev" (str "lo." vni)
   "dst" drill-vtep "self" "static"])

(defn fabric-unpin-args
  "Putting it out. The kernel errors when the entry is already gone, which the
   caller treats as extinguished rather than as a failure."
  [vni]
  ["sudo" "bridge" "fdb" "del" drill-mac "dev" (str "lo." vni)])

(defn ignite!
  "Applies the drill manifests inside the drill namespace. Returns true when
   the fire is burning. For a firebook written as runbook/execute!; one
   written with `book` applies its manifests as an effect plan."
  [context manifests]
  (let [manifest-list {:apiVersion "v1"
                       :kind "List"
                       :items (vec (cons (namespace-manifest) manifests))}
        result (shell/execute-with-stdin (json/encode manifest-list)
                                         "kubectl" (str "--context=" context)
                                         "apply" "--filename" "-")]
    (if (zero? (:exit result))
      (do (stdout/ok (format "fire started in namespace %s" drill-namespace))
          (stdout/err-println "🔥 Let the team find it with the detectives, e.g.:")
          (stdout/err-println (str "   " (catalog/run-command "runbooks/detectives/cluster.clj")))
          (stdout/err-println "   The drill is over when the findings are green again.")
          (stdout/err-println "   Extinguish with --extinguish once you are done.")
          true)
      (do (stdout/error (format "could not start the fire:\n%s" (:err result)))
          false))))

(defn extinguish!
  "Deletes the drill namespace and everything in it, then verifies it is gone.
   For a firebook written as runbook/execute!; one written with `book`
   deletes it as an effect plan, behind a confirmation."
  [context]
  (let [result (shell/execute "kubectl" (str "--context=" context)
                              "delete" "namespace" drill-namespace "--ignore-not-found")]
    (if-not (zero? (:exit result))
      (do (stdout/error (format "could not extinguish the fire:\n%s" (:err result))) false)
      (do (stdout/ok "fire extinguished, drill namespace deleted")
          (stdout/print-section "🔎 Post-check")
          (verify/all [{:description "the fire-drill namespace is gone"
                        :timeout 180
                        :interval 10
                        :verifies-absence? true
                        :check #(kubectl/confirmed-gone? context nil "namespaces" drill-namespace)}])))))

(defn burning-check
  "Post-check map proving that a drill resource exists inside the namespace.
   That the resource exists says nothing about whether a detective sees the
   fire; a firebook written with `book` runs its detectives instead."
  [context type name]
  {:description (format "%s %s/%s exists - the fire is burning" type drill-namespace name)
   :timeout 30
   :check #(some? (kubectl/get-namespaced-resource context drill-namespace type name))})

;; --- proving the fire with its detectives ------------------------------------

(defn unread
  "The exception a drill's :collect throws when it could not read what its
   detectives judge. A post-check counts it as not yet verified and prints its
   message, so silence from a host or a cluster that did not answer is never
   taken for a fire that is out."
  [message]
  (ex-info message {::kubectl/unread true}))

(defn fire-findings
  "The findings of the expected detectives over `evidence` that are the drill's
   fire. Pure. A detective that throws throws here, where an investigation
   would turn it into a finding of its own, so a broken detective never reads
   as a fire that is out."
  [{:keys [detectives fire?]} evidence ctx]
  (filter #(fire? % ctx)
          (mapcat (fn [{:keys [name detect]}]
                    (map #(assoc % :detective name) (detect evidence)))
                  detectives)))

(defn- detectives-report
  "\"the pods detective reports\" or \"the pods, stuck-pods detectives report\"."
  [detectives verb]
  (let [one? (= 1 (count detectives))]
    (format "the %s detective%s %s%s" (str/join ", " (map :name detectives))
            (if one? "" "s") verb (if one? "s" ""))))

(defn- judged-evidence
  "Freshly read evidence, or `unread` when an expected detective read nothing
   from it."
  [{:keys [detectives collect]} ctx]
  (let [evidence (collect ctx)
        blind (filter #(detective/unread? % evidence) detectives)]
    (when (seq blind)
      (throw (unread (str (detectives-report blind "find") " nothing to read"))))
    evidence))

(defn fire-check
  "The post-check of an ignite: the expected detectives report the fire."
  [{:keys [detectives timeout interval] :or {timeout 60 interval 5} :as expect} ctx]
  {:description (str "the fire is burning: " (detectives-report detectives "report") " it")
   :timeout timeout
   :interval interval
   :check #(boolean (seq (fire-findings expect (judged-evidence expect ctx) ctx)))})

(defn out-check
  "The post-check of an extinguish: the expected detectives no longer report
   the fire."
  [{:keys [detectives out-timeout interval] :or {out-timeout 60 interval 5} :as expect} ctx]
  {:description (str "the fire is out: " (detectives-report detectives "no longer report") " it")
   :timeout out-timeout
   :interval interval
   :verifies-absence? true
   :check #(empty? (fire-findings expect (judged-evidence expect ctx) ctx))})

;; --- the kubernetes drill ----------------------------------------------------

(defn- in-drill-namespace?
  [finding]
  (str/starts-with? (str (:component finding)) (str drill-namespace "/")))

(defn- manifest-list
  [manifests]
  {:apiVersion "v1"
   :kind "List"
   :items (vec (cons (namespace-manifest) manifests))})

(defn- namespace-gone-check
  [{:keys [context]}]
  {:description "the fire-drill namespace is gone"
   :timeout 180
   :interval 10
   :verifies-absence? true
   :check #(kubectl/confirmed-gone? context nil "namespaces" drill-namespace)})

(defn- kubernetes-recovery
  [{:keys [context]}]
  (str "the " drill-namespace " namespace may still exist, and the fire in it may still burn.\n"
       "Check:   kubectl --context=" context " get namespace " drill-namespace "\n"
       "A namespace stuck in Terminating waits on a finalizer of an object in it:\n"
       "         kubectl --context=" context " get all --namespace " drill-namespace "\n"
       "Run this firebook again with --extinguish once that is resolved."))

(defn- kubernetes-lit
  [_]
  (stdout/ok (format "fire started in namespace %s" drill-namespace))
  (stdout/err-println "🔥 Let the team find it with the detectives, e.g.:")
  (stdout/err-println (str "   " (catalog/run-command "runbooks/detectives/cluster.clj")))
  (stdout/err-println "   The drill is over when the findings are green again.")
  (stdout/err-println "   Extinguish with --extinguish once you are done."))

(defn kubernetes-drill
  "The parts of a drill that burns inside the drill namespace, derived from
   its :drill, :manifests and :expect: the change plans, the namespace check,
   the evidence its detectives read, and the prerequisites that needs. A
   finding counts as the fire only when it is about an object of the drill
   namespace, so a real fault elsewhere on the cluster never stands in for it."
  [{:keys [drill manifests expect]}]
  (let [{:keys [detectives fire?]} expect]
    {:derive (fn [_]
               (let [cluster (kubectl/current-cluster)]
                 {:context (kubectl/current-context) :cluster cluster
                  :system cluster :target cluster}))
     :ignite {:items (fn [_] [(str drill-namespace "/" drill)])
              :effect (fn [{:keys [context]}]
                        (effect/kubectl-stdin context (json/encode (manifest-list manifests))
                                              "apply" "--filename" "-"))}
     :extinguish {:put-out "delete the fire-drill namespace"
                  :items (fn [_] [(str "namespace " drill-namespace " and everything in it")])
                  :effect (fn [{:keys [context]}]
                            (effect/kubectl context "delete" "namespace" drill-namespace "--ignore-not-found"))
                  :checks (fn [ctx] [(namespace-gone-check ctx)])
                  :recovery kubernetes-recovery}
     :expect (assoc expect
                    :collect (fn [{:keys [context cluster]}]
                               (detective/read-evidence context cluster detectives))
                    :fire? (fn [finding ctx] (and (in-drill-namespace? finding) (fire? finding ctx))))
     :lit kubernetes-lit
     :prerequisites {:installed-tools ["kubectl"]
                     :cluster-capabilities []
                     :kubectl-can-create ["namespaces"]
                     :kubectl-can-delete ["namespaces"]
                     :kubectl-can-get (vec (distinct (cons "namespaces" (mapcat :requires detectives))))}}))

;; --- the fabric drill --------------------------------------------------------

(defn fabric-evidence
  "What the fabric detectives read about one host and VNI, or `unread` when
   the host did not answer: it is unreachable over ssh, or FRR printed no MAC
   table for the VNI."
  [connection vni]
  (let [{:keys [error] :as result} (evpn/collect-host connection vni)]
    (when error
      (throw (unread (str (:host connection) ": " error))))
    {"evpn-fdb" (:fdb result)
     "evpn-macs" (:macs result)
     "evpn-hosts" [result]}))

(defn fabric-recovery
  "What to do when putting out a fabric drill failed: the entry may still be
   pinned."
  [{:keys [conn vni]}]
  (let [target (ssh/target conn)]
    (str "the drill's static entry " drill-mac " may still be in " (:host conn) "'s table on lo." vni ".\n"
         "Check:  ssh " target " -- bridge fdb show dev lo." vni "\n"
         "Remove: ssh " target " -- " (str/join " " (fabric-unpin-args vni)))))

;; --- the book ----------------------------------------------------------------

(def ^:private ignite-recovery
  (str "the drill may be lit, partly or fully, even though this run did not prove it.\n"
       "Put it out with this firebook's --extinguish before you leave."))

(defn- extinguish-option
  [put-out]
  {:extinguish {:desc (str "Put out the fire: " put-out)
                :alias :e
                :coerce :boolean}})

(defn drill-spec
  "The spec a firebook runs: a kubernetes drill's derived parts under what the
   spec declares itself, its options with --extinguish, and its prerequisites
   joined with the drill's. Throws when an option of the spec takes
   --extinguish or its alias -e."
  [{:keys [manifests options] :as spec}]
  (when (or (contains? options :extinguish) (some #(= :e (:alias %)) (vals options)))
    (throw (ex-info "a firebook's own options must leave --extinguish and -e to the firebook" {:options options})))
  (let [derived (when manifests (kubernetes-drill spec))
        merged (merge-with (fn [d s] (if (and (map? d) (map? s)) (merge d s) s))
                           (dissoc derived :prerequisites)
                           ;; a kubernetes drill's :expect is derived from the spec's
                           (cond-> spec manifests (dissoc :expect)))]
    (assoc merged
           :options (merge options (extinguish-option (get-in merged [:extinguish :put-out])))
           :prerequisites (prerequisites/union (:prerequisites derived) (:prerequisites spec)))))

(defn run!
  "Lights the drill, or puts it out under --extinguish, through gumshoe.flow.
   Returns true only when the change ran and its post-checks held."
  [{:keys [drill fire derive ignite extinguish expect lit]} opts announcement-data]
  (let [ctx (merge {:opts opts :announcement-data announcement-data} (derive opts))
        extinguishing? (boolean (:extinguish opts))
        {:keys [items effect checks recovery]} (if extinguishing? extinguish ignite)
        ok (flow/change!
            {:confirmation {:action (if extinguishing?
                                      (str "put out the fire: " (:put-out extinguish))
                                      (str "start a fire drill: " fire))
                            :target (:target ctx)
                            :items (items ctx)}
             :announce! (when-not extinguishing?
                          #(announce/announce! (:system ctx) announcement-data
                                               (str "Fire drill started: " drill)))
             :effect (effect ctx)
             :post-checks (concat (when checks (checks ctx))
                                  [(if extinguishing? (out-check expect ctx) (fire-check expect ctx))])})]
    (cond
      flow/*dry-run* ok
      (not ok) (do (outcome/recovery! (if extinguishing? recovery (or recovery ignite-recovery)) ctx)
                   false)
      :else (do (when (and lit (not extinguishing?)) (lit ctx))
                true))))

(defn book
  "Builds and runs a firebook from its spec (see the namespace doc)."
  [{:keys [description] :as spec}]
  (let [drill (drill-spec spec)]
    (runbook/execute!
     {:description description
      :options (:options drill)
      :prerequisites (:prerequisites drill)
      :action (fn [opts {:keys [announcement-data]}] (run! drill opts announcement-data))})))
