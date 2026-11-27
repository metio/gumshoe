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
      :derive (fn [opts] -> map)   ; merged into every callback's ctx; names the
                                   ; :target the confirmation shows, the :system
                                   ; the announcement names, and the :context
      :reads {name (fn [ctx] -> value)}   ; optional, live reads of both stages;
                                          ; they see the engine's ctx, before
                                          ; what :derive returns
      :refusal (fn [ctx] -> reason or nil) ; optional, guards both stages
      :ignite {:items  (fn [ctx] -> seq)        ; what the confirmation lists
               :effect (fn [ctx] -> effect plan)
               :recovery \"...\" or (fn [ctx] -> string)}  ; optional
      :extinguish {:put-out \"...\"             ; \"put out the fire: <put-out>\"
                   :items .. :effect .. :recovery ..
                   :checks (fn [ctx] -> post-checks)}  ; optional, before the out check
      :expect {:detectives [..]                 ; each declares :requires
               :collect (fn [ctx] -> evidence)  ; throws `unread` when it read nothing
               :fire? (fn [finding ctx] -> bool)
               :empty-is-out \"why\"            ; optional, see empty-reading
               :timeout 30 :interval 5 :out-timeout 60}
      :lit (fn [ctx])}            ; prints what to do once the fire burns

   `stages` turns the drill into two mutation specs, the ignite and the
   extinguish, and `run!` runs the one the flags ask for through
   gumshoe.mutation/run-on!, as every other change runs: :reads and
   :refusal are judged before the confirmation and again after it, a
   rehearsal under --dry-run shows the plan and changes nothing, and a
   change that fails records its :recovery, which the harness prints under
   the FAILED banner: the fire may still burn. A kubernetes drill refuses a
   fire-drill namespace no drill created (foreign-namespace). The firebook
   keeps only what is a drill's own: which stage runs, the fire and out
   checks, and the :lit message.

   After the ignite the burning check runs the expected detectives over
   freshly read evidence until one reports the fire; after the extinguish
   the out check runs them until none does. Evidence a detective read
   nothing from counts as not yet verified, and so does evidence a detective
   read only empty lists from, unless the drill says why an empty reading
   is real evidence of absence (:empty-is-out): neither check holds on a
   read that did not answer, and the out check never holds on an empty
   one."
  (:refer-clojure :exclude [run!])
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.catalog :as catalog]
            [gumshoe.detective :as detective]
            [gumshoe.deletes :as deletes]
            [gumshoe.effect :as effect]
            [gumshoe.evpn :as evpn]
            [gumshoe.flags :as flags]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.reads.result :as result]
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
  "Putting it out by hand, as the recovery text prints it. The kernel errors
   when the entry is already gone; a human reads that as done, while the
   extinguish effect runs fabric-extinguish-args instead."
  [vni]
  ["sudo" "bridge" "fdb" "del" drill-mac "dev" (str "lo." vni)])

(defn fabric-extinguish-script
  "The shell script that puts the drill out idempotently: it reads the table
   first and deletes the entry only when it is there, so a repeated extinguish,
   or one after an ignite whose pin failed, succeeds without a delete the
   kernel would refuse. A table that cannot be read (no such device, no
   permission) still fails, so an unreadable fabric is never reported out."
  [vni]
  (let [dev (str "lo." (long vni))]
    (str "out=$(bridge fdb show dev " dev ") || exit 1; "
         "printf '%s\\n' \"$out\" | grep -qiF " drill-mac " || exit 0; "
         "exec bridge fdb del " drill-mac " dev " dev)))

(defn fabric-extinguish-args
  "The extinguish effect's command: fabric-extinguish-script run as root."
  [vni]
  ["sudo" "sh" "-c" (fabric-extinguish-script vni)])

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
  (let [result (apply shell/execute "kubectl" (str "--context=" context)
                     (deletes/args {:type "namespace" :name drill-namespace}))]
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
  (result/unread-exception message))

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

(defn empty-reading
  "Why evidence is too empty for the out check to call the fire out, or nil.
   Pure. An expected detective whose required evidence all came back as
   empty lists judged nothing: an fdb table read as no rows, a listing that
   answered with no objects. Its silence then says as little as a read that
   did not answer, so it never shows the fire is out. A drill whose empty
   reading is real evidence of absence says why as :empty-is-out, and the
   reading is accepted as it is. A kubectl list whose :items are empty is an
   empty reading too (see gumshoe.detective/empty-subjects?)."
  [{:keys [detectives empty-is-out]} evidence]
  (when-not empty-is-out
    (let [blind (filter (fn [{:keys [requires]}]
                          (and (seq requires) (every? #(detective/empty-subjects? (get evidence %)) requires)))
                        detectives)]
      (when (seq blind)
        (format "%s only an empty %s, and an empty reading never shows the fire is out"
                (detectives-report blind "read")
                (str/join ", " (distinct (mapcat :requires blind))))))))

(defn out-check
  "The post-check of an extinguish: the expected detectives no longer report
   the fire, over evidence that answered and is not empty (see
   empty-reading). Either kind of silence counts as not yet verified."
  [{:keys [detectives out-timeout interval] :or {out-timeout 60 interval 5} :as expect} ctx]
  {:description (str "the fire is out: " (detectives-report detectives "no longer report") " it")
   :timeout out-timeout
   :interval interval
   :verifies-absence? true
   :check #(let [evidence (judged-evidence expect ctx)]
             (when-let [why (empty-reading expect evidence)]
               (throw (unread why)))
             (empty? (fire-findings expect evidence ctx)))})

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

(def ^:private managed-by (keyword "app.kubernetes.io/managed-by"))

(defn foreign-namespace
  "Why the drill must not burn in or delete the namespace object read as
   the drill namespace, or nil. Pure. A namespace a drill created carries
   gumshoe's managed-by label (see namespace-manifest); one without it
   belongs to somebody else, who named it fire-drill, and neither applying
   the drill into it nor deleting it with everything in it is the drill's
   to do. An absent namespace is nil: the ignite creates it, and the
   extinguish finds nothing to delete."
  [namespace]
  (when (and namespace (not= "gumshoe" (get-in namespace [:metadata :labels managed-by])))
    (str "namespace " drill-namespace " exists and no drill created it (it lacks the label "
         "app.kubernetes.io/managed-by=gumshoe), so the drill neither burns in it nor deletes it")))

(defn kubernetes-drill
  "The parts of a drill that burns inside the drill namespace, derived from
   its :drill, :manifests and :expect: the change plans, the namespace check,
   the guard against a namespace no drill created, the evidence its
   detectives read, and the prerequisites that needs. A finding counts as
   the fire only when it is about an object of the drill namespace, so a
   real fault elsewhere on the cluster never stands in for it."
  [{:keys [drill manifests expect]}]
  (let [{:keys [detectives fire?]} expect]
    {:reads {:drill-namespace (fn [{:keys [context]}]
                                (kubectl/get-cluster-resource context "namespaces" drill-namespace))}
     :refusal (fn [{:keys [drill-namespace]}] (foreign-namespace drill-namespace))
     :derive (fn [_]
               (let [{:keys [context cluster]} (kubectl/kube-connection)]
                 {:context context :cluster cluster
                  :system cluster :target cluster}))
     :ignite {:items (fn [_] [(str drill-namespace "/" drill)])
              :effect (fn [{:keys [context]}]
                        (effect/kubectl-stdin context (json/encode (manifest-list manifests))
                                              "apply" "--filename" "-"))}
     :extinguish {:put-out "delete the fire-drill namespace"
                  :items (fn [_] [(str "namespace " drill-namespace " and everything in it")])
                  ;; --wait=false: kubectl's default waits as long as the
                  ;; namespace stays Terminating, which a finalizer can make
                  ;; forever; namespace-gone-check is the bounded wait, and
                  ;; its failure prints kubernetes-recovery
                  :effect (fn [{:keys [context]}]
                            (deletes/effect context {:type "namespace" :name drill-namespace}
                                            {:wait false}))
                  :checks (fn [ctx] [(namespace-gone-check ctx)])
                  :recovery kubernetes-recovery}
     :expect (assoc expect
                    :empty-is-out (str "the extinguish first confirms the " drill-namespace
                                       " namespace is gone, so an empty listing shows the drill's objects are gone too")
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

(defn- expect-problem
  "Why expect cannot judge the fire out fail-closed, or nil."
  [{:keys [detectives empty-is-out] :as expect}]
  (cond
    (and (contains? expect :empty-is-out) (not (and (string? empty-is-out) (not (str/blank? empty-is-out)))))
    ":empty-is-out must be a string saying why an empty reading shows the fire is out"

    (and (not empty-is-out) (some #(empty? (:requires %)) detectives))
    (str "an expected detective without :requires cannot tell an empty reading from the fire being out: "
         (str/join ", " (map :name (filter #(empty? (:requires %)) detectives))))))

(defn drill-spec
  "The spec a firebook runs: a kubernetes drill's derived parts under what the
   spec declares itself, its options with --extinguish, and its prerequisites
   joined with the drill's. Throws when an option of the spec takes
   --extinguish or its alias -e, and when its :expect cannot judge the fire
   out fail-closed (see empty-reading)."
  [{:keys [manifests options] :as spec}]
  (when (or (contains? options :extinguish) (some #(= :e (:alias %)) (vals options)))
    (throw (ex-info "a firebook's own options must leave --extinguish and -e to the firebook" {:options options})))
  (let [derived (when manifests (kubernetes-drill spec))
        merged (merge-with (fn [d s] (if (and (map? d) (map? s)) (merge d s) s))
                           (dissoc derived :prerequisites)
                           ;; a kubernetes drill's :expect is derived from the spec's
                           (cond-> spec manifests (dissoc :expect)))]
    (when-let [problem (expect-problem (:expect merged))]
      (throw (ex-info (str "a firebook's :expect is refused: " problem) {:expect (:expect merged)})))
    (assoc merged
           :options (flags/merge-ordered options (extinguish-option (get-in merged [:extinguish :put-out])))
           :prerequisites (prerequisites/union (:prerequisites derived) (:prerequisites spec)))))

(defn- drill-ctx
  "The context a firebook callback receives: the engine's, with what the
   drill's :derive returned merged over it."
  [ctx]
  (merge ctx (::drill ctx)))

(defn- in-drill
  "f called with drill-ctx, for a callback written against the drill's
   context; a string or nil as it is."
  [f]
  (if (fn? f) (fn [ctx] (f (drill-ctx ctx))) f))

(defn connection
  "What gumshoe.mutation's :connect returns for what a drill's :derive
   returned: its :context, its :target as what the confirmation shows the
   change acting on, and its :system as what the changelog names."
  [{:keys [context target system]}]
  {:context context :cluster target :announce-as system})

(defn stages
  "The two changes of a drill-spec, as mutation specs: {:ignite .. :extinguish ..}.
   Each runs through gumshoe.mutation/run-on! as every other change does, so
   it is judged, confirmed, guarded, announced, applied and verified there.
   The drill's :derive becomes the :connect and the per-run value every
   callback reads its context from; its :reads and :refusal guard both
   stages, before the confirmation and again after it. The ignite announces
   the drill and proves the fire burns (fire-check); the extinguish proves
   it is out (out-check) after its own :checks. Neither declares :done: a
   drill relit over a burning fire proves the detective again, and an
   extinguish skipped because the detectives see no fire would leave the
   drill's objects behind. Neither is destructive: a drill acts only on what
   it is contained to (its namespace, or a MAC no guest can hold)."
  [{:keys [drill fire derive ignite extinguish expect prerequisites reads refusal]}]
  (let [shared (cond-> {:prerequisites (or prerequisites {})
                        :connect (fn [opts] (connection (derive opts)))
                        :values {::drill (fn [{:keys [opts]}] (derive opts))}}
                 reads (assoc :reads reads)
                 refusal (assoc :refusal (in-drill refusal)))]
    {:ignite (merge shared
                    {:confirm {:action (str "start a fire drill: " fire) :destructive? false}
                     :items (in-drill (:items ignite))
                     :effect (in-drill (:effect ignite))
                     :announce (fn [_] (str "Fire drill started: " drill))
                     :verify (fn [ctx] [(fire-check expect (drill-ctx ctx))])
                     :recovery (in-drill (or (:recovery ignite) ignite-recovery))})
     :extinguish (cond-> (merge shared
                                {:confirm {:action (str "put out the fire: " (:put-out extinguish)) :destructive? false}
                                 :items (in-drill (:items extinguish))
                                 :effect (in-drill (:effect extinguish))
                                 :verify (fn [ctx]
                                           (let [ctx (drill-ctx ctx)]
                                             (concat (when-let [checks (:checks extinguish)] (checks ctx))
                                                     [(out-check expect ctx)])))})
                   (:recovery extinguish) (assoc :recovery (in-drill (:recovery extinguish))))}))

(defn run!
  "Lights the drill, or puts it out under --extinguish, as the stage of
   `stages` gumshoe.mutation/run-on! runs, and prints the drill's :lit once
   a real ignite proved the fire burns. Returns true only when the change
   ran and its post-checks held, or a rehearsal showed it."
  [{:keys [lit] :as drill} opts announcement-data]
  (let [derive (memoize (:derive drill))
        extinguishing? (boolean (:extinguish opts))
        stage (get (stages (assoc drill :derive derive)) (if extinguishing? :extinguish :ignite))
        ok (mutation/run-on! stage (:target (derive opts)) opts announcement-data)]
    (when (and ok lit (not extinguishing?) (not flow/*dry-run*))
      (lit (merge {:opts opts :announcement-data announcement-data} (derive opts))))
    ok))

(defn book
  "Builds and runs a firebook from its spec (see the namespace doc)."
  [{:keys [description] :as spec}]
  (let [drill (drill-spec spec)]
    (runbook/execute!
     {:description description
      :options (:options drill)
      :prerequisites (:prerequisites drill)
      :action (fn [opts {:keys [announcement-data]}] (run! drill opts announcement-data))})))
