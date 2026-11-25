;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.firebook-books-test
  "Characterises the firebooks end to end against a recorded cluster and a
   recorded hypervisor: for each book and scenario, what the confirmation
   shows, the commands it runs, the announcement, the post-checks it declares
   and whether they held, what the run records and everything it prints. The
   transcripts are kept in tests/golden/firebook_books.edn.

   A scenario names what the cluster or host holds once the change ran, the
   exit of each change command, whether reads answer, whether the operator
   confirms, and whether the run is a rehearsal. Post-checks run once each,
   with their declared timeout kept in the transcript, so no scenario sleeps.

   Regenerate the file after an intended change with
   (gumshoe.firebook-books-test/write-golden!) and review its diff."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.flow :as flow]
            [gumshoe.streams-support :as streams]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

(def golden-path "tests/golden/firebook_books.edn")

;; --- the recorded cluster ----------------------------------------------------------

(def ^:private unanswered
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"})

(defn- arg-value
  [argv prefix]
  (some #(when (str/starts-with? % prefix) (subs % (count prefix))) argv))

(defn- kubectl-answer
  "What kubectl get answers for argv against {:objects [[type object]..] :answering?}."
  [{:keys [objects answering?]} argv]
  (let [namespace (arg-value argv "--namespace=")
        [_ type name] (drop-while #(not= "get" %) argv)
        name (when (and name (not (str/starts-with? name "-"))) name)
        matches (fn [[t o]] (and (= t type)
                                 (or (nil? namespace) (= namespace (-> o :metadata :namespace)))
                                 (or (nil? name) (= name (-> o :metadata :name)))))]
    (cond
      (not answering?) unanswered
      (some #{"--output=name"} argv)
      {:exit 0 :out (str/join "\n" (for [[t o] objects :when (matches [t o])] (str t "/" (-> o :metadata :name)))) :err ""}
      name (if-let [[_ o] (first (filter matches objects))]
             {:exit 0 :out (json/generate-string o) :err ""}
             {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" type name)})
      :else {:exit 0 :out (json/generate-string {:items (map second (filter matches objects))}) :err ""})))

;; --- the recorded hypervisor --------------------------------------------------------

(def ^:private ssh-unanswered
  {:exit 255 :out "" :err "ssh: connect to host hv1 port 22: Connection timed out"})

(def ^:private drill-row "02:00:cb:00:71:01 dev lo.4208 dst 192.0.2.1 self static")
(def ^:private guest-row "aa:bb:cc:dd:ee:01 dev tap0 master br0")

(defn- ssh-answer
  "What a read over ssh answers against {:fdb [rows] or :unread :answering?}."
  [{:keys [fdb answering?]} argv]
  (let [line (str/join " " argv)]
    (cond
      (not answering?) ssh-unanswered
      (str/includes? line "exit 0") {:exit 0 :out "" :err ""}
      (str/includes? line "bridge fdb show") (if (= :unread fdb)
                                               {:exit 1 :out "" :err "bridge: command not found"}
                                               {:exit 0 :out (str/join "\n" fdb) :err ""})
      (str/includes? line "show evpn mac vni") {:exit 0 :out "Number of MACs (local and remote) known for this VNI: 1\n" :err ""}
      (str/includes? line "show evpn vni") {:exit 0 :out "VNI: 4208\n Local Vtep Ip: 10.0.0.11\n" :err ""}
      :else {:exit 0 :out "" :err ""})))

;; --- running a book ------------------------------------------------------------------

(defn load-book
  "The runbook config a book file hands to the harness."
  [path]
  (binding [runbook/*loading* true]
    (load-file path)))

(defn- normalise
  "x with every instant replaced, so a transcript does not depend on the clock."
  [x]
  (edn/read-string (str/replace (pr-str x) #"\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?Z" "<instant>")))

(defn- extinguish-script?
  "Whether argv runs the fabric drill's idempotent extinguish script."
  [argv]
  (boolean (some #(and (string? %) (str/includes? % "exec bridge fdb del")) argv)))

(defn- extinguish-answer
  "What the extinguish script answers against the recorded table: it fails when
   the table cannot be read, succeeds without a delete when the drill entry is
   absent, and otherwise exits as the delete does (`del-exit`)."
  [{:keys [fdb answering?]} del-exit]
  (cond
    (not answering?) ssh-unanswered
    (= :unread fdb) {:exit 1 :out "" :err "bridge: command not found"}
    (some #(str/includes? % "02:00:cb:00:71:01") fdb)
    {:exit del-exit :out "" :err (if (zero? del-exit) "" "RTNETLINK answers: Operation not permitted")}
    :else {:exit 0 :out "" :err ""}))

(defn- change-command?
  [argv]
  (boolean (some #{"apply" "delete" "replace" "del"} argv)))

(defn transcript
  "What the book does in one scenario."
  [config {:keys [opts objects fdb answering? exits confirm? dry-run?]
           :or {answering? true confirm? true exits {}}}]
  (let [state {:objects objects :fdb fdb :answering? answering?}
        calls (atom [])
        confirmations (atom [])
        announcements (atom [])
        checks (atom [])
        record (outcome/fresh)
        exit-of (fn [argv] (get exits (some #{"apply" "delete" "replace" "del"} argv) 0))
        run-command (fn [argv]
                      (swap! calls conj (str/join " " argv))
                      (cond
                        (extinguish-script? argv)
                        (extinguish-answer state (get exits "del" 0))

                        (change-command? argv)
                        (let [exit (exit-of argv)]
                          {:exit exit :out "" :err (if (zero? exit) "" "Error from server (Forbidden): refused")})
                        (= "ssh" (first argv)) (ssh-answer state argv)
                        :else (kubectl-answer state argv)))
        eventually verify/eventually
        out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        result (with-redefs [shell/execute (fn [& argv] (run-command argv))
                             shell/execute-with-stdin (fn [stdin & argv]
                                                        (run-command (concat argv [(str "<stdin " (count (get (json/parse-string stdin true) :items)) " items>")])))
                             shell/run-with-output (fn [& argv] (:exit (run-command argv)))
                             kubectl/current-context (constantly "ctx")
                             kubectl/current-cluster (constantly "cluster")
                             interact/confirm! (fn [request]
                                                 (swap! confirmations conj (select-keys request [:action :target :items :destructive?]))
                                                 (when-not confirm? (outcome/aborted! "declined at the confirmation"))
                                                 confirm?)
                             announce/announce! (fn [system _ message] (swap! announcements conj [system message]) nil)
                             verify/eventually (fn [check]
                                                 (swap! checks conj (select-keys check [:description :timeout :interval :soft?]))
                                                 (eventually (assoc check :timeout 0)))]
                 (binding [*out* out *err* err
                           ssh/*user-config* nil
                           flow/*dry-run* (boolean dry-run?)
                           outcome/*record* record]
                   (try ((:action config) opts {:announcement-data {:cluster "cluster"}})
                        (catch Exception e (str "threw: " (ex-message e))))))]
    (normalise
     (streams/with-stdout
      {:result result
       :confirmations @confirmations
       :announcements @announcements
       :calls @calls
       :checks @checks
       :record @record
       :out (str/split-lines (stdout/strip-colors (str/trim (str err))))}
      (str out) #(str/split-lines (stdout/strip-colors (str/trim %)))))))

(defn surface
  [config]
  (select-keys config [:description :options :prerequisites :announce?]))

;; --- the books and their scenarios ---------------------------------------------------

(def ^:private old "2026-01-01T00:00:00Z")

(defn- deployment
  [name]
  ["deployments" {:metadata {:namespace "fire-drill" :name name}
                  :spec {:replicas 1}}])

(defn- pod
  [name status]
  ["pods" {:metadata {:namespace "fire-drill" :name (str name "-7d9f8-abcde") :creationTimestamp old
                      :ownerReferences [{:kind "ReplicaSet" :name (str name "-7d9f8")}]}
           :spec {:containers [{:name name}]}
           :status (merge {:phase "Running"} status)}])

(defn- waiting
  [name reason]
  {:containerStatuses [{:name name :restartCount 4 :ready false
                        :state {:waiting {:reason reason :message (str "the drill's " reason)}}
                        :lastState {:terminated {:exitCode 1 :reason "Error"}}}]})

(defn- running
  [name]
  {:containerStatuses [{:name name :restartCount 0 :ready true
                        :state {:running {:startedAt old}}}]})

(def ^:private namespace-object
  "The drill namespace as a drill creates it, carrying gumshoe's label."
  ["namespaces" {:metadata {:name "fire-drill" :labels {:app.kubernetes.io/managed-by "gumshoe"}}}])

(def ^:private foreign-namespace
  "A namespace that happens to be called fire-drill and no drill created."
  ["namespaces" {:metadata {:name "fire-drill" :labels {:team "payments"}}}])

(defn- pending-claim
  [phase]
  ["persistentvolumeclaims" {:metadata {:namespace "fire-drill" :name "pending-pvc" :creationTimestamp old}
                             :spec {:storageClassName "this-storage-class-does-not-exist"}
                             :status {:phase phase}}])

(defn- kubernetes-scenarios
  "The scenarios of a kubernetes firebook. `burning` is what the cluster holds
   while the fire burns and a detective can see it, `unseen` what it holds when
   the drill's objects exist but nothing about them is wrong."
  [burning unseen]
  {:ignite {:opts {} :objects (cons namespace-object burning)}
   :ignite-unseen {:opts {} :objects (cons namespace-object unseen)}
   :ignite-apply-fails {:opts {} :objects [] :exits {"apply" 1}}
   :ignite-declined {:opts {} :objects [] :confirm? false}
   :ignite-dry-run {:opts {} :objects [] :dry-run? true}
   :extinguish {:opts {:extinguish true} :objects []}
   :extinguish-delete-fails {:opts {:extinguish true} :objects (cons namespace-object burning) :exits {"delete" 1}}
   :extinguish-still-burning {:opts {:extinguish true} :objects (cons namespace-object burning)}
   :extinguish-unanswered {:opts {:extinguish true} :objects [] :answering? false}
   :extinguish-dry-run {:opts {:extinguish true} :objects (cons namespace-object burning) :dry-run? true}
   :ignite-foreign-namespace {:opts {} :objects (cons foreign-namespace burning)}
   :ignite-unanswered {:opts {} :objects [] :answering? false}
   :extinguish-foreign-namespace {:opts {:extinguish true} :objects (cons foreign-namespace burning)}})

(def ^:private fabric-opts {:host "hv1" :vni 4208})

(def books
  {"firebooks/kubernetes/crash_loop.clj"
   (kubernetes-scenarios [(deployment "crash-loop") (pod "crash-loop" (waiting "crash-loop" "CrashLoopBackOff"))]
                         [(deployment "crash-loop") (pod "crash-loop" (running "crash-loop"))])

   "firebooks/kubernetes/image_pull.clj"
   (kubernetes-scenarios [(deployment "image-pull") (pod "image-pull" (waiting "image-pull" "ImagePullBackOff"))]
                         [(deployment "image-pull") (pod "image-pull" (running "image-pull"))])

   "firebooks/kubernetes/pending_pvc.clj"
   (kubernetes-scenarios [(pending-claim "Pending")]
                         [(pending-claim "Bound")])

   "firebooks/fabric/stale_fdb_entry.clj"
   {:ignite {:opts fabric-opts :fdb [guest-row drill-row]}
    :ignite-unseen {:opts fabric-opts :fdb [guest-row]}
    :ignite-pin-fails {:opts fabric-opts :fdb [guest-row] :exits {"replace" 1}}
    :ignite-declined {:opts fabric-opts :fdb [guest-row] :confirm? false}
    :ignite-dry-run {:opts fabric-opts :fdb [guest-row] :dry-run? true}
    :extinguish {:opts (assoc fabric-opts :extinguish true) :fdb [guest-row]}
    :extinguish-unpin-fails {:opts (assoc fabric-opts :extinguish true) :fdb [guest-row drill-row] :exits {"del" 1}}
    ;; the entry is already gone and a delete would be refused by the kernel:
    ;; the extinguish succeeds and the out-check runs
    :extinguish-already-out {:opts (assoc fabric-opts :extinguish true) :fdb [guest-row] :exits {"del" 255}}
    :extinguish-still-burning {:opts (assoc fabric-opts :extinguish true) :fdb [guest-row drill-row]}
    :extinguish-unanswered {:opts (assoc fabric-opts :extinguish true) :fdb [guest-row drill-row] :answering? false}
    :extinguish-dry-run {:opts (assoc fabric-opts :extinguish true) :fdb [guest-row drill-row] :dry-run? true}
    ;; FRR answers and only `bridge fdb show` fails: the empty table still
    ;; reads as a fire that is out
    :extinguish-fdb-unread {:opts (assoc fabric-opts :extinguish true) :fdb :unread}
    ;; FRR answers and `bridge fdb show` answers with no entries at all
    :extinguish-fdb-empty {:opts (assoc fabric-opts :extinguish true) :fdb []}
    :ignite-unanswered {:opts fabric-opts :fdb [guest-row] :answering? false}}})

(defn transcripts
  []
  (into (sorted-map)
        (for [[path scenarios] books
              :let [config (load-book path)]]
          [path (into (sorted-map :surface (normalise (surface config)))
                      (for [[scenario setup] scenarios]
                        [scenario (transcript config setup)]))])))

(defn write-golden!
  []
  (spit golden-path
        ;; split, so a REUSE scan does not read these strings as this file's header
        (str ";; SPDX" "-FileCopyrightText: The gumshoe Authors\n"
             ";; SPDX" "-License-Identifier: 0BSD\n"
             ";; Generated by (gumshoe.firebook-books-test/write-golden!); review its diff.\n"
             (with-out-str (pprint/pprint (transcripts))))))

(deftest every-firebook-keeps-its-transcripts
  (let [golden (edn/read-string (slurp golden-path))
        actual (transcripts)]
    (is (= (set (keys golden)) (set (keys actual))) "the same books")
    (doseq [[path scenarios] actual
            [scenario transcript] scenarios]
      (testing (str path " " scenario)
        (is (= (get-in golden [path scenario]) transcript))))))
