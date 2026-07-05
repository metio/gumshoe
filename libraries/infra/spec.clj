;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.spec
  "Schemas for the load-bearing data shapes, written in clojure.spec, which
   babashka supports natively. Two uses:

   - validating an env.edn at the boundary, so a well-formed-EDN-but-wrong-shape
     config (a typo'd :select, a string where a list belongs) is reported to the
     operator instead of silently ignored;
   - pinning the shape of findings, effects, and book descriptors in the test
     suite, so a drift in any of those is caught the moment it happens.

   Validation here never throws and is never required for a book to run - it is
   a safety net over the existing graceful degradation, not a gate in front of it."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

;; ---------------------------------------------------------------------------
;; findings (infra.detective)

(s/def ::severity #{:critical :warning :info})
(s/def ::component string?)
(s/def ::summary non-blank-string?)
(s/def ::hint (s/nilable string?))
(s/def ::detective string?)
(s/def ::finding (s/keys :req-un [::severity ::component ::summary]
                         :opt-un [::hint ::detective]))
(s/def ::findings (s/coll-of ::finding))

;; ---------------------------------------------------------------------------
;; effects (infra.effect) - a plan is a sequence of [op & args] vectors

(def effect-ops
  "Every operation the effect interpreter knows. A plan carrying any other op
   would fail at run time, so the set is the contract."
  #{:kubectl :kubectl-stdin :ssh :cmd :note})

(s/def ::effect (s/and vector? (s/cat :op effect-ops :args (s/* any?))))
(s/def ::plan (s/and sequential? (s/coll-of ::effect)))

;; ---------------------------------------------------------------------------
;; env.edn (infra.config)

(s/def :env/interface string?)
(s/def :env/vpn (s/keys :opt-un [:env/interface]))
(s/def :env/mgr-hosts (s/coll-of string?))
(s/def :env/ceph (s/keys :opt-un [:env/mgr-hosts]))
(s/def :env/select (s/map-of keyword? some?))
(s/def :env/environment (s/keys :opt-un [:env/select :env/vpn :env/ceph]))
(s/def :env/environments (s/map-of keyword? :env/environment))
(s/def :env/defaults (s/keys :opt-un [:env/vpn :env/ceph]))
(s/def ::env-config (s/keys :opt-un [:env/environments :env/defaults :env/vpn :env/ceph]))

;; ---------------------------------------------------------------------------
;; book descriptors (infra.mutation, infra.detective)

(s/def :book/description non-blank-string?)
(s/def :book/prerequisites map?)

(s/def :book/detectives (s/coll-of map?))
(s/def ::detective-book (s/keys :req-un [:book/description :book/detectives :book/prerequisites]))

(s/def :book/mode #{:one :many :namespaced})
(s/def :book/label non-blank-string?)
(s/def :book/candidates ifn?)
(s/def :book/select (s/keys :req-un [:book/mode :book/label :book/candidates]))
(s/def :book/action non-blank-string?)
(s/def :book/confirm (s/keys :req-un [:book/action]))
(s/def :book/effect ifn?)
(s/def ::mutation-book (s/keys :req-un [:book/description :book/select :book/confirm :book/effect]))

;; ---------------------------------------------------------------------------
;; validation helpers - safe, human-readable

(defn valid?
  [spec value]
  (try (s/valid? spec value) (catch Exception _ false)))

(defn problems
  "A seq of human-readable problem strings for value against spec, or nil when
   it conforms. Never throws."
  [spec value]
  (try
    (when-not (s/valid? spec value)
      (->> (::s/problems (s/explain-data spec value))
           (map (fn [{:keys [in pred val]}]
                  ;; drop spec's internal map-entry indices from the path so it
                  ;; reads as the key trail an operator would recognize
                  (let [path (remove integer? in)]
                    (format "%s: %s is not %s"
                            (if (seq path) (str/join " -> " (map pr-str path)) "config")
                            (pr-str val)
                            (pr-str pred)))))
           (seq)))
    (catch Exception e
      [(str "could not validate: " (or (ex-message e) (str e)))])))

(defn env-config-problems
  "Problems with an env.edn config map, plus a semantic nudge: an environment
   with no :select can never be auto-chosen from the current cluster. Returns
   nil when the config is well-shaped."
  [config]
  (let [structural (problems ::env-config config)
        no-select (for [[nm env] (:environments config)
                        :when (not (seq (:select env)))]
                    (format ":environments -> %s has no :select, so it is only usable via GUMSHOE_ENVIRONMENT"
                            (pr-str nm)))]
    (seq (concat structural no-select))))
