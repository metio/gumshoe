;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.certificates.renew
  "Renews certificates managed by cert-manager. cmctl only marks a
   Certificate for reissuance, so the post-check waits for a revision above
   the one read before the renewal instead of trusting a Ready condition the
   old certificate still holds, and stops at once when cert-manager records
   a failed issuance."
  (:require [clojure.string :as str]
            [gumshoe.conditions :as conditions]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]
            [gumshoe.verify :as verify]))

(def certificate-type "certificates.cert-manager.io")

(defn certificates
  [context]
  (kubectl/namespaces-names (kubectl/get-all context certificate-type)))

(defn renew-effect
  [context selected]
  (apply effect/plan
         (for [certificate selected
               :let [{:keys [namespace name]} (kubectl/namespace-name-map certificate)]]
           (effect/cmd "cmctl" "renew" name
                       (str "--context=" context)
                       (str "--namespace=" namespace)))))

(defn issuance
  "What a Certificate's status says about its latest issuance:
   {:revision <status.revision, 0 before the first> :last-failure
   <status.lastFailureTime or nil>}, or nil for a Certificate that does not
   exist. cert-manager raises the revision once for every certificate it
   stores, so a raised revision is evidence of a new certificate; Ready
   alone is not, it stays True while the old certificate is still valid."
  [certificate]
  (when certificate
    {:revision (or (-> certificate :status :revision) 0)
     :last-failure (-> certificate :status :lastFailureTime)}))

(defn renewed?
  "Whether certificate shows an issuance newer than before, the issuance
   read before the renewal: its revision is above before's, it is no longer
   issuing, and it is Ready."
  [before certificate]
  (verify/object-holds? certificate
                        #(and (> (:revision (issuance %)) (or (:revision before) 0))
                              (not (conditions/holds? % "Issuing"))
                              (conditions/ready? %))))

(defn renewal-failure
  "Why the issuance of certificate failed after before was read, or nil:
   cert-manager stamps lastFailureTime on every failed issuance, so a stamp
   that is new names a failure of this renewal."
  [before certificate]
  (let [{:keys [last-failure]} (issuance certificate)]
    (when (and last-failure (not= last-failure (:last-failure before)))
      (str "cert-manager failed the issuance at " last-failure
           (when-let [message (conditions/first-line (:message (conditions/condition certificate "Issuing")))]
             (str ": " message))))))

(defn issued
  "The issuance of every selected Certificate, keyed by namespace/name."
  [context selected]
  (into {} (for [certificate selected
                 :let [{:keys [namespace name]} (kubectl/namespace-name-map certificate)]]
             [certificate (issuance (kubectl/get-namespaced-resource context namespace certificate-type name))])))

(defn renewed-checks
  [context selected issued]
  (for [certificate selected
        :let [{:keys [namespace name]} (kubectl/namespace-name-map certificate)
              before (get issued certificate)
              fetch #(kubectl/get-namespaced-resource context namespace certificate-type name)]]
    {:description (format "certificate %s was reissued (revision above %d) and is Ready"
                          certificate (or (:revision before) 0))
     :timeout 180 :interval 10
     :check #(renewed? before (fetch))
     :abort #(renewal-failure before (fetch))}))

(def selection
  {:mode :many :label "Certificate" :flag :certificates :candidates certificates})

(mutation/book
 {:description "Renews certificates managed by cert-manager"
  :options (select/options selection {:certificates {:desc "namespace/name of certificates to renew, repeatable - interactive selection when omitted"}})
  :prerequisites {:installed-tools ["cmctl" "kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [certificate-type]}
  :select selection
  :confirm {:action "renew certificates - new certificates are requested from the issuer" :destructive? false}
  :announce (fn [{:keys [target]}] (format "Renew certificates [%s]" (str/join ", " target)))
  :reads {:issued (fn [{:keys [context target]}] (issued context target))}
  :effect (fn [{:keys [context target]}] (renew-effect context target))
  :verify (fn [{:keys [context target issued]}] (renewed-checks context target issued))})
