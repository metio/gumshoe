;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.toggle-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.shell :as shell]
            [gumshoe.toggle :as toggle]))

(def ^:private widget-type "widgets.example.io")

(defn- widget
  [namespace name held]
  {:metadata {:namespace namespace :name name :uid (str "uid-" name) :generation 1}
   :spec {:held held}})

(defn- held? [widget] (true? (-> widget :spec :held)))

(defn- refusal
  [widget wanted]
  (cond
    (nil? widget) "the Widget no longer exists"
    (= wanted (held? widget)) (if wanted "it is held already" "it is not held")
    :else nil))

(def ^:private declaration
  {:spec {:options {:widget {:desc "\"<namespace>/<name>\"" :coerce :string}}
          :select {:mode :one :label "Widget" :flag :widget}
          :subject {:type widget-type}}
   :refusal refusal
   :items (fn [{:keys [object]} wanted] [(format "%s %s" (if wanted "hold" "release") (-> object :metadata :name))])
   :patch (fn [wanted] (json/generate-string {:spec {:held wanted}}))
   :applied? (fn [object wanted] (= wanted (held? object)))
   :verify {:timeout 30 :interval 3}
   :on {:verb "hold" :empty-message "every Widget is held already" :confirm {:action "hold a Widget"}
        :announce "Hold Widget %s" :verified "Widget %s is held"}
   :off {:verb "release" :empty-message "no Widget is held" :confirm {:action "release a Widget"}
         :announce "Release Widget %s" :verified "Widget %s is released"
         :verify {:timeout 60}}})

(defn- cluster
  "A shell/execute answering kubectl get against objects."
  [objects]
  (fn [& argv]
    (let [[_ _ name] (drop-while #(not= "get" %) argv)
          by-name (first (filter #(= name (-> % :metadata :name)) objects))]
      (cond
        (some #{"--all-namespaces"} argv) {:exit 0 :out (json/generate-string {:items objects}) :err ""}
        by-name {:exit 0 :out (json/generate-string by-name) :err ""}
        :else {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" widget-type name)}))))

(def ^:private objects
  [(widget "zeta" "b" false) (widget "alpha" "c" true) (widget "alpha" "a" false)])

(deftest each-side-is-the-declaration-at-its-wanted-state
  (let [{:keys [on off]} (toggle/pair declaration)
        ctx {:context "ctx" :target "alpha/a" :ref {:type widget-type :namespace "alpha" :name "a"}}]
    (doseq [[spec wanted expected]
            [[on true {:candidates ["alpha/a" "zeta/b"] :empty "every Widget is held already" :action "hold a Widget"
                       :items ["hold a"] :fallback ["hold alpha/a"] :refusal "it is held already"
                       :announce "Hold Widget alpha/a" :patch "--patch={\"spec\":{\"held\":true}}"
                       :check {:description "Widget alpha/a is held" :timeout 30 :interval 3}}]
             [off false {:candidates ["alpha/c"] :empty "no Widget is held" :action "release a Widget"
                         :items ["release a"] :fallback ["release alpha/a"] :refusal "it is not held"
                         :announce "Release Widget alpha/a" :patch "--patch={\"spec\":{\"held\":false}}"
                         :check {:description "Widget alpha/a is released" :timeout 60 :interval 3}}]]]
      (testing (str "wanted " wanted)
        (with-redefs [shell/execute (cluster objects)]
          (is (= (:candidates expected) ((-> spec :select :candidates) "ctx")) "the unrefused objects, sorted")
          (is (= {:mode :one :label "Widget" :flag :widget} (dissoc (:select spec) :candidates)))
          (is (= (:empty expected) (:empty-message spec)))
          (is (= {:action (:action expected)} (:confirm spec)))
          (is (= (:items expected) ((:items spec) (assoc ctx :object (widget "alpha" "a" false)))))
          (is (= (:fallback expected) ((:items spec) ctx)) "no object read: the verb and the pick")
          (is (= (:refusal expected) ((:refusal spec) {:object (widget "x" "y" wanted)})))
          (is (nil? ((:refusal spec) {:object (widget "x" "y" (not wanted))})))
          (is (= (:announce expected) ((:announce spec) ctx)))
          (is (= [[:kubectl "ctx" "--namespace=alpha" "patch" widget-type "a" "--type=merge" (:patch expected)]]
                 ((:effect spec) ctx)))
          (let [[check & more] ((:verify spec) ctx)]
            (is (nil? more))
            (is (= (:check expected) (dissoc check :check)))
            (is (= (not wanted) ((:check check))) "widget alpha/a is not held")))))))

(deftest a-pick-that-does-not-parse-still-names-the-type-in-the-plan
  (let [{:keys [on]} (toggle/pair declaration)]
    (is (= [[:kubectl "ctx" "--namespace=" "patch" widget-type nil "--type=merge" "--patch={\"spec\":{\"held\":true}}"]]
           ((:effect on) {:context "ctx" :target "a" :ref nil})))))

(deftest declared-candidates-and-effect-take-wanted
  (let [{:keys [on off]} (toggle/pair (-> declaration
                                          (dissoc :patch)
                                          (assoc :candidates (fn [context wanted] [context wanted])
                                                 :effect (fn [{:keys [target]} wanted] [:effect target wanted]))))]
    (is (= ["ctx" true] ((-> on :select :candidates) "ctx")))
    (is (= ["ctx" false] ((-> off :select :candidates) "ctx")))
    (is (= [:effect "alpha/a" false] ((:effect off) {:target "alpha/a"})))))

(deftest a-side-keeps-the-shared-spec
  (let [{:keys [on]} (toggle/pair (assoc-in declaration [:spec :derive] ::derive))]
    (is (= ::derive (:derive on)))
    (is (= {:type widget-type} (:subject on)))
    (is (str/includes? (-> on :options :widget :desc) "namespace"))))
