;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ui.charm-test
  "The charm backend fed the bytes a terminal sends. Each test types bytes
   into a JLine terminal in raw mode, reads them back with charm's own input
   reader and turns each event into a message the way charm.program's input
   loop does, so a test cannot pass on a message no terminal produces."
  (:require [charm.input.handler :as handler]
            [charm.input.keymap :as keymap]
            [charm.message :as msg]
            [charm.program :as program]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.ui.charm :as charm])
  (:import [java.io ByteArrayOutputStream PipedInputStream PipedOutputStream]
           [org.jline.terminal TerminalBuilder]))

(def ^:private key-bytes
  "What a terminal in raw mode sends for each key the tests press."
  {:space " " :enter "\r" :escape "\u001b" :ctrl+c "\u0003"
   :up "\u001b[A" :down "\u001b[B" :home "\u001b[H" :end "\u001b[F"})

(defn- event->message
  "The message charm.program's input loop makes of a keyboard event: the
   runes of a :runes event as the key, otherwise its :type."
  [event]
  (msg/key-press (if (= :runes (:type event)) (:runes event) (:type event))
                 :ctrl (:ctrl event) :alt (:alt event) :shift (:shift event)))

(defn- typed
  "The messages charm reads from a terminal on which keys were typed, each
   key a keyword of key-bytes or a string sent as it is."
  [& keys]
  (let [out (PipedOutputStream.)
        in (PipedInputStream. out 4096)
        terminal (-> (TerminalBuilder/builder)
                     (.system false)
                     (.streams in (ByteArrayOutputStream.))
                     (.type "xterm")
                     (.build))]
    (try
      ;; raw mode first: in cooked mode JLine consumes Ctrl+C as a signal
      (.enterRawMode terminal)
      (.write out (.getBytes ^String (str/join (map #(get key-bytes % %) keys)) "UTF-8"))
      (.flush out)
      (let [keys-map (keymap/create-keymap terminal)]
        (->> (repeatedly #(handler/read-event terminal :timeout-ms 200 :keymap keys-map))
             (take-while some?)
             (mapv event->message)))
      (finally (.close terminal)))))

(deftest typed-keys-arrive-as-the-messages-the-backend-matches
  (is (= [" "] (mapv :key (typed :space))) "Space arrives as the key \" \", not :space")
  (is (not (msg/key-match? (first (typed :space)) :space)))
  (is (= [:enter :escape] (mapv :key (concat (typed :enter) (typed :escape)))))
  (is (msg/key-match? (first (typed :ctrl+c)) "ctrl+c"))
  (is (= [:up :down :home :end] (mapv :key (typed :up :down :home :end)))))

(defn- play
  "The final state of a program started at `state` and fed `keys` through
   charm/step with `component-update`, stopping at the first key that quits,
   as program/run does."
  [component-update state keys]
  (reduce (fn [s k]
            (let [[s' command] (charm/step component-update s k)]
              (if (= program/quit-cmd command) (reduced s') s')))
          state keys))

(deftest a-bare-enter-at-the-confirmation-declines
  (is (= "no" (first charm/confirm-choices)))
  (is (false? (charm/confirmed? (play charm/list-update (charm/list-state charm/confirm-choices)
                                      (typed :enter))))))

(deftest the-confirmation-approves-only-an-explicit-yes
  (is (true? (charm/confirmed? (play charm/list-update (charm/list-state charm/confirm-choices)
                                     (typed :down :enter)))))
  (testing "a cancel after moving to yes declines"
    (is (false? (charm/confirmed? (play charm/list-update (charm/list-state charm/confirm-choices)
                                        (typed :down :escape)))))
    (is (false? (charm/confirmed? (play charm/list-update (charm/list-state charm/confirm-choices)
                                        (typed :down :ctrl+c)))))))

(def ^:private destructive-options
  {:prompt "Proceed?" :affirmative "Yes, delete 34 fdb rows" :negative "No" :default false :destructive? true})

(deftest a-labelled-confirmation-starts-on-the-negative
  (let [choices (charm/option-choices destructive-options)]
    (is (= ["No" "Yes, delete 34 fdb rows"] choices))
    (is (false? (charm/chose-affirmative? choices (play charm/list-update (charm/list-state choices)
                                                        (typed :enter)))))))

(deftest a-labelled-confirmation-approves-only-the-affirmative
  (let [choices (charm/option-choices destructive-options)]
    (is (true? (charm/chose-affirmative? choices (play charm/list-update (charm/list-state choices)
                                                       (typed :down :enter)))))
    (is (false? (charm/chose-affirmative? choices (play charm/list-update (charm/list-state choices)
                                                        (typed :down :escape)))))
    (is (false? (charm/chose-affirmative? choices (play charm/list-update (charm/list-state choices)
                                                        (typed :down :ctrl+c)))))))

(deftest labels-that-cannot-be-told-apart-give-no-choices
  (doseq [options [{:affirmative "No" :negative "No"}
                   {:affirmative " Yes " :negative "Yes"}
                   {:affirmative "" :negative "No"}
                   {:affirmative "Yes, delete" :negative nil}
                   {}]]
    (is (nil? (charm/option-choices options)) (pr-str options))
    (is (false? (charm/confirm options)) "declines without starting a picker")))

(deftest the-backend-declares-the-options-contract
  (is (true? (get-in @@#'gumshoe.ui/providers [:charm :confirm-options?]))))

(deftest a-pick-returns-the-value-under-the-cursor
  (let [state (charm/list-state ["a" "b" "c"])]
    (is (= "a" (charm/picked (play charm/list-update state (typed :enter)))))
    (is (= "c" (charm/picked (play charm/list-update state (typed :down "j" :enter)))))
    (is (= "b" (charm/picked (play charm/list-update state (typed :end :up :enter)))))
    (is (nil? (charm/picked (play charm/list-update state (typed :down :escape)))))
    (testing "keys after Enter are not read"
      (is (= "a" (charm/picked (play charm/list-update state (typed :enter :down))))))))

(deftest a-multi-pick-returns-the-marked-values-in-list-order
  (let [state (charm/multi-state ["a" "b" "c"])]
    (is (= [] (charm/picked-many (play charm/multi-update state (typed :enter)))))
    (is (= ["a" "c"] (charm/picked-many (play charm/multi-update state
                                               (typed :end :space :home :space :enter)))))
    (is (= ["b"] (charm/picked-many (play charm/multi-update state
                                           (typed :down :space :down :space :space :enter))))
        "a second Space unmarks")
    (is (nil? (charm/picked-many (play charm/multi-update state (typed :space :escape)))))))

(deftest the-multi-view-marks-each-value
  (let [view (charm/multi-view (play charm/multi-update (charm/multi-state ["a" "b"]) (typed :space)))]
    (is (re-find #"\[x\] a" view))
    (is (re-find #"\[ \] b" view))))

(deftest text-is-read-from-the-input
  (let [state (reduce charm/text-update (charm/text-state) (typed "h" "i"))]
    (is (= "hi" (charm/entered state)))
    (is (nil? (charm/entered (assoc state charm/cancelled true))))
    (is (nil? (charm/entered (charm/text-state))) "an empty input is no answer")))

(deftest only-keypresses-reach-the-component
  (let [seen (atom [])
        update (fn [s m] (swap! seen conj m) s)]
    (charm/step update {} (msg/window-size 80 24))
    (charm/step update {} (first (typed "x")))
    (is (= (typed "x") @seen))))
