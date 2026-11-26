;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.flags
  "The short aliases of the flags a select reads, decided once for every
   book, and the aliases those flags answered to before.

   A flag has one short alias wherever it appears, so an operator who learnt
   `-N` on one book can use it on the next, and a playbook can join steps
   from different tools without two steps disagreeing on an alias.

   A deprecated alias keeps working where its option exists and no option of
   the book claims the letter for itself. A run that uses one prints a note
   naming the alias that replaces it, and --help lists it beside the option."
  (:require [babashka.cli :as cli]
            [clojure.string :as str]))

(def aliases
  "The short alias of each select flag, by the option it names."
  {:application :a
   :backup :b
   :certificates :c
   :cluster :c
   :instance :i
   :job :j
   :name :N
   :namespace :n
   :node :n
   :orders :o
   :osd :d
   :pick :p
   :pod :p
   :pooler :p
   :prometheus :p
   :pvc :p
   :release :r
   :schedule :s
   :scheduled-backup :s
   :service :s
   :statefulset :s
   :volumes :v
   :vtep :V})

(def deprecated
  "The short aliases an option answered to before the convention, by option."
  {:name [:r :s]
   :service [:m]})

;; --- option order -------------------------------------------------------------
;;
;; --help lists a book's options in the order of its option map's keys. A map
;; literal of more than eight entries is read as a hash map, and assoc'ing a
;; ninth key onto an array map turns it into one, so either loses the order
;; the options were written in. These functions build array maps of any size
;; instead, which keep their insertion order as long as no new key is
;; assoc'ed onto them.

(defn merge-ordered-with
  "Like merge-with, keeping the keys in order of first appearance: the keys
   of the first map in its order, then each later map's new keys in theirs.
   A key a later map repeats keeps its first position and takes
   (f earlier later). nil maps are skipped. Returns an array map, nil when
   every map is nil."
  [f & maps]
  (when (some some? maps)
    (let [[order values] (reduce (fn [[order values] [k v]]
                                   (if (contains? values k)
                                     [order (assoc values k (f (get values k) v))]
                                     [(conj order k) (assoc values k v)]))
                                 [[] {}]
                                 (mapcat seq maps))]
      (apply array-map (mapcat (juxt identity values) order)))))

(defn merge-ordered
  "Like merge, keeping the keys in order of first appearance (see
   merge-ordered-with): a key a later map repeats keeps its first position
   and takes the later value."
  [& maps]
  (apply merge-ordered-with (fn [_ later] later) maps))

(defn ordered
  "An option map that keeps the order its options are written in, at any
   size, from flag keyword and option map pairs:

     (flags/ordered :url {:desc ..} :flavor {:desc ..} ..)

   A repeated key keeps its first position and takes the later option. Use
   it for a book with more than eight options, whose map literal would lose
   the order."
  [& kvs]
  (when (odd? (count kvs))
    (throw (ex-info "flags/ordered takes flag and option pairs" {:kvs kvs})))
  (or (apply merge-ordered (map (fn [[k v]] {k v}) (partition 2 kvs))) {}))

(defn order-lost?
  "True when an option map cannot carry the order its options were written
   in: a hash map, as a map literal of more than eight entries or a plain
   merge past eight becomes. An array map keeps its order and a sorted map
   orders by name on purpose."
  [options]
  (and (map? options)
       (not (sorted? options))
       (not (instance? clojure.lang.PersistentArrayMap options))))

(defn- flag
  [letter]
  (str "-" (name letter)))

(defn deprecated-aliases
  "{alias option} for every deprecated alias these options still take: its
   option is among them, and no option declares the alias as its own."
  [options]
  (let [claimed (set (keep :alias (vals options)))]
    (into (sorted-map)
          (for [[option letters] deprecated
                :when (contains? options option)
                letter letters
                :when (not (claimed letter))]
            [letter option]))))

(defn documented
  "The options, each one that still takes deprecated aliases naming them at
   the end of its :desc, so --help lists them. The options keep their order."
  [options]
  (let [by-option (group-by val (deprecated-aliases options))]
    (reduce-kv (fn [options option pairs]
                 (update-in options [option :desc]
                            #(str % (when % " ") "(deprecated alias: "
                                  (str/join ", " (map (comp flag key) pairs)) ")")))
               options
               by-option)))

(defn notes
  "One line for each deprecated alias the arguments use, naming the alias or
   flag that replaces it. Arguments after `--` are values, never flags. A
   value that itself starts with a deprecated alias, as in `--grep -s`, also
   reads as a use; the note is then spurious, and the parse is unaffected."
  [options args]
  (let [taken (deprecated-aliases options)
        used (distinct (for [arg (take-while #(not= "--" %) args)
                             :let [[_ letter] (re-matches #"-([A-Za-z]).*" arg)]
                             :when letter
                             :let [option (taken (keyword letter))]
                             :when option]
                         [(keyword letter) option]))]
    (vec (for [[letter option] used
               :let [current (get-in options [option :alias])]]
           (format "%s is a deprecated alias of --%s; use %s"
                   (flag letter) (name option)
                   (if current (flag current) (str "--" (name option))))))))

(defn parse-without-positionals
  "babashka.cli's parse of args with spec, refusing the first positional
   argument. babashka.cli stops reading options at the first token that is
   not one, so every flag after a stray word, a typo or a value given to a
   boolean flag would be dropped silently: a --dry-run behind it would turn
   a rehearsal into a real change, an --output json behind it a text report.
   The refusal goes to the spec's :error-fn as babashka.cli hands its own
   over, with cause :unexpected-argument, and throws when there is none or
   it returns, so the half-read options are never handed on. `takes` names
   what the command accepts, for the message."
  [args spec takes]
  (let [{:keys [args] :as parsed} (cli/parse-args args spec)]
    (if-let [arg (first args)]
      (let [data {:type :org.babashka/cli
                  :cause :unexpected-argument
                  :msg (format (str "unexpected argument: %s - %s takes only options, and nothing after it was read;"
                                    " a boolean flag such as --dry-run takes no value (write --dry-run=false to turn one off)")
                               (pr-str arg) takes)
                  :argument arg}]
        (when-let [error-fn (:error-fn spec)] (error-fn data))
        (throw (ex-info (:msg data) data)))
      (:opts parsed))))

(defn parse-with-search-terms
  "babashka.cli's parse of args with spec into {:opts :args}, where the
   positional arguments are search terms, refusing an option among them.
   babashka.cli stops reading options at the first search term that follows
   an option, so `--no-color drain --dry-run` would hand --dry-run on as a
   search term: the book chosen by the search would then run without it. A
   positional that starts with \"-\" is therefore refused instead of searched
   for, through the spec's :error-fn with cause :option-after-search-term,
   and throws when there is none or it returns. `takes` names the command,
   for the message."
  [args spec takes]
  (let [{:keys [args] :as parsed} (cli/parse-args args spec)]
    (if-let [arg (first (filter #(str/starts-with? % "-") args))]
      (let [data {:type :org.babashka/cli
                  :cause :option-after-search-term
                  :msg (format (str "%s was not read: an option after a search term is taken for one;"
                                    " put every option of %s before the search terms")
                               (pr-str arg) takes)
                  :argument arg}]
        (when-let [error-fn (:error-fn spec)] (error-fn data))
        (throw (ex-info (:msg data) data)))
      (select-keys parsed [:opts :args]))))
