;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tally
  "Aggregation for detectives whose evidence repeats one thing many times: a
  path denied once per packet, an fdb entry printed once per row. A detective
  that reports each repetition buries the few distinct things under copies an
  operator cannot tell apart, so it groups by what the finding names and says
  how many it saw.")

(defn counted
  "\"1 denied flow\" or \"3 denied flows\": n with the noun that agrees with it."
  [n singular plural]
  (str n " " (if (= 1 n) singular plural)))

(defn tally
  "Groups xs by (key-fn x), in the order each key was first seen, as maps of
  :key, :items and :count. The order is the evidence's own, so the findings
  built from it read in a stable order however many groups there are, which
  group-by does not promise once its map outgrows an array map."
  [key-fn xs]
  (let [{:keys [order groups]}
        (reduce (fn [acc x]
                  (let [k (key-fn x)]
                    (cond-> (update-in acc [:groups k] (fnil conj []) x)
                      (not (contains? (:groups acc) k)) (update :order conj k))))
                {:order [] :groups {}}
                xs)]
    (for [k order
          :let [items (get groups k)]]
      {:key k :items items :count (count items)})))
