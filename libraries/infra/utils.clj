;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.utils
  "Small collection helpers shared by the other libraries.")

(defn conj-if-not-empty
  [collection values add]
  (if (and (not-empty values) (not (some #(= % add) collection)))
    (conj collection add)
    collection))

(defn collection-contains?
  [collection value]
  (some #(= value %) collection))
