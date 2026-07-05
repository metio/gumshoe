;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns casebook.hello.example
  "A worked example of a casebook detective. Copy its shape for your own read-only
   checks; for a book that changes something, use infra.runbook/execute! or the
   infra.mutation/book helper instead. Run it with: bb runbooks/hello/example.clj"
  (:require [infra.detective :as detective]))

(detective/book
 {:description "Example casebook check - always clean, a starting point to copy"
  :when-to-run "Reach for this to see the shape of a casebook detective end to end."
  :prerequisites {}
  :detectives [{:name "example"
                :description "An example check that never complains"
                :requires []
                :detect (fn [_evidence] [])}]})
