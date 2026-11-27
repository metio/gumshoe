;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.evidence
  "Questions about an evidence map that detect functions ask before they
   judge. It depends on nothing in gumshoe but gumshoe.reads.result, which
   depends on nothing, so every detective namespace can require it without
   reaching the registry.

   A read that did not answer is nil in the evidence, or a three-state
   result of gumshoe.reads.result in the :unread state; missing? holds for
   both. An empty list and an :absent result are answers."
  (:require [gumshoe.reads.result :as result]))

(defn missing?
  "Whether an evidence value is a read that did not answer: nil, or a
   three-state result that is :unread. Any other value answered, an empty
   list and an :absent result included."
  [v]
  (or (nil? v) (and (result/result? v) (result/unread? v))))

(defn joined?
  "Whether every read a check joins answered. A read that did not answer is
   nil in the evidence; an empty list is an answer. A check that correlates
   one read against another judges nothing unless this holds, because the
   missing side would read as an object that does not exist. An :unread
   result counts as a read that did not answer."
  [evidence reads]
  (not-any? #(missing? (get evidence %)) reads))

(defn consulted?
  "Whether any read a check consults answered. A check that judges against
   several reads of which any one is enough - each judged only when it
   answered - has nothing to judge against when none did. An empty list is an
   answer."
  [evidence reads]
  (boolean (some #(not (missing? (get evidence %))) reads)))
