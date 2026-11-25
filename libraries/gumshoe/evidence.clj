;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.evidence
  "Questions about an evidence map that detect functions ask before they
   judge. It depends on nothing else in gumshoe, so every detective namespace
   can require it without reaching the registry.")

(defn joined?
  "Whether every read a check joins answered. A read that did not answer is
   nil in the evidence; an empty list is an answer. A check that correlates
   one read against another judges nothing unless this holds, because the
   missing side would read as an object that does not exist."
  [evidence reads]
  (every? #(some? (get evidence %)) reads))

(defn consulted?
  "Whether any read a check consults answered. A check that judges against
   several reads of which any one is enough - each judged only when it
   answered - has nothing to judge against when none did. An empty list is an
   answer."
  [evidence reads]
  (boolean (some #(some? (get evidence %)) reads)))
