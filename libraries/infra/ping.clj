;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.ping
  "Reachability checks over ICMP, for IPv4 and IPv6."
  (:require [infra.shell :as shell]))

(defn reachable?
  "Whether a host answers a single ping over the given IP protocol version (4 or
   6). Bounded by a short deadline so an unreachable host fails fast: the
   Prerequisites phase must not stall on a dead network. The deadline kills ping
   regardless of which platform's flags are in play, so an unroutable host reads
   as unreachable rather than hanging."
  [protocol-version host]
  (binding [shell/*timeout-ms* 8000]
    (zero? (shell/exit-code-of "ping" (str "-" protocol-version) "-c" "1" "-q" host))))
