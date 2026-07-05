;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.net
  "Small local-network facts. The one the books use: is a given interface up,
   which stands in for 'am I on that VPN'. wg-quick and most VPN tooling only
   create the interface while the tunnel is up, so its presence is a good,
   generic signal - and the interface name stays out of the code, in env.edn."
  (:require [clojure.string :as str]
            [infra.shell :as shell]))

(defn interface-up?
  "True when the named interface is present. A blank/nil interface means there
   is no gate to apply, so it is treated as up."
  [interface]
  (or (str/blank? (str interface))
      (zero? (shell/exit-code-of "ip" "-o" "link" "show" (str interface)))))

(defn vpn-like?
  "Names that look like a VPN tunnel by convention."
  [interface]
  (boolean (re-find #"^(wg|tun|tap|wireguard|vpn|tailscale|nebula)" (str interface))))

(defn parse-interface-names
  "Interface names out of 'ip -o link show' output."
  [ip-output]
  (->> (str/split-lines (str ip-output))
       (keep #(second (re-find #"^\d+:\s+([^:@\s]+)" %)))
       vec))

(defn vpn-candidates
  "Best-effort list of VPN-looking interfaces on this host: WireGuard tunnels
   plus anything named like a tunnel."
  []
  (let [wireguard (some-> (not-empty (shell/stdout-of "wg" "show" "interfaces"))
                          (str/split #"\s+"))
        tunnels (filter vpn-like? (parse-interface-names (shell/stdout-of "ip" "-o" "link" "show")))]
    (vec (distinct (concat wireguard tunnels)))))
