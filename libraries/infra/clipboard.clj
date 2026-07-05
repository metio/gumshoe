;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.clipboard
  "Copying text to the system clipboard with whatever tool the platform has -
   wl-copy on Wayland, xclip/xsel on X11, pbcopy on macOS."
  (:require [infra.command :as command]
            [infra.shell :as shell]))

(def ^:private tools
  "Preference order of clipboard tools, each with the argv that reads stdin."
  [["wl-copy" ["wl-copy"]]
   ["xclip" ["xclip" "-selection" "clipboard"]]
   ["xsel" ["xsel" "--clipboard" "--input"]]
   ["pbcopy" ["pbcopy"]]])

(defn available?
  "Whether any clipboard tool is installed."
  []
  (some (fn [[tool _]] (command/installed? tool)) tools))

(defn copy!
  "Copies content to the clipboard, returning the name of the tool that did it,
   or nil when no clipboard tool is installed or the copy failed. Best-effort -
   copying is a convenience, never a reason for a book to fail."
  [content]
  (some (fn [[tool argv]]
          (when (and (command/installed? tool)
                     (zero? (apply shell/pipe-to! content argv)))
            tool))
        tools))
