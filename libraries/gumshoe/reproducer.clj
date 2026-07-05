;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.reproducer
  "A reproducer is the exact sequence of CLI calls a read-only book made,
   written to a throwaway shell script so the operator can regenerate the same
   data by hand - or hand it to someone else. When a clipboard tool is present
   (wl-copy on Wayland, xclip/xsel on X11, pbcopy on macOS) the run command is
   copied automatically."
  (:require [clojure.string :as str]
            [gumshoe.clipboard :as clipboard]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(defn script-content
  "Pure rendering of the reproducer script from a description and commands."
  [description commands]
  (str "#!/usr/bin/env sh\n"
       "# Reproducer for: " description "\n"
       "# The exact CLI calls behind the report you just saw - review before running.\n\n"
       (str/join "\n" commands)
       "\n"))

(defn- write-script!
  [description commands]
  (let [file (java.io.File/createTempFile "bookstore-reproducer-" ".sh")]
    (spit file (script-content description commands))
    (str file)))

(defn offer!
  "Writes a reproducer for the commands run so far and points the operator at
   it, copying the run command to the clipboard when possible. Never throws -
   a reproducer is a convenience, never a reason for a book to fail."
  [description]
  (try
    (let [commands (shell/recording)]
      (when (seq commands)
        (let [content (script-content description commands)
              path (write-script! description commands)]
          (stdout/print-section-marker)
          (stdout/err-println (format "📋 Reproducer: %d commands saved to %s" (count commands) path))
          ;; put the script's contents on the clipboard, not a `sh <tmpfile>`
          ;; invocation - the contents are self-contained and paste anywhere
          ;; (an editor, a pad, a colleague's chat), while the temp file may be
          ;; cleaned up from under you.
          (if-let [tool (clipboard/copy! content)]
            (stdout/err-println (format "   its commands are on your clipboard (%s) - paste to review or share, or run: sh %s"
                                        tool path))
            (stdout/err-println (format "   run it with: sh %s" path))))))
    (catch Exception _ nil)))
