;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.extensions
  "Integrating extension repos cloned alongside gumshoe - casebooks, tool
   packages, plugin bundles - as first-class peers of the built-ins. A third-party
   repo reaches every seam the core offers, and does it with pure babashka: no
   dependency resolution, no JVM.

   An extension is a directory with a `gumshoe.edn` manifest at its root:

     {:paths [\"libraries\"]                 ; code to add to the classpath
      :book-paths [\"runbooks\" \"playbooks\"]  ; where its books live
      :plugins [acme.ceph.detectors          ; namespaces to load, which register
                acme.announce.irc]}          ; announcers/detectives/capabilities/...

   env.edn lists the cloned roots:

     :extensions [\"../casebook-infra-run\" \"../gumshoe-community-ceph\"]

   Activating an extension puts its code on the classpath and loads its plugins,
   so its detectives, announcers, capability detectors, tool support, and books
   all light up exactly as an official package's do. Nothing gumshoe ships names
   any single operator - infra.run is just one more extension repo."
  (:require [babashka.classpath :as classpath]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [infra.config :as config]
            [infra.shell :as shell]
            [infra.stdout :as stdout]))

(defn roots
  "The extension repo roots configured in env.edn :extensions."
  []
  (config/value [:extensions] []))

(defn- manifest
  "The gumshoe.edn manifest at an extension root, or nil when absent/unreadable."
  [root]
  (let [file (fs/path root "gumshoe.edn")]
    (when (fs/exists? file)
      (try (edn/read-string (slurp (str file)))
           (catch Exception _ nil)))))

(defn- resolve-existing
  "The manifest's relative dirs turned into paths under the root, keeping only
   those that exist (a stale entry is skipped, not fatal)."
  [root paths]
  (->> paths
       (map #(str (fs/path root %)))
       (filter fs/exists?)))

(defn code-dirs
  "Every extension's :paths (code) dirs, resolved and existing."
  ([] (code-dirs (roots)))
  ([roots] (mapcat #(resolve-existing % (:paths (manifest %))) roots)))

(defn book-paths
  "Every extension's :book-paths (book) dirs, resolved and existing - added to
   book discovery alongside the built-ins."
  ([] (book-paths (roots)))
  ([roots] (mapcat #(resolve-existing % (:book-paths (manifest %))) roots)))

(defn classpath-string
  "The classpath for launching a book as a subprocess: gumshoe's own plus every
   extension's code dirs, so an extension's book can require the extension's own
   shared code. nil when no extensions add code, so callers fall back to the
   plain bb.edn classpath. BABASHKA_CLASSPATH replaces bb.edn :paths, so gumshoe's
   own classpath is included explicitly."
  ([] (classpath-string (roots)))
  ([roots]
   (let [dirs (code-dirs roots)]
     (when (seq dirs)
       (str/join (System/getProperty "path.separator")
                 (cons (classpath/get-classpath) dirs))))))

(defn launch-book!
  "Runs a book as a subprocess with the classpath extended to include every
   configured extension, so an extension's book can require the extension's own
   shared code. With no extensions, a plain streaming run, so bb.edn :paths apply."
  [& args]
  (if-let [cp (classpath-string)]
    (apply shell/run-with-output-env {"BABASHKA_CLASSPATH" cp} args)
    (apply shell/run-with-output args)))

(defn activate!
  "Adds every extension's code to the classpath (in this process) and returns the
   plugin namespaces they declare, for the loader to require. Best-effort: a
   missing manifest is warned about, never fatal."
  ([] (activate! (roots)))
  ([roots]
   (vec
    (mapcat
     (fn [root]
       (if-let [m (manifest root)]
         (do (doseq [dir (resolve-existing root (:paths m))]
               (classpath/add-classpath dir))
             (:plugins m))
         (do (stdout/warn (format "extension %s: no gumshoe.edn manifest - skipping" root))
             nil)))
     roots))))
