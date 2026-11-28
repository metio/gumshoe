;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.edn-book-test
  "Books written as EDN: the reader refuses everything but one bounded map
   of plain EDN, the loader refuses what compile refuses with the file
   named, the catalog, the run door and conformance treat an .edn file as
   a book, and a fresh bb runs one exactly as it runs the Clojure book it
   mirrors."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.catalog :as catalog]
            [gumshoe.conformance :as conformance]
            [gumshoe.dsl :as dsl]
            [gumshoe.edn-book :as edn-book]
            [gumshoe.edn-reader :as reader]
            [gumshoe.main :as main]
            [gumshoe.shell :as shell]))

(def ^:private suspend-edn "tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn")
(def ^:private suspend-clj "runbooks/kubernetes/cronjobs/suspend.clj")

(def ^:private fixtures
  ["tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn"
   "tests/edn-books/runbooks/kubernetes/cronjobs/resume.edn"
   "tests/edn-books/runbooks/kubernetes/workloads/scale.edn"
   "tests/edn-books/runbooks/kubernetes/objects/delete.edn"])

(def ^:private header
  (str ";; SPDX-" "FileCopyrightText: The gumshoe Authors\n;; SPDX-" "License-Identifier: 0BSD\n"))

(defn- refusal
  "The reason the reader or loader gives for refusing the file, or
   :accepted."
  [f file]
  (try (f file) :accepted
       (catch clojure.lang.ExceptionInfo e
         (is (reader/refused? e) (ex-message e))
         (is (= (str file) (:file (ex-data e))))
         (is (str/starts-with? (ex-message e) (str file ": ")) "the message names the file first")
         (:reason (ex-data e)))))

(defn- spit-bytes
  [file content]
  (if (string? content)
    (spit (str file) content)
    (fs/write-bytes file content))
  (str file))

(defn- nested
  [depth]
  (str "{:a " (apply str (repeat (dec depth) "[")) (apply str (repeat (dec depth) "]")) "}"))

;; --- the reader --------------------------------------------------------------

(def ^:private reader-refusals
  "[what the file holds, its content, a word the reason holds]."
  [["#= read-eval" "{:gumshoe/dsl :v0-experimental :description #=(java.lang.System/exit 3)}" "is not plain EDN"]
   ["#= read-eval at the top" "#=(java.lang.Runtime/getRuntime)" "is not plain EDN"]
   ["an #inst tagged literal" "{:at #inst \"2026-11-25\"}" "tagged literal #inst"]
   ["a #uuid tagged literal" "{:id #uuid \"00000000-0000-0000-0000-000000000000\"}" "tagged literal #uuid"]
   ["an unknown tagged literal" "{:x #gumshoe/run \"rm -rf\"}" "tagged literal #gumshoe/run"]
   ["a nested tagged literal" "{:x [{:y #js {}}]}" "tagged literal #js"]
   ["a reader conditional" "{:x #?(:clj 1)}" "is not plain EDN"]
   ["a regex" "{:x #\"a.*\"}" "is not plain EDN"]
   ["an anonymous function" "{:x #(System/exit 1)}" "is not plain EDN"]
   ["syntax quote" "{:x `(System/exit 1)}" "is not plain EDN"]
   ["a deref" "{:x @(System/exit 1)}" "is not plain EDN"]
   ["a var quote" "{:x #'clojure.core/eval}" "is not plain EDN"]
   ["a duplicate key" "{:a 1 :a 2}" "is not plain EDN"]
   ["an unclosed map" "{:a 1" "is not plain EDN"]
   ["nothing" "" "holds no form"]
   ["only comments" ";; a comment\n#_{:a 1}\n" "holds no form"]
   ["two forms" "{:a 1} {:b 2}" "holds more than one form"]
   ["a map and trailing garbage" "{:a 1} )" "is not plain EDN"]
   ["a vector" "[:gumshoe/dsl :v0-experimental]" "holds a vector, not a map"]
   ["a list" "(System/exit 1)" "holds a list, not a map"]
   ["a set" "#{1}" "holds a set, not a map"]
   ["a symbol" "eval" "holds eval, not a map"]
   ["one collection too deep" (nested (inc reader/max-depth)) (str "nests deeper than " reader/max-depth)]
   ;; refused by the depth check, or as too deep to read where the reader
   ;; runs out of stack first, which depends on the thread's stack size
   ["far too deep" (nested 20000) "nests"]
   ["one byte too many" (str "{:a \"" (apply str (repeat (- (inc reader/max-bytes) 7) "x")) "\"}") "larger than 65536 bytes"]
   ["a large file" (apply str (repeat (* 4 reader/max-bytes) " ")) "larger than 65536 bytes"]
   ["invalid UTF-8" (byte-array (map unchecked-byte [0x7b 0x3a 0x61 0x20 0x22 0xff 0xfe 0x22 0x7d])) "not valid UTF-8"]])

(deftest the-reader-refuses-all-but-one-bounded-map-of-plain-edn
  (fs/with-temp-dir [dir {}]
    (doseq [[what content expected] reader-refusals]
      (testing what
        (let [file (spit-bytes (fs/path dir "book.edn") content)
              reason (refusal reader/read-book file)]
          (is (string? reason))
          (is (str/includes? (str reason) expected) (str reason)))))))

(deftest the-reader-refuses-a-path-that-is-no-file
  (fs/with-temp-dir [dir {}]
    (is (= "is not a file" (refusal reader/read-book (str dir))))
    (is (= "is not a file" (refusal reader/read-book (str (fs/path dir "missing.edn")))))))

(deftest the-reader-refuses-a-file-it-cannot-read
  (fs/with-temp-dir [dir {}]
    (let [file (spit-bytes (fs/path dir "locked.edn") "{:description \"x\"}")]
      (testing "an IOException opening the file is a refusal, never a raw exception"
        (with-redefs [clojure.java.io/input-stream
                      (fn [& _] (throw (java.io.FileNotFoundException. (str file " (Permission denied)"))))]
          (is (= (str "cannot be read: " file " (Permission denied)") (refusal reader/read-book file)))))
      (testing "a mode-000 file, where the user cannot read it (root, or a user namespace, can)"
        (fs/set-posix-file-permissions file "---------")
        (try
          (when-not (fs/readable? file)
            (is (str/starts-with? (refusal reader/read-book file) "cannot be read: ")))
          (finally (fs/set-posix-file-permissions file "rw-------")))))))

(deftest the-reader-accepts-what-sits-at-its-bounds
  (fs/with-temp-dir [dir {}]
    (testing "exactly max-bytes"
      (let [content (str "{:a \"" (apply str (repeat (- reader/max-bytes 7) "x")) "\"}")
            file (spit-bytes (fs/path dir "max.edn") content)]
        (is (= reader/max-bytes (count (.getBytes content "UTF-8"))))
        (is (= :accepted (refusal reader/read-book file)))))
    (testing "exactly max-depth"
      (is (= :accepted (refusal reader/read-book (spit-bytes (fs/path dir "deep.edn") (nested reader/max-depth))))))
    (testing "comments, discards, namespaced maps and non-ASCII text"
      (is (= {:gumshoe/dsl :v0-experimental :description "Fährt hoch" :x #:a{:b 1}}
             (reader/read-book (spit-bytes (fs/path dir "plain.edn")
                                           (str header ";; c\n#_ignored\n{:gumshoe/dsl :v0-experimental :description \"Fährt hoch\" :x #:a{:b 1}}"))))))))

(deftest a-refused-tag-never-runs-a-reader
  (fs/with-temp-dir [dir {}]
    (let [ran (atom false)
          file (spit-bytes (fs/path dir "book.edn") "{:x #inst \"2026-11-25\"}")]
      (binding [*data-readers* {'inst (fn [_] (reset! ran true))}]
        (is (string? (refusal reader/read-book file))))
      (is (false? @ran) "a reader the caller binds is not consulted"))))

(deftest every-fixture-reads-and-stays-within-the-bounds
  (doseq [file fixtures]
    (testing file
      (is (map? (reader/read-book file)))
      (is (< (fs/size file) (quot reader/max-bytes 8)) "the bounds leave room for books much larger than these"))))

;; --- the loader ----------------------------------------------------------------

(defn- with-source
  [f]
  (str header (pr-str (f (reader/read-book suspend-edn)))))

(def ^:private loader-refusals
  "[what is wrong, the file's content, the start of the reason]."
  [["no version marker" (with-source #(dissoc % :gumshoe/dsl)) "[:gumshoe/dsl]: the change declares no version"]
   ["the number 0" (with-source #(assoc % :gumshoe/dsl 0)) "[:gumshoe/dsl]: the change declares :gumshoe/dsl 0, and this compiles :v0-experimental"]
   ["a string marker" (with-source #(assoc % :gumshoe/dsl "v0-experimental")) "[:gumshoe/dsl]: the change declares :gumshoe/dsl \"v0-experimental\""]
   ["a symbol marker" (str header "{:gumshoe/dsl v0-experimental :description \"x\"}") "[:gumshoe/dsl]: the change declares :gumshoe/dsl v0-experimental,"]
   ["a later version" (with-source #(assoc % :gumshoe/dsl :v1-experimental)) "[:gumshoe/dsl]: the change declares :gumshoe/dsl :v1-experimental"]
   ["an unknown key" (with-source #(assoc % :steps [])) "[:steps]: :steps is not a key of a change"]
   ["an unknown target key" (with-source #(assoc-in % [:target :command] "rm")) "[:target :command]: :command is not a key"]
   ["a symbol where a keyword is required"
    (str/replace (with-source identity) ":gumshoe.workloads/suspend-items" "gumshoe.workloads/suspend-items")
    "[:items 1]: gumshoe.workloads/suspend-items is a symbol"]
   ["a symbol naming clojure.core/eval" (with-source #(assoc % :announce 'clojure.core/eval)) "[:announce]: clojure.core/eval is a symbol"]
   ["an unregistered function" (with-source #(assoc % :items [:call :clojure.java.shell/sh "rm" "-rf" "/"]))
    "[:items 1]: :clojure.java.shell/sh is not a registered function"]
   ["a list where data is" (str/replace (with-source identity) "[:spec :suspend]" "(:spec :suspend)") "[:state :holds 1]: a change holds vectors and maps"]])

(deftest the-loader-refuses-what-compile-refuses-naming-the-file
  (fs/with-temp-dir [dir {}]
    (doseq [[what content expected] loader-refusals]
      (testing what
        (let [file (spit-bytes (fs/path dir "book.edn") content)
              reason (refusal edn-book/load-book file)]
          (is (string? reason))
          (is (str/starts-with? (str reason) expected) (str reason)))))))

(deftest the-loader-refuses-what-the-reader-refuses
  (fs/with-temp-dir [dir {}]
    (let [file (spit-bytes (fs/path dir "book.edn") "{:gumshoe/dsl :v0-experimental :x #=(java.lang.System/exit 3)}")]
      (is (str/starts-with? (refusal edn-book/load-book file) "is not plain EDN")))))

(deftest every-fixture-loads-as-the-data-it-holds
  (doseq [file fixtures]
    (testing file
      (let [change (edn-book/load-book file)]
        (is (= (reader/read-book file) (dsl/source-of change)))
        (is (= :v0-experimental (:gumshoe/dsl (dsl/source-of change))))
        (is (fn? (:effect change)))))))

(deftest the-experimental-note-is-one-line-naming-the-file-and-the-marker
  (let [note (edn-book/experimental-note "a/b.edn")]
    (is (not (str/includes? note "\n")))
    (is (str/includes? note "a/b.edn"))
    (is (str/includes? note ":gumshoe/dsl :v0-experimental"))
    (is (str/includes? note "may change"))))

(deftest run-refuses-a-file-and-returns-1-without-running-anything
  (fs/with-temp-dir [dir {}]
    (let [file (spit-bytes (fs/path dir "book.edn") (with-source #(assoc % :gumshoe/dsl 0)))
          commands (atom [])
          err (java.io.StringWriter.)
          property (System/getProperty "babashka.file")]
      (with-redefs [shell/execute (fn [& args] (swap! commands conj args) {:exit 0 :out "" :err ""})]
        (is (= 1 (binding [*err* err] (edn-book/run file ["--dry-run"])))))
      (is (= [] @commands) "no command ran")
      (is (= property (System/getProperty "babashka.file")) "the run left the book property alone")
      (let [[note refused] (str/split-lines (str err))]
        (is (= (edn-book/experimental-note file) note))
        (is (str/includes? refused (str "refused " file ": [:gumshoe/dsl]: the change declares :gumshoe/dsl 0")))))))

;; --- the catalog and the run door ------------------------------------------------

(deftest the-catalog-lists-an-edn-book-in-a-book-root
  (fs/with-temp-dir [dir {}]
    (let [root (fs/path dir "runbooks")]
      (fs/create-dirs (fs/path root "team"))
      (fs/copy suspend-edn (fs/path root "team" "suspend.edn"))
      (spit-bytes (fs/path root "team" "broken.edn") "{:description #=(java.lang.System/exit 3)}")
      (spit-bytes (fs/path root "team" "tagged.edn") "{:description #inst \"2026-11-25\"}")
      (let [books (catalog/books [(str root)] (str dir))
            by-id (into {} (map (juxt :id identity)) books)]
        (is (= #{"runbooks/team/suspend.edn" "runbooks/team/broken.edn" "runbooks/team/tagged.edn"} (set (keys by-id))))
        (is (= "Suspends a CronJob so no new Job starts" (:description (by-id "runbooks/team/suspend.edn"))))
        (is (= "team/suspend" (:name (by-id "runbooks/team/suspend.edn"))))
        (testing "a file the reader refuses is listed by its path, and nothing in it is read"
          (is (nil? (:description (by-id "runbooks/team/broken.edn"))))
          (is (nil? (:description (by-id "runbooks/team/tagged.edn")))))
        (is (= {:path (str (fs/canonicalize (fs/path root "team" "suspend.edn")))}
               (catalog/resolve-book "runbooks/team/suspend.edn" books (str dir))))))))

(deftest the-catalog-lists-gumshoe-books-once
  (let [ids (map :id (catalog/books))]
    (is (not-any? #(str/ends-with? % ".edn") ids)
        "gumshoe's own EDN forms live outside its book roots while the format is experimental")
    (is (some #{"runbooks/kubernetes/cronjobs/suspend.clj"} ids))))

(deftest short-name-strips-the-edn-suffix
  (is (= "kubernetes/cronjobs/suspend" (catalog/short-name "runbooks/kubernetes/cronjobs/suspend.edn")))
  (is (catalog/edn-book? "x/y.edn"))
  (is (not (catalog/edn-book? "x/y.clj"))))

(deftest the-run-door-launches-an-edn-book-through-its-runner
  (is (= ["bb" "-m" "gumshoe.edn-book" "/b/runbooks/x.edn" "--dry-run"]
         (main/book-command "/b/runbooks/x.edn" ["--dry-run"])))
  (is (= ["bb" "/b/runbooks/x.clj" "--dry-run"] (main/book-command "/b/runbooks/x.clj" ["--dry-run"])))
  (let [launched (atom nil)
        catalog [{:path "/pin/runbooks/team/suspend.edn" :id "runbooks/team/suspend.edn" :name "team/suspend"
                  :description "Suspends"}]]
    (with-redefs [catalog/books (fn [& _] catalog)
                  catalog/casebook-root (fn [] "/nonexistent-casebook")
                  shell/run-with-output (fn [& args] (reset! launched (vec args)) 0)]
      (testing "bb run"
        (is (zero? (main/run ["runbooks/team/suspend.edn" "--job" "nightly"])))
        (is (= ["bb" "-m" "gumshoe.edn-book" "/pin/runbooks/team/suspend.edn" "--job" "nightly"] @launched)))
      (testing "bb gumshoe"
        (reset! launched nil)
        (is (zero? (main/front-door ["runbooks/team/suspend.edn" "--help"])))
        (is (= ["bb" "-m" "gumshoe.edn-book" "/pin/runbooks/team/suspend.edn" "--help"] @launched)))
      (testing "--list prints it like any book"
        (is (= "runbooks/team/suspend.edn\tSuspends\n" (with-out-str (main/run ["--list"]))))))))

;; --- conformance ---------------------------------------------------------------

(deftest conformance-finds-edn-books-below-the-roots
  (fs/with-temp-dir [dir {}]
    (fs/create-dirs (fs/path dir "runbooks" "team"))
    (spit-bytes (fs/path dir "runbooks" "team" "a.edn") "{}")
    (spit-bytes (fs/path dir "runbooks" "team" "b.clj") "")
    (spit-bytes (fs/path dir "runbooks" "team" "c.txt") "")
    (is (= [(str (fs/path dir "runbooks" "team" "a.edn")) (str (fs/path dir "runbooks" "team" "b.clj"))]
           (conformance/book-files [(str (fs/path dir "runbooks"))])))))

(defn- report
  [books]
  (conformance/report {:books books :libraries [] :license "0BSD" :guards :require}))

(deftest conformance-judges-each-edn-book-as-it-judges-its-clojure-book
  (doseq [[clj edn] (map vector
                         ["runbooks/kubernetes/cronjobs/suspend.clj" "runbooks/kubernetes/cronjobs/resume.clj"
                          "runbooks/kubernetes/workloads/scale.clj" "runbooks/kubernetes/objects/delete.clj"]
                         fixtures)]
    (testing edn
      (is (= {:findings [] :advisories []} (report [edn])))
      (is (= (map :check (:findings (report [clj]))) (map :check (:findings (report [edn]))))))))

(deftest conformance-reports-an-edn-book-the-run-door-refuses
  (fs/with-temp-dir [dir {}]
    (let [refused (spit-bytes (fs/path dir "refused.edn") (with-source #(assoc % :gumshoe/dsl 0)))
          tagged (spit-bytes (fs/path dir "tagged.edn") (str header "{:at #inst \"2026-11-25\"}"))
          unlicensed (spit-bytes (fs/path dir "unlicensed.edn") (pr-str (reader/read-book suspend-edn)))
          {:keys [findings]} (report [refused tagged unlicensed])]
      (is (= #{[refused :load] [tagged :load] [unlicensed :license]}
             (set (map (juxt :file :check) findings))))
      (is (some #(str/starts-with? (:message %) "does not load: [:gumshoe/dsl]: the change declares :gumshoe/dsl 0") findings))
      (is (some #(= "does not load: holds the tagged literal #inst - a book is plain EDN, and no tag is read" (:message %)) findings)))))

(deftest conformance-holds-an-edn-books-change-to-the-spec-contract
  (testing "the change an EDN book compiles to is judged: one missing its confirmation is a finding against the file"
    (let [load-book edn-book/load-book]
      (with-redefs [edn-book/load-book (fn [file] (assoc-in (load-book file) [:confirm :action] " "))]
        (let [{:keys [findings]} (report [suspend-edn])]
          (is (= [[suspend-edn :spec-contract]] (map (juxt :file :check) findings)))
          (is (str/includes? (:message (first findings)) (str "mutation spec " suspend-edn))))))))

(deftest conformance-holds-an-edn-books-change-to-the-guard-rule
  (testing "a destructive change without a guard is a :guard finding against the file"
    (let [load-book edn-book/load-book
          delete-edn "tests/edn-books/runbooks/kubernetes/objects/delete.edn"]
      (with-redefs [edn-book/load-book (fn [file] (dissoc (load-book file) :refusal :subjects))]
        (is (= [[delete-edn :guard]]
               (map (juxt :file :check) (:findings (report [delete-edn])))))))))

;; --- a fresh bb ----------------------------------------------------------------

(defn- absolute-classpath
  []
  (->> (str/split (System/getProperty "java.class.path") #":")
       (map #(str (fs/absolutize %)))))

(def ^:private fake-kubectl
  "A kubectl that answers the reads the CronJob books make, logs every
   call, and refuses anything else."
  (str "#!/bin/sh\n"
       "echo \"$*\" >> \"$FAKE_DIR/kubectl.log\"\n"
       "case \"$*\" in\n"
       "  *\"auth can-i\"*) echo yes ;;\n"
       "  *\"config view\"*) echo '{\"current-context\":\"fake\",\"contexts\":[{\"name\":\"fake\",\"context\":{\"cluster\":\"fake-cluster\"}}]}' ;;\n"
       "  *api-resources*) printf 'cronjobs  cj  batch/v1  true  CronJob\\n' ;;\n"
       "  *version*) echo '{\"clientVersion\":{\"gitVersion\":\"v1.31.0\"}}' ;;\n"
       "  *\"get cronjobs --all-namespaces\"*) printf '{\"kind\":\"List\",\"items\":[%s]}' \"$(cat \"$FAKE_DIR/cronjob.json\")\" ;;\n"
       "  *\"get cronjobs nightly\"*) cat \"$FAKE_DIR/cronjob.json\" ;;\n"
       "  *\"get nodes\"*) printf '{\"kind\":\"List\",\"items\":[{\"kind\":\"Node\",\"metadata\":{\"name\":\"w1\"},\"spec\":{\"unschedulable\":true}},{\"kind\":\"Node\",\"metadata\":{\"name\":\"w2\"}}]}' ;;\n"
       "  *) echo \"unexpected kubectl $*\" >&2; exit 1 ;;\n"
       "esac\n"))

(def ^:private cronjob
  "{\"apiVersion\":\"batch/v1\",\"kind\":\"CronJob\",\"metadata\":{\"name\":\"nightly\",\"namespace\":\"shop\",\"uid\":\"u-1\",\"resourceVersion\":\"7\",\"generation\":1},\"spec\":{\"schedule\":\"0 3 * * *\",\"suspend\":false}}")

(defn- scratch-casebook!
  "A casebook in dir whose bb.edn puts this run's classpath on its :paths and
   wires the run door, with a fake kubectl and fzf on a bin directory. bb.edn
   takes relative :paths only, so each entry is a symbolic link below dir."
  [dir]
  (spit (str (fs/path dir "bb.edn"))
        (pr-str {:paths (vec (map-indexed (fn [i entry]
                                            (let [link (fs/path "classpath" (str i) (fs/file-name entry))]
                                              (fs/create-dirs (fs/parent (fs/path dir link)))
                                              (fs/create-sym-link (fs/path dir link) entry)
                                              (str link)))
                                          (absolute-classpath)))
                 :tasks {'run {:override-builtin true
                               :requires '([gumshoe.main :as main])
                               :task '(System/exit (main/run *command-line-args*))}}}))
  (fs/create-dirs (fs/path dir "bin"))
  (spit (str (fs/path dir "bin" "kubectl")) fake-kubectl)
  (spit (str (fs/path dir "bin" "fzf")) "#!/bin/sh\necho 0.55.0\n")
  (doseq [tool ["kubectl" "fzf"]]
    (fs/set-posix-file-permissions (fs/path dir "bin" tool) "rwxr-xr-x"))
  (spit (str (fs/path dir "cronjob.json")) cronjob))

(defn- bb-run
  "Runs `bb run <args>` in the scratch casebook dir and returns {:exit :out
   :err :kubectl}, where :kubectl is every kubectl call it made."
  [dir & args]
  (fs/delete-if-exists (fs/path dir "kubectl.log"))
  (let [result @(apply process/process
                       {:dir (str dir) :out :string :err :string :in "" :continue true
                        :extra-env {"GUMSHOE_ENV" (str (fs/path dir "env.edn"))
                                    "NO_COLOR" "1"
                                    "KUBECONFIG" "/dev/null"
                                    "FAKE_DIR" (str dir)
                                    "PATH" (str (fs/path dir "bin") ":" (System/getenv "PATH"))}}
                       "bb" "run" args)
        log (fs/path dir "kubectl.log")]
    (assoc (select-keys result [:exit :out :err])
           :kubectl (if (fs/exists? log) (str/split-lines (slurp (str log))) []))))

(defn- without-note
  [err]
  (subs err (inc (str/index-of err "\n"))))

(defn- slug-free
  "Output with the recording's timestamp blanked."
  [s]
  (str/replace s #"(recordings/\S+/)\d{8}-\d{6}" "$1<stamp>"))

(deftest a-fresh-bb-runs-an-edn-book-as-it-runs-its-clojure-book
  (fs/with-temp-dir [dir {}]
    (scratch-casebook! dir)
    (let [edn (str (fs/absolutize suspend-edn))
          clj (str (fs/absolutize suspend-clj))]
      (testing "--help"
        (let [from-edn (bb-run dir edn "--help")
              from-clj (bb-run dir clj "--help")]
          (is (zero? (:exit from-edn)) (:err from-edn))
          (is (= (edn-book/experimental-note edn) (first (str/split-lines (:err from-edn)))))
          (is (str/includes? (:out from-edn) (str "bb run " edn " [options]")) "the usage line names the run door")
          (is (= (str/replace (:out from-clj) (str "bb " clj) "<usage>")
                 (str/replace (:out from-edn) (str "bb run " edn) "<usage>")))
          (is (= [] (:kubectl from-edn)) "--help reaches no cluster")))
      (testing "--dry-run against a stubbed kubectl"
        (let [args ["--namespace" "shop" "--job" "nightly" "--dry-run"]
              from-edn (apply bb-run dir edn args)
              from-clj (apply bb-run dir clj args)]
          (is (zero? (:exit from-edn)) (str (:out from-edn) (:err from-edn)))
          (is (zero? (:exit from-clj)) (str (:out from-clj) (:err from-clj)))
          (is (= (edn-book/experimental-note edn) (first (str/split-lines (:err from-edn)))))
          (is (str/includes? (:err from-edn) "would run: kubectl --namespace=shop patch cronjob nightly --type=merge --patch={\"spec\":{\"suspend\":true}}")
              (:err from-edn))
          (is (= (:out from-clj) (:out from-edn)) "stdout is byte-identical")
          (is (= (slug-free (:err from-clj)) (slug-free (without-note (:err from-edn))))
              "stderr is byte-identical beside the note")
          (is (= (:kubectl from-clj) (:kubectl from-edn)) "the same kubectl calls, none of them a patch")
          (is (not-any? #(str/includes? % "--type=merge") (:kubectl from-edn))))))))

(deftest a-fresh-bb-runs-an-edn-detective-and-only-reads
  (fs/with-temp-dir [dir {}]
    (scratch-casebook! dir)
    (let [edn (str (fs/absolutize "tests/edn-books/runbooks/kubernetes/nodes/cordoned.edn"))
          run (bb-run dir edn "--output" "json")
          report (json/parse-string (:out run) true)]
      (is (zero? (:exit run)) (str (:out run) (:err run)))
      (is (= (edn-book/experimental-note edn) (first (str/split-lines (:err run)))))
      (is (= [{:severity "info" :component "w1" :summary "node is cordoned (unschedulable)"
               :hint "uncordon with runbooks/kubernetes/nodes/uncordon.clj when the work on it is done"
               :detective "cordoned-nodes"}]
             (:findings report)))
      (is (= [{:name "nodes" :state "present" :count 2}] (:reads report)))
      (is (some #(str/includes? % "get nodes") (:kubectl run)))
      (is (not-any? #(re-find #"\b(patch|delete|annotate|scale|apply|create|label|cordon|drain)\b" %) (:kubectl run))
          (pr-str (:kubectl run))))))

(deftest a-fresh-bb-refuses-an-edn-book-before-it-reaches-anything
  (fs/with-temp-dir [dir {}]
    (scratch-casebook! dir)
    (doseq [[what content expected]
            [["the number 0 as marker" (with-source #(assoc % :gumshoe/dsl 0)) "[:gumshoe/dsl]: the change declares :gumshoe/dsl 0"]
             ["#= read-eval" (str header "{:gumshoe/dsl :v0-experimental :description #=(java.lang.System/exit 3)}") "is not plain EDN"]]]
      (testing what
        (let [file (spit-bytes (fs/path dir "refused.edn") content)
              {:keys [exit err kubectl]} (bb-run dir file "--namespace" "shop" "--job" "nightly")]
          (is (= 1 exit) err)
          (is (= (edn-book/experimental-note file) (first (str/split-lines err))))
          (is (str/includes? err (str "refused " file ": " expected)) err)
          (is (= [] kubectl) "no kubectl call"))))))
