;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.stdout-test
  (:require [clojure.string]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.stdout :as stdout]))

(deftest data-table-test
  (testing "aligns labels and keeps insertion order"
    (is (= (str "cluster : kube.example.org\n"
                "node    : worker-1")
           (stdout/data-table {:cluster "kube.example.org"
                               :node "worker-1"}))))
  (testing "renders nothing for empty data"
    (is (= "" (stdout/data-table {})))))

(deftest table-test
  (testing "aligns columns to their widest cell and trims trailing padding"
    (is (= (str "NAMESPACE  POD         STATUS\n"
                "moodle     web-1       Running\n"
                "keycloak   keycloak-0  CrashLoopBackOff")
           (stdout/table [["NAMESPACE" :ns] ["POD" :pod] ["STATUS" :status]]
                         [{:ns "moodle" :pod "web-1" :status "Running"}
                          {:ns "keycloak" :pod "keycloak-0" :status "CrashLoopBackOff"}]))))
  (testing "an accessor may be a function of the row, not just a key"
    (is (= (str "POD    RESTARTS\n"
                "web-1  3")
           (stdout/table [["POD" :pod] ["RESTARTS" (comp str :restarts)]]
                         [{:pod "web-1" :restarts 3}]))))
  (testing "the header widens a column when it is wider than every cell"
    (is (= (str "CONTAINER  IMAGE\n"
                "a          nginx")
           (stdout/table [["CONTAINER" :c] ["IMAGE" :img]]
                         [{:c "a" :img "nginx"}]))))
  (testing "a cell carrying ANSI codes still lines up (column width excludes escapes)"
    (let [out (stdout/table [["NAME" :name] ["STATUS" :status]]
                            [{:name "a" :status "[31mbad[0m"}
                             {:name "longername" :status "ok"}])
          lines (clojure.string/split-lines (stdout/strip-colors out))]
      (is (apply = (map #(clojure.string/index-of %2 %1)
                        ["STATUS" "bad" "ok"] lines))
          "the STATUS column starts at the same offset on every row despite ANSI codes")))
  (testing "no rows render nothing"
    (is (= "" (stdout/table [["A" :a]] [])))))

(deftest elide-test
  (testing "a value within the width is unchanged"
    (is (= "short" (stdout/elide 20 "short"))))
  (testing "a long value is middle-elided to fit, keeping its head and tail"
    (let [out (stdout/elide 30 "/home/seb/.cache/gitlibs/io.github.metio.gumshoe/abc/cordon.clj")]
      (is (= 30 (count out)))
      (is (clojure.string/includes? out "…"))
      (is (clojure.string/starts-with? out "/home"))
      (is (clojure.string/ends-with? out ".clj"))))
  (testing "an astral character at a cut boundary is not split into a lone surrogate"
    (is (= "🔦…🔦🔦" (stdout/elide 4 "🔦🔦🔦🔦🔦🔦"))
        "slicing must be on code points, not UTF-16 units")))

(deftest wrap-test
  (let [prose "the exact kubectl calls are offered as a reproducer below - keep them with the incident notes"]
    (testing "a long line wraps at word boundaries, each line within the width"
      (let [lines (clojure.string/split-lines (stdout/wrap prose 40))]
        (is (> (count lines) 1))
        (is (every? #(<= (count %) 40) lines))
        (is (= (clojure.string/split prose #" ")
               (clojure.string/split (clojure.string/join " " lines) #" "))
            "no word is split or lost across the wrap")))
    (testing "text within the width stays on one line"
      (is (= "short line" (stdout/wrap "short line" 40))))
    (testing "a single word longer than the width is left whole, not split"
      (is (= "supercalifragilisticexpialidocious"
             (stdout/wrap "supercalifragilisticexpialidocious" 10))))))

(deftest shorten-path-test
  (let [home (System/getProperty "user.home")]
    (testing "a path under home is shortened to ~, staying copy-pasteable"
      (is (= "~/.cache/gitlibs/x.edn" (stdout/shorten-path (str home "/.cache/gitlibs/x.edn")))))
    (testing "the home directory itself becomes ~"
      (is (= "~" (stdout/shorten-path home))))
    (testing "a sibling that merely shares the home prefix is left alone"
      (is (= (str home "-sibling/x") (stdout/shorten-path (str home "-sibling/x")))
          "matching must be on a path boundary, not a raw string prefix"))
    (testing "a path outside home is unchanged"
      (is (= "/etc/hosts" (stdout/shorten-path "/etc/hosts"))))))

(deftest choose-width-test
  (testing "captured output keeps the fixed width, whatever COLUMNS says"
    (is (= 80 (stdout/choose-width {:live? false :columns "200" :stty "200"})))
    (is (= 80 (stdout/choose-width {}))))
  (testing "a live terminal uses COLUMNS, else stty"
    (is (= 100 (stdout/choose-width {:live? true :columns "100" :stty "90"})))
    (is (= 90 (stdout/choose-width {:live? true :columns "" :stty "90"})))
    (is (= 90 (stdout/choose-width {:live? true :columns "wide" :stty " 90 "}))))
  (testing "a live terminal whose size cannot be read gets the fixed width"
    (is (= 80 (stdout/choose-width {:live? true}))))
  (testing "a live width is clamped"
    (is (= 120 (stdout/choose-width {:live? true :columns "300"})))
    (is (= 60 (stdout/choose-width {:live? true :columns "20"}))))
  (testing "an explicit override wins everywhere, clamped too"
    (is (= 100 (stdout/choose-width {:override "100" :live? false})))
    (is (= 70 (stdout/choose-width {:override "70" :live? true :columns "110"})))
    (is (= 120 (stdout/choose-width {:override "500"})))
    (is (= 80 (stdout/choose-width {:override "auto"})) "an unreadable override is ignored")))

(defn- stderr-of [f]
  (let [err (java.io.StringWriter.)]
    (binding [*err* err] (f))
    (str err)))

(deftest rules-fill-the-width-test
  (doseq [w [60 80 117]]
    (binding [stdout/*width* w]
      (is (= w (count (clojure.string/trimr (stderr-of stdout/print-section-marker)))) (str "marker at " w))
      (is (= w (#'stdout/visible-width (clojure.string/trimr (stderr-of #(stdout/print-section "Findings"))))) (str "section at " w))
      (is (every? #(<= (#'stdout/visible-width %) w)
                  (clojure.string/split-lines (stderr-of #(stdout/print-section (apply str (repeat 200 "x"))))))
          "a long title is elided within the width"))))

(deftest hanging-test
  (testing "unwrapped when there is no width"
    (is (= "        hint: a b c" (stdout/hanging "        " "hint: " "a b c" nil))))
  (testing "wrapped under the text"
    (let [text "the management cluster cannot reach the workload cluster's API server, so nothing is reconciled"
          lines (clojure.string/split-lines (stdout/hanging "        " "hint: " text 60))]
      (is (< 1 (count lines)))
      (is (clojure.string/starts-with? (first lines) "        hint: the management"))
      (is (every? #(clojure.string/starts-with? % "              ") (rest lines)) "continuations sit under the text")
      (is (every? #(<= (count %) 60) lines))
      (is (= text (clojure.string/join " " (map clojure.string/trim (cons (subs (first lines) 14) (rest lines))))) "no word is lost"))))

(deftest the-spinner-animates-only-on-a-live-stderr-test
  (testing "stderr decides, not stdout or FORCE_COLOR"
    (is (true? (stdout/animate? {:stderr-terminal? true})))
    (is (false? (stdout/animate? {:stderr-terminal? false})) "`2> run.log` gets the line once")
    (is (false? (stdout/animate? {:stderr-terminal? true :no-color? true})))
    (is (false? (stdout/animate? {:stderr-terminal? true :no-color-flag? true}))))
  (testing "off a live stderr the message is printed once, with no carriage-return frames"
    (let [err (java.io.StringWriter.)
          result (with-redefs-fn {#'stdout/stderr-live? (constantly false)
                                  #'stdout/colors? (atom true)}
                   (fn [] (binding [*err* err]
                            (stdout/with-spinner "listing pods" #(do (Thread/sleep 250) :done)))))]
      (is (= :done result))
      (is (= "listing pods\n" (str err)))
      (is (not (clojure.string/includes? (str err) "\r"))))))

(def ^:private diagnostic-calls
  "One call per printing helper. Everything they print is diagnostic chatter,
   which belongs on stderr so a result on stdout stays clean."
  {#'stdout/err-println #(stdout/err-println "a" "line")
   #'stdout/with-spinner #(stdout/with-spinner "reading pods" (constantly :done))
   #'stdout/print-section-marker #(stdout/print-section-marker)
   #'stdout/print-section #(stdout/print-section "Pods")
   #'stdout/print-banner #(stdout/print-banner stdout/red "stop")
   #'stdout/ok #(stdout/ok "fine")
   #'stdout/error #(stdout/error "broken")
   #'stdout/warn #(stdout/warn "careful")
   #'stdout/check-ok #(stdout/check-ok "can connect to" "host")
   #'stdout/check-error #(stdout/check-error "cannot connect to" "host")
   #'stdout/line #(stdout/line :done "migrated")
   #'stdout/item #(stdout/item :not-yet "restored")
   #'stdout/note #(stdout/note "a" "note")
   #'stdout/detail #(stdout/detail "kubectl" "get" "pods")
   #'stdout/banner #(stdout/banner :failure "stop")
   #'stdout/selected #(stdout/selected "Node" "w-1")
   #'stdout/print-data-table #(stdout/print-data-table {:node "w-1"})
   #'stdout/print-table #(stdout/print-table [["POD" :pod]] [{:pod "web-1"}])
   #'stdout/print-command #(stdout/print-command "kubectl" "get" "pods")
   #'stdout/child-warnings #(stdout/child-warnings ["Warning: v1 Endpoints is deprecated"])})

(def ^:private non-printing
  "The public vars that print nothing: formatters returning strings, colour
   and width settings."
  #{#'stdout/fixed-width #'stdout/choose-width #'stdout/*width* #'stdout/width
    #'stdout/results-wrap-width #'stdout/disable-colors! #'stdout/colors-enabled?
    #'stdout/no-color-requested? #'stdout/strip-colors #'stdout/elide #'stdout/wrap
    #'stdout/hanging #'stdout/shorten-path #'stdout/red #'stdout/green #'stdout/yellow
    #'stdout/blue #'stdout/bold #'stdout/animate? #'stdout/data-table #'stdout/table #'stdout/context})

(deftest every-public-helper-is-classified
  (is (= (set (vals (ns-publics 'gumshoe.stdout)))
         (into non-printing (keys diagnostic-calls)))
      "a new helper is either a printer with a stream test below or prints nothing"))

(deftest diagnostics-go-to-stderr-and-leave-stdout-empty
  (doseq [[helper call] diagnostic-calls]
    (testing (str (symbol helper))
      (let [out (java.io.StringWriter.)
            err (java.io.StringWriter.)]
        (binding [*out* out *err* err] (call))
        (is (= "" (str out)) "nothing reaches stdout")
        (is (not (clojure.string/blank? (str err))) "the text reaches stderr")))))
