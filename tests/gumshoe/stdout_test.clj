;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.stdout-test
  (:require [clojure.test :refer [deftest is testing]]
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
