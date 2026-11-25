;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.main-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.catalog]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.main :as main]
            [gumshoe.shell :as shell]))

(def ^:private guided #'main/guided)

(deftest guided-menu-dispatch-test
  (testing "each menu label dispatches to its action without throwing"
    ;; The fuzzy finder returns the label STRING; the menu must match on the
    ;; string value, not the label symbol.
    (doseq [[label expected]
            [[#'main/follow-lead :investigate]
             [#'main/scan-area :detect]
             [#'main/run-book :run]]]
      (with-redefs [fuzzy/select-single (fn [& _] @label)
                    main/investigate (fn [& _] :investigate)
                    main/detect (fn [& _] :detect)
                    main/run (fn [& _] :run)]
        (is (= expected (guided))
            (str "label " @label " should dispatch to " expected)))))
  (testing "an escaped (nil) selection at the top level quits with exit code 1"
    (with-redefs [fuzzy/select-single (fn [& _] nil)]
      (is (= 1 (guided))))))

(def ^:private menu-choice #'main/menu-choice)

(deftest menu-choice-test
  (testing "choosing the Back row resolves to :back"
    (with-redefs [fuzzy/select-single (fn [_ rows & _] (last rows))]
      (is (= :back (menu-choice "p" ["a" "b"] true))
          "with back? the '⬅ Back' row is appended and selecting it means step up")))
  (testing "ESC is :back in a submenu, but nil (quit) at the top"
    (with-redefs [fuzzy/select-single (fn [& _] nil)]
      (is (= :back (menu-choice "p" ["a"] true)))
      (is (nil? (menu-choice "p" ["a"] false)))))
  (testing "a normal pick passes through unchanged"
    (with-redefs [fuzzy/select-single (fn [& _] "a")]
      (is (= "a" (menu-choice "p" ["a" "b"] false))))))

(deftest guided-back-loops-test
  (testing "a submenu returning :back re-shows the top menu instead of exiting"
    (let [picks (atom [@#'main/scan-area @#'main/follow-lead])]
      (with-redefs [fuzzy/select-single (fn [& _] (let [v (first @picks)] (swap! picks rest) v))
                    main/detect (fn [& _] :back)
                    main/investigate (fn [& _] :done)]
        (is (= :done (guided))
            "Back from the scan submenu loops to the top, where the next pick runs")))))

(deftest catalog-lines-test
  (testing "each book is its path and description, separated by a tab"
    (is (= ["/books/runbooks/a.clj\tDoes a"
            "/books/runbooks/b.clj\t"]
           (main/catalog-lines [{:path "/books/runbooks/a.clj" :name "a" :description "Does a"}
                                {:path "/books/runbooks/b.clj" :name "b" :description nil}])))))

(deftest exec-map-reads-raw-arguments-test
  (testing "a door called by babashka's exec parses *command-line-args*"
    (binding [*command-line-args* ["--list"]]
      (with-redefs [gumshoe.catalog/books (fn [] [{:path "/b/runbooks/a.clj" :name "a" :description "A"}])]
        (is (= "/b/runbooks/a.clj\tA\n"
               (with-out-str (is (zero? (main/run {:list true}))))))))))

(deftest catalog-lines-print-the-stable-path-test
  (is (= ["runbooks/detectives/fabric.clj\tFabric"]
         (main/catalog-lines [{:path "/gitlibs/libs/io.github.metio/gumshoe/abc/runbooks/detectives/fabric.clj"
                               :id "runbooks/detectives/fabric.clj"
                               :name "detectives/fabric"
                               :description "Fabric"}]))))

(def ^:private fake-catalog
  [{:path "/pin/runbooks/detectives/fabric.clj" :id "runbooks/detectives/fabric.clj"
    :name "detectives/fabric" :description "Fabric"}])

(defmacro ^:private with-fake-catalog
  [launched & body]
  `(with-redefs [gumshoe.catalog/books (fn [& _#] fake-catalog)
                 gumshoe.catalog/casebook-root (fn [] "/nonexistent-casebook")
                 shell/run-with-output (fn [& args#] (reset! ~launched (vec args#)) 0)]
     ~@body))

(deftest a-listed-path-launches-its-book-test
  (let [launched (atom nil)]
    (with-fake-catalog launched
      (testing "the run door launches a path with the flags after it"
        (is (zero? (main/run ["runbooks/detectives/fabric.clj" "--vni" "4208"])))
        (is (= ["bb" "/pin/runbooks/detectives/fabric.clj" "--vni" "4208"] @launched)))
      (testing "the front door does the same"
        (reset! launched nil)
        (is (zero? (main/front-door ["runbooks/detectives/fabric.clj" "--help"])))
        (is (= ["bb" "/pin/runbooks/detectives/fabric.clj" "--help"] @launched)))
      (testing "an unknown path launches nothing and names the near matches"
        (reset! launched nil)
        (let [err (java.io.StringWriter.)]
          (is (= 1 (binding [*err* err] (main/run ["runbooks/detectives/fabrik.clj"]))))
          (is (nil? @launched))
          (is (str/includes? (str err) "runbooks/detectives/fabric.clj")))))))

(deftest the-front-door-lists-books-test
  (with-redefs [gumshoe.catalog/books (fn [& _] fake-catalog)]
    (is (= "runbooks/detectives/fabric.clj\tFabric\n"
           (with-out-str (is (zero? (main/front-door ["--list"]))))))))

(def ^:private door-tasks
  "{run     {:override-builtin true
            :requires ([gumshoe.main :as main])
            :task (System/exit (main/run *command-line-args*))}
    gumshoe {:requires ([gumshoe.main :as main])
             :task (System/exit (main/front-door *command-line-args*))}}")

(defn- casebook-edn!
  "A casebook whose :paths reach gumshoe's libraries and a pinned upstream's
   books. babashka accepts only relative :paths, so both are written relative
   to the casebook."
  [casebook upstream]
  (let [relative #(str (fs/relativize (fs/canonicalize casebook) (fs/canonicalize %)))]
    (spit (str (fs/path casebook "bb.edn"))
          (format "{:paths [%s \"runbooks\" %s]\n :tasks %s}"
                  (pr-str (relative "libraries"))
                  (pr-str (relative (fs/path upstream "runbooks")))
                  door-tasks))))

(defn- echo-book!
  [root says]
  (let [file (fs/path root "runbooks" "up" "echo.clj")]
    (fs/create-dirs (fs/parent file))
    (spit (str file) (format "(println %s)\n" (pr-str says)))))

(defn- bb-in
  [dir & args]
  (let [{:keys [out err exit]} (apply process/shell {:dir (str dir) :out :string :err :string :continue true}
                                      "bb" args)]
    {:out (str/trim out) :err err :exit exit}))

(deftest a-casebook-runs-a-listed-upstream-book-verbatim-test
  (fs/with-temp-dir [tmp {}]
    (let [casebook (fs/path tmp "casebook")
          pin (fn [c] (fs/path tmp "gitlibs" "libs" "io.github.metio" "gumshoe" (apply str (repeat 40 c))))]
      (fs/create-dirs (fs/path casebook "runbooks"))
      (echo-book! (pin "a") "upstream at a")
      (echo-book! (pin "b") "upstream at b")
      (casebook-edn! casebook (pin "a"))
      (let [listed (->> (str/split-lines (:out (bb-in casebook "run" "--list")))
                        (map #(first (str/split % #"\t")))
                        (set))]
        (is (= #{"runbooks/up/echo.clj"} listed) "the listing names the book by its stable path")
        (is (= listed (->> (str/split-lines (:out (bb-in casebook "gumshoe" "--list")))
                           (map #(first (str/split % #"\t")))
                           (set)))))
      (is (= "upstream at a" (:out (bb-in casebook "run" "runbooks/up/echo.clj"))))
      (is (= "upstream at a" (:out (bb-in casebook "gumshoe" "runbooks/up/echo.clj"))))
      (casebook-edn! casebook (pin "b"))
      (is (= "upstream at b" (:out (bb-in casebook "run" "runbooks/up/echo.clj")))
          "the same line runs after the pin moves")
      (echo-book! casebook "ours")
      (is (= "ours" (:out (bb-in casebook "run" "runbooks/up/echo.clj")))
          "the casebook's own book of that path wins")
      (let [{:keys [exit err]} (bb-in casebook "run" "runbooks/up/ecko.clj")]
        (is (= 1 exit))
        (is (str/includes? err "runbooks/up/echo.clj"))))))

(deftest the-advised-command-runs-a-listed-path-in-a-casebook-test
  (fs/with-temp-dir [tmp {}]
    (let [casebook (fs/path tmp "casebook")
          pin (fs/path tmp "gitlibs" "libs" "io.github.metio" "gumshoe" (apply str (repeat 40 "c")))]
      (fs/create-dirs (fs/path casebook "runbooks"))
      (echo-book! pin "upstream")
      (casebook-edn! casebook pin)
      (let [listed (first (str/split (:out (bb-in casebook "run" "--list")) #"\t"))
            [program & args] (str/split (gumshoe.catalog/run-command listed) #" ")]
        (is (= "bb" program))
        (is (= "upstream" (:out (apply bb-in casebook args)))
            "the advice, with a path copied from --list, runs the book")))))

(defn- door-task
  [bb-edn task]
  (get-in (clojure.edn/read-string (slurp bb-edn)) [:tasks task]))

(deftest the-advised-command-resolves-every-listed-book-in-this-checkout-test
  (testing "the checkout and the example casebook wire bb run to the run door"
    (doseq [bb-edn ["bb.edn" "casebook-example/bb.edn"]
            :let [task (door-task bb-edn 'run)]]
      (is (:override-builtin task) (str bb-edn " replaces babashka's built-in run"))
      (is (= '(System/exit (main/run *command-line-args*)) (:task task))
          (str bb-edn " hands bb run to gumshoe.main/run"))))
  (let [books (gumshoe.catalog/books)
        root (gumshoe.catalog/casebook-root)]
    (is (seq books))
    (doseq [line (main/catalog-lines books)
            :let [listed (first (str/split line #"\t"))
                  [program door path & more] (str/split (gumshoe.catalog/run-command listed) #" ")]]
      (is (= ["bb" "run" listed nil] [program door path (first more)]))
      (is (#'main/book-path? path) (str path " dispatches as a book path"))
      (is (some-> (:path (gumshoe.catalog/resolve-book path books root)) fs/regular-file?)
          (str "bb run " path " resolves to a book file")))))

(deftest detect-refuses-a-positional-argument-test
  (let [refusals (atom [])]
    (with-redefs-fn {#'main/detect-refusal! (fn [data] (swap! refusals conj data))}
      (fn []
        (testing "a stray word before --dry-run is refused before any book is chosen"
          (with-redefs [fuzzy/select-single (fn [& _] (throw (ex-info "the menu opened" {})))]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unexpected argument: \"stray\""
                                  (main/detect ["stray" "--dry-run"]))))
          (is (= :unexpected-argument (:cause (first @refusals))))
          (is (= "stray" (:argument (first @refusals)))))
        (testing "flags alone still parse"
          (is (= {:dry-run true :no-color true} (main/detect-options ["--dry-run" "--no-color"]))))))))
