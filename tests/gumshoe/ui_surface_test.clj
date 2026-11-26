;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ui-surface-test
  "The documented output surface: outcome lines, items, sections, banners,
   task lists and command echoes take every mark from the theme, render the
   same through their old names, and lose no meaning without colour."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gumshoe.progress :as progress]
            [gumshoe.render :as render]
            [gumshoe.stdout :as stdout]
            [gumshoe.theme :as theme]))

;; Each test registers into a copy of the registry and leaves the default
;; active, so no theme leaks into another test.
(use-fixtures :each (fn [t]
                      (with-redefs-fn {#'theme/themes (atom @@#'theme/themes)}
                        (fn [] (theme/select! :default) (t) (theme/select! :default)))))

(def ^:private escape #"\u001b\[")

(defn- stderr-of
  "What thunk prints to stderr with colour set to color? and no live repaint."
  ([thunk] (stderr-of false thunk))
  ([color? thunk]
   (let [err (java.io.StringWriter.)
         out (java.io.StringWriter.)]
     (with-redefs-fn {#'stdout/colors? (atom color?)
                      #'progress/live? (constantly false)}
       (fn [] (binding [*err* err *out* out stdout/*width* 40] (thunk))))
     (str err))))

(def ^:private stars
  "A theme that shares no outcome glyph with the built-ins."
  {:name :stars
   :outcome {:success {:glyph "★"} :failure {:glyph "✱"} :finding {:glyph "✦"}
             :info {:glyph "·"} :done {:glyph "◆"} :not-yet {:glyph "◇"}
             :in-progress {:glyph "◈"} :skipped {:glyph "▫"}}
   :rule "="})

(defn- a-book
  "Stands in for a casebook book that renders only through the surface."
  []
  (stdout/print-section "Adoption status")
  (stdout/line :success "database restored")
  (stdout/line :failure "ingress missing")
  (stdout/line :finding "certificate expires in 5 days")
  (stdout/item :done "volume migrated")
  (stdout/item :not-yet "secrets copied")
  (stdout/item :skipped "dns switched")
  (stdout/item :in-progress "waiting for rollout")
  (progress/checklist [["kubectl" (constantly true)] ["helm" (constantly false)]]
                      {:stop-on-failure? false}))

(deftest changing-the-theme-restyles-a-book-without-editing-it
  (theme/register! stars)
  (let [default-run (do (theme/select! :default) (stderr-of a-book))
        stars-run (do (theme/select! :stars) (stderr-of a-book))
        ascii-run (do (theme/select! :ascii) (stderr-of a-book))]
    (testing "every line names the same text under every theme"
      (doseq [run [default-run stars-run ascii-run]]
        (is (str/includes? run "Adoption status"))
        (is (str/includes? run "database restored"))
        (is (str/includes? run "helm"))))
    (testing "the default draws its own marks"
      (is (str/includes? default-run "✅ database restored"))
      (is (str/includes? default-run "❌ ingress missing"))
      (is (str/includes? default-run "🔶 certificate expires"))
      (is (str/includes? default-run "  ⬜ secrets copied"))
      (is (str/includes? default-run "  ✅ kubectl") "the task list takes the done mark")
      (is (str/includes? default-run "  ❌ helm")))
    (testing "another theme draws its marks in the same places"
      (is (str/includes? stars-run "★ database restored"))
      (is (str/includes? stars-run "✱ ingress missing"))
      (is (str/includes? stars-run "✦ certificate expires"))
      (is (str/includes? stars-run "  ◆ volume migrated"))
      (is (str/includes? stars-run "  ◇ secrets copied"))
      (is (str/includes? stars-run "  ▫ dns switched"))
      (is (str/includes? stars-run "  ◈ waiting for rollout"))
      (is (str/includes? stars-run "  ◆ kubectl"))
      (is (str/includes? stars-run "  ✱ helm"))
      (is (str/starts-with? stars-run "== ") "the section rule follows the theme")
      (is (not-any? #(str/includes? stars-run %) ["✅" "❌" "🔶" "⬜"])))
    (testing "ascii draws words"
      (is (str/includes? ascii-run "[OK] database restored"))
      (is (str/includes? ascii-run "  [TODO] secrets copied"))
      (is (str/includes? ascii-run "  [ERROR] helm")))))

(deftest the-pure-renderers-follow-the-theme-they-are-given
  (let [plain (render/context theme/default-theme false 30)
        starry (render/context stars false 30)]
    (doseq [[f args] [[render/line [:success "x"]]
                      [render/item [:not-yet "x"]]
                      [render/task [:done "x"]]
                      [render/task [:failed "x"]]
                      [render/task [:pending "x"]]
                      [render/section ["Title"]]]]
      (is (not= (apply f plain args) (apply f starry args)) (str f args)))
    (is (= "✅ x" (render/line plain :success "x")))
    (is (= "[OK] x" (render/line (render/context :ascii false) :success "x")) "a theme by name")
    (is (= "✅ x" (render/line (render/context :no-such-theme false) :success "x")))
    (is (= "★ x" (render/line starry :success "x")))
    (is (= "  ◇ x" (render/task starry :pending "x")))
    (is (= "── Title ─────────────────────" (render/section plain "Title")))))

(deftest old-names-render-as-their-outcome
  (doseq [[old new] [[#(stdout/ok "fine") #(stdout/line :success "fine")]
                     [#(stdout/error "broken") #(stdout/line :failure "broken")]
                     [#(stdout/warn "careful") #(stdout/line :finding "careful")]
                     [#(stdout/check-ok "can" "get") #(stdout/item :success "can" "get")]
                     [#(stdout/check-error "cannot" "get") #(stdout/item :failure "cannot" "get")]]]
    (doseq [color? [false true]]
      (is (= (stderr-of color? old) (stderr-of color? new)))))
  (testing "the legacy theme tokens read the outcomes"
    (is (= (theme/glyph :success) (theme/token :ok)))
    (is (= (theme/glyph :failure) (theme/token :error) (theme/token :check-error)))
    (is (= (theme/glyph :finding) (theme/token :warn)))
    (is (= (theme/glyph :done) (theme/token :check-ok)))
    (is (= (theme/glyph :not-yet) (theme/token :pending)))
    (is (= (theme/glyph :info) (theme/token :bullet)))))

(deftest every-old-printer-still-prints
  (doseq [[nm call] {"ok" #(stdout/ok "fine")
                     "error" #(stdout/error "broken")
                     "warn" #(stdout/warn "careful")
                     "check-ok" #(stdout/check-ok "fine")
                     "check-error" #(stdout/check-error "broken")
                     "print-section" #(stdout/print-section "Title")
                     "print-section-marker" #(stdout/print-section-marker)
                     "print-banner" #(stdout/print-banner stdout/yellow "📦 paused")
                     "print-command" #(stdout/print-command "kubectl" "get" "pods")
                     "print-table" #(stdout/print-table [["POD" :pod]] [{:pod "web-1"}])
                     "print-data-table" #(stdout/print-data-table {:node "w-1"})
                     "selected" #(stdout/selected "Node" "w-1")
                     "err-println" #(stdout/err-println "  a" "line")}]
    (testing nm
      (is (not (str/blank? (stderr-of call))))))
  (testing "the colour helpers return their text"
    (is (= "x" (stdout/red "x") (stdout/green "x") (stdout/yellow "x") (stdout/blue "x")
           (stdout/bold "x")))))

(deftest plain-output-carries-no-escape-codes
  (testing "with colour off, nothing the surface prints carries an escape"
    (is (not (re-find escape (stderr-of false a-book))))
    (is (not (re-find escape (stderr-of false #(stdout/banner :failure "stop")))))
    (is (not (re-find escape (stderr-of false #(stdout/print-command "kubectl" "get"))))))
  (testing "with colour on the marks are coloured"
    (is (re-find escape (stderr-of true #(stdout/line :success "fine")))))
  (testing "the :plain theme stays colourless even where colour is possible"
    (theme/select! :plain)
    (is (not (re-find escape (stderr-of true a-book))))))

(deftest a-theme-of-colour-only-glyphs-falls-back-to-plain-marks
  (let [dots {:name :dots
              :outcome {:success {:glyph "●" :color :green :plain "[ok]"}
                        :failure {:glyph "●" :color :red :plain "[fail]"}}}
        coloured (render/context dots true)
        colourless (render/context dots false)]
    (is (= "\u001b[32m●\u001b[0m fine" (render/line coloured :success "fine")))
    (is (= "[ok] fine" (render/line colourless :success "fine")))
    (is (= "[fail] broken" (render/line colourless :failure "broken")))
    (is (= "🔶 careful" (render/line colourless :finding "careful"))
        "an outcome the theme leaves alone keeps the default's mark")))

(deftest a-theme-written-with-the-old-keys-still-applies
  (theme/register! {:name :thumbs :ok "👍" :check-error "👎" :bullet "-"})
  (theme/select! :thumbs)
  (is (= "👍" (theme/glyph :success)))
  (is (= "👎" (theme/glyph :failure)))
  (is (= "-" (theme/glyph :info)))
  (is (= (theme/glyph :not-yet) "⬜") "what it leaves alone comes from the default")
  (is (str/includes? (stderr-of #(stdout/ok "fine")) "👍 fine"))
  (testing "an outcome named explicitly wins over an old key"
    (is (= "★" (:glyph (theme/outcome-in (theme/resolve-theme {:ok "👍" :outcome {:success {:glyph "★"}}})
                                         :success)))))
  (testing "an unknown outcome reads as info, so its text still prints"
    (is (= (theme/glyph :info) (:glyph (theme/outcome :nonsense))))))

(def ^:private casebook-surface
  "Every gumshoe.stdout var the infra.run casebook calls, with the argument
   lists it calls them with (git grep over its libraries, runbooks and
   playbooks on 2026-11-26). Changing one of these breaks a consumer."
  {'error '[& parts]
   'print-section '[title]
   'ok '[& parts]
   'warn '[& parts]
   'err-println '[& parts]
   'print-command '[& args]
   'check-error '[& parts]
   'check-ok '[& parts]
   'yellow '[text]
   'red '[text]
   'print-banner '[color-fn text]
   'blue '[text]})

(deftest the-casebook-surface-keeps-its-vars-and-arities
  (doseq [[sym arglist] casebook-surface
          :let [v (ns-resolve 'gumshoe.stdout sym)]]
    (testing (str sym)
      (is (some? v) "the var still exists")
      (is (fn? @v))
      (is (some #{arglist} (:arglists (meta v))) "the argument list is unchanged")))
  (testing "a progress checklist keeps both arities"
    (is (= '([steps] [steps {:keys [stop-on-failure?] :or {stop-on-failure? true}}])
           (:arglists (meta #'progress/checklist))))))

(deftest every-public-surface-var-is-documented
  (doseq [ns-sym '[gumshoe.stdout gumshoe.render gumshoe.theme gumshoe.progress]
          [sym v] (ns-publics ns-sym)]
    (is (not (str/blank? (:doc (meta v)))) (str ns-sym "/" sym " has no docstring"))))
