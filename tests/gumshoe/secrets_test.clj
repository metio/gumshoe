;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.secrets-test
  (:require [clojure.string]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.config :as config]
            [gumshoe.promql]
            [gumshoe.secrets :as secrets]))

(defn- with-secrets
  "Runs f with the :secrets config fixed to the given map."
  [secrets-config f]
  (with-redefs [config/value (fn [path & [d]] (if (= path [:secrets]) secrets-config d))]
    (f)))

(deftest resolve-command-test
  (testing "substitutes {path} and {field} into a command template"
    (is (= ["gopass" "show" "--password" "matrix/bot"]
           (secrets/resolve-command ["gopass" "show" "--password" "{path}"] {:path "matrix/bot"})))
    (is (= ["gopass" "show" "matrix/bot" "login"]
           (secrets/resolve-command ["gopass" "show" "{path}" "{field}"] {:path "matrix/bot" :field "login"})))
    (testing "a different manager with its own path structure is just a template"
      (is (= ["op" "read" "op://ops/matrix/bot/password"]
             (secrets/resolve-command ["op" "read" "op://ops/{path}/password"] {:path "matrix/bot"})))
      (is (= ["pass" "show" "matrix/bot"]
             (secrets/resolve-command ["pass" "show" "{path}"] {:path "matrix/bot"})))))
  (testing "a placeholder with no substitution is left untouched"
    (is (= ["cmd" "{field}"] (secrets/resolve-command ["cmd" "{field}"] {:path "x"})))))

(deftest defaults-are-gopass-test
  (is (= "gopass" (first (:password secrets/defaults))))
  (is (= "gopass" (first (:search secrets/defaults)))))

(deftest built-in-providers-test
  (testing "gopass, pass, passage, pasejo, and the command escape hatch ship built in"
    (is (every? (set (secrets/registered-providers)) [:gopass :pass :passage :pasejo :command]))))

(deftest active-name-selection-test
  (testing "an explicit :provider wins"
    (is (= :pass (secrets/active-name {:provider :pass})))
    (is (= :pasejo (secrets/active-name {:provider :pasejo}))))
  (testing "no config defaults to gopass"
    (is (= :gopass (secrets/active-name {}))))
  (testing "a legacy config of bare templates is read as the :command provider"
    (is (= :command (secrets/active-name {:password ["op" "read" "op://ops/{path}/pw"]})))))

(deftest first-line-is-the-password-test
  (testing "the password is the first non-blank line (pass/passage/pasejo convention)"
    (is (= "s3cr3t" (secrets/first-line "s3cr3t\nlogin: alice\nurl: https://x")))
    (is (= "s3cr3t" (secrets/first-line "\n\ns3cr3t\n")))
    (is (nil? (secrets/first-line "   \n")))))

(deftest field-in-parses-name-value-lines-test
  (let [secret "s3cr3t\nlogin: alice\nurl: https://x:8443/path"]
    (is (= "alice" (secrets/field-in secret "login")))
    (testing "a value may itself contain colons"
      (is (= "https://x:8443/path" (secrets/field-in secret "url"))))
    (is (nil? (secrets/field-in secret "missing")))))

(deftest plugin-can-register-a-native-provider-test
  (testing "a plugin adds a backend the CLI-template model can not express"
    (secrets/register-provider! {:name :vault-native :binary "vault"
                                 :password (fn [_] "x") :field (fn [_ _] nil)
                                 :find-path (fn [_] nil) :available? (fn [_] true)})
    (is (contains? (set (secrets/registered-providers)) :vault-native))))

(deftest command-name-reflects-the-active-provider-test
  (secrets/register-provider! {:name :vault-nobinary
                               :password (fn [_] "x") :field (fn [_ _] nil)
                               :find-path (fn [_] nil) :available? (fn [_] true)})
  (testing "a plugin provider without :binary yields nil, not the gopass template word"
    (is (nil? (with-secrets {:provider :vault-nobinary} secrets/command-name))))
  (testing "the template-driven :command provider still reports its command's first word"
    (is (= "gopass" (with-secrets {:provider :command} secrets/command-name))))
  (testing "a built-in provider reports its own binary"
    (is (= "pass" (with-secrets {:provider :pass} secrets/command-name)))))

(deftest unknown-provider-warns-and-falls-back-test
  (testing "a configured provider that is not registered warns instead of silently using gopass"
    (reset! @#'secrets/warned-unknown #{})
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (is (= "gopass" (with-secrets {:provider :nonexistent-typo} secrets/command-name))))
      (is (clojure.string/includes? (str err) "not registered")))))

(deftest a-url-finds-the-secret-named-for-its-host
  (let [searched (atom [])]
    (secrets/register-provider! {:name :substring-store
                                 :password (fn [_] "p") :field (fn [_ _] "u")
                                 :find-path (fn [term]
                                              (swap! searched conj term)
                                              (first (filter #(clojure.string/includes? % term)
                                                             ["api/prometheus.example.org"])))
                                 :available? (fn [_] true)})
    (with-secrets {:provider :substring-store}
      (fn []
        (is (= "api/prometheus.example.org" (secrets/find-path "https://prometheus.example.org:9090/")))
        (is (= "api/prometheus.example.org" (secrets/find-path "prometheus.example.org")))
        (is (= {:username "u" :password "p"}
               (gumshoe.promql/credentials "https://prometheus.example.org/" {})))))))

(deftest a-path-names-a-term-only-as-a-whole-segment
  (is (true? (secrets/matches-term? "a.example.com" "a.example.com")))
  (is (true? (secrets/matches-term? "a.example.com" "api/a.example.com")))
  (is (true? (secrets/matches-term? "a.example.com" "hosts/a.example.com/admin")))
  (testing "a path that merely contains the host belongs to another host"
    (is (false? (secrets/matches-term? "a.example.com" "api/data.example.com")))
    (is (false? (secrets/matches-term? "a.example.com" "api/a.example.com.old")))
    (is (false? (secrets/matches-term? "example.com" "api/a.example.com"))))
  (is (false? (secrets/matches-term? "" "api/a.example.com"))))

(deftest best-match-never-picks-another-hosts-secret
  (testing "the substring hit that sorts first is not taken"
    (is (nil? (secrets/best-match "a.example.com" ["api/data.example.com" "api/ba.example.com"])))
    (is (= "api/a.example.com"
           (secrets/best-match "a.example.com" ["api/data.example.com" "api/a.example.com"]))))
  (testing "an exact path wins, then a path ending in the host, then the rest, each sorted"
    (is (= "a.example.com"
           (secrets/best-match "a.example.com" ["z/a.example.com" "a.example.com" "a.example.com/admin"])))
    (is (= "b/a.example.com"
           (secrets/best-match "a.example.com" ["z/a.example.com" "a.example.com/admin" "b/a.example.com"])))
    (is (= "a.example.com/admin"
           (secrets/best-match "a.example.com" ["a.example.com/readonly" "a.example.com/admin"]))))
  (testing "blank lines and padding from a CLI's output are ignored"
    (is (= "api/a.example.com" (secrets/best-match "a.example.com" ["" "  api/a.example.com  " nil])))))

(deftest find-path-filters-every-providers-answer
  (testing "a provider's candidates go through best-match"
    (secrets/register-provider! {:name :candidate-store
                                 :password (fn [_] "p") :field (fn [_ _] "u")
                                 :candidates (fn [_] ["api/data.example.com" "monitoring/a.example.com"])
                                 :available? (fn [_] true)})
    (with-secrets {:provider :candidate-store}
      (fn []
        (is (= "monitoring/a.example.com" (secrets/find-path "https://a.example.com:9090/")))
        (is (nil? (secrets/find-path "https://b.example.com/")))
        (is (nil? (secrets/find-path ""))))))
  (testing "a legacy :find-path answer that only contains the host is refused"
    (secrets/register-provider! {:name :fuzzy-store
                                 :password (fn [_] "p") :field (fn [_ _] "u")
                                 :find-path (fn [_] "api/data.example.com")
                                 :available? (fn [_] true)})
    (with-secrets {:provider :fuzzy-store}
      (fn []
        (is (nil? (secrets/find-path "a.example.com")))
        (is (= {:username nil :password nil}
               (gumshoe.promql/credentials "https://a.example.com/" {})))))))

(deftest the-built-in-providers-list-candidates-for-best-match
  (doseq [provider [secrets/gopass secrets/pass secrets/passage secrets/pasejo secrets/command]]
    (testing (name (:name provider))
      (is (fn? (:candidates provider)))
      (is (nil? (:find-path provider)) "no provider picks a path by substring itself"))))

(deftest pasejo-picks-the-host-segment-from-its-listing
  (with-redefs [gumshoe.shell/stdout-of (fn [& _] "api/data.example.com\napi/a.example.com\n")]
    (is (= "api/a.example.com" (secrets/choose-path secrets/pasejo "a.example.com")))
    (is (nil? (secrets/choose-path secrets/pasejo "ta.example.com")))))
