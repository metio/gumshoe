;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.apiserver-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.apiserver :as apiserver]
            [gumshoe.mutation :as mutation]))

(defn- node [name labels]
  {:metadata {:name name :labels labels}})

(deftest control-plane-nodes-test
  (let [nodes {:items [(node "cp-1" {(keyword apiserver/control-plane-label) ""
                                     :kubernetes.io/hostname "cp-1"})
                       (node "cp-2" {(keyword apiserver/control-plane-label) ""})
                       (node "worker-1" {:kubernetes.io/hostname "worker-1"})]}]
    (testing "the role label selects control-plane nodes, whatever its value"
      (is (= ["cp-1" "cp-2"] (apiserver/control-plane-nodes nodes))))
    (testing "a cluster with no labelled node yields nothing rather than every node"
      (is (empty? (apiserver/control-plane-nodes {:items [(node "worker-1" {})]}))))))

(deftest container-id-test
  (testing "the single id crictl -q prints"
    (is (= "abc123" (apiserver/container-id "abc123\n"))))
  (testing "surrounding whitespace is not part of the id"
    (is (= "abc123" (apiserver/container-id "  abc123  "))))
  (testing "no running container reads as nil, not as an empty id"
    (is (nil? (apiserver/container-id "")))
    (is (nil? (apiserver/container-id "\n\n")))
    (is (nil? (apiserver/container-id nil))))
  (testing "the first id wins when the runtime prints more than one"
    (is (= "first" (apiserver/container-id "first\nsecond\n")))))

(deftest ready?-test
  (testing "only ok is ready"
    (is (true? (apiserver/ready? "ok")))
    (is (true? (apiserver/ready? "ok\n"))))
  (testing "a partial or failing answer is not ready"
    (is (false? (apiserver/ready? "[+]ping ok\n[-]etcd failed")))
    (is (false? (apiserver/ready? "")))
    (is (false? (apiserver/ready? nil)))))

(deftest restarted?-test
  (testing "a different id means the kubelet replaced the container"
    (is (true? (apiserver/restarted? "old" "new"))))
  (testing "the same id means nothing has restarted yet"
    (is (false? (apiserver/restarted? "old" "old"))))
  (testing "no replacement yet is not a restart"
    (is (false? (apiserver/restarted? "old" nil))))
  (testing "a first container where there was none counts as replaced"
    (is (true? (apiserver/restarted? nil "new")))))

(deftest commands-test
  (testing "the id query asks only for kube-apiserver, quietly"
    (is (= ["sudo" "crictl" "ps" "--name" "kube-apiserver" "-q"]
           (apiserver/container-id-command))))
  (testing "stop and inspect name the container they act on"
    (is (= ["sudo" "crictl" "stop" "abc"] (apiserver/stop-command "abc")))
    (is (= "abc" (last (apiserver/started-at-command "abc")))))
  (testing "readyz is asked of this node rather than the load balancer"
    (is (some #(= "https://localhost:6443/readyz" %) (apiserver/readyz-command))))
  (testing "readyz gives up instead of waiting on an apiserver that accepts and never answers"
    (is (= ["--max-time" (str apiserver/default-readyz-max-time)]
           (->> (apiserver/readyz-command) (drop-while #(not= "--max-time" %)) (take 2))))
    (is (= ["--max-time" "3"]
           (->> (apiserver/readyz-command 3) (drop-while #(not= "--max-time" %)) (take 2))))))

(deftest a-restart-is-refused-on-a-node-the-cluster-does-not-list-as-control-plane
  (let [cp (node "cp-1" {(keyword apiserver/control-plane-label) ""})]
    (is (nil? (apiserver/restart-refusal "cp-1" cp)))
    (is (= "the cluster no longer lists node cp-1, so it is not reached over SSH"
           (apiserver/restart-refusal "cp-1" nil)))
    (is (= "node w-1 carries no node-role.kubernetes.io/control-plane label"
           (apiserver/restart-refusal "w-1" (node "w-1" {:kubernetes.io/os "linux"}))))
    (is (apiserver/control-plane? cp))
    (is (not (apiserver/control-plane? (node "w-1" nil))))))

(deftest the-restart-leaves-the-nodes-crictl-configuration-alone
  (let [_spec (with-redefs [mutation/book identity]
                (load-file "runbooks/kubernetes/nodes/restart_apiserver.clj"))
        restart-effect (requiring-resolve 'runbooks.kubernetes.nodes.restart-apiserver/restart-effect)
        plan (restart-effect {:host "cp-1" :needs-sudo? true} "abc123")
        commands (map #(vec (drop 2 %)) plan)]
    (testing "the plan is the stop and nothing else"
      (is (= [["sudo" "crictl" "stop" "abc123"]] commands)))
    (testing "no step writes /etc/crictl.yaml"
      (is (not-any? #(some #{"config"} %) commands)))
    (is (nil? (resolve 'gumshoe.apiserver/configure-command)))))
