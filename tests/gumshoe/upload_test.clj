;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.upload-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.upload :as upload]
            [gumshoe.phpstack :as phpstack]
            [gumshoe.size :as size]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest size-parse-test
  (testing "suffixes are 1024-based, case-insensitive"
    (is (= 1048576 (size/parse "1M")))
    (is (= 1048576 (size/parse "1m")))
    (is (= 536870912 (size/parse "512M")))
    (is (= 2147483648 (size/parse "2G")))
    (is (= 102400 (size/parse "100k")))
    (is (= 500 (size/parse "500"))))
  (testing "-1 and 0 are unlimited, junk is nil"
    (is (= :unlimited (size/parse "-1")))
    (is (= :unlimited (size/parse "0")))
    (is (nil? (size/parse "lots"))))
  (testing "human rendering"
    (is (= "512M" (size/human 536870912)))
    (is (= "2.0G" (size/human 2147483648)))
    (is (= "unlimited" (size/human :unlimited)))
    (is (= "unknown" (size/human nil)))))

(deftest size-compare-test
  (testing "smaller? treats unlimited as largest and unknown as never-smaller"
    (is (size/smaller? 1048576 8388608))
    (is (not (size/smaller? 8388608 1048576)))
    (is (not (size/smaller? :unlimited 8388608)))
    (is (size/smaller? 8388608 :unlimited))
    (is (not (size/smaller? nil 8388608)))
    (is (not (size/smaller? 8388608 nil))))
  (testing "minimum ignores unlimited and nil"
    (is (= 1048576 (size/minimum [1048576 8388608 :unlimited])))
    (is (nil? (size/minimum [:unlimited nil])))))

(deftest nginx-parse-test
  (is (= ["512M" "1m"]
         (phpstack/nginx-body-sizes "http { client_max_body_size 512M; }\nlocation / { client_max_body_size 1m; }")))
  (is (= "2M" (phpstack/php-directive "upload_max_filesize => 2M => 2M" "upload_max_filesize"))))

(deftest analyze-nginx-bottleneck-test
  (testing "nginx at its 1M default in front of a generous PHP is the classic bottleneck"
    (let [findings (upload/analyze {:component "nextcloud"
                                    :nginx-body-size 1048576
                                    :nginx-explicit? false
                                    :upload-max 536870912
                                    :post-max 536870912
                                    :memory-limit 536870912})]
      (is (contains? (summaries findings)
                     "nginx caps uploads at 1M, below PHP's 512M - nginx rejects larger uploads with 413"))
      (is (contains? (summaries findings)
                     "effective upload limit is 1M (capped by nginx client_max_body_size)")))))

(deftest analyze-post-below-upload-test
  (testing "a POST cap under the file cap makes the file limit a mirage"
    (is (contains? (summaries (upload/analyze {:component "moodle"
                                               :nginx-body-size 536870912
                                               :nginx-explicit? true
                                               :upload-max 209715200
                                               :post-max 8388608
                                               :memory-limit 268435456}))
                   "PHP post_max_size (8M) is smaller than upload_max_filesize (200M) - uploads fail inside PHP"))))

(deftest analyze-coherent-test
  (testing "a coherent chain only reports the effective limit"
    (let [findings (upload/analyze {:component "app"
                                    :nginx-body-size 536870912
                                    :nginx-explicit? true
                                    :upload-max 536870912
                                    :post-max 536870912
                                    :memory-limit 1073741824})]
      (is (= [:info] (map :severity findings)))
      (is (contains? (summaries findings) "effective upload limit is 512M (capped by nginx client_max_body_size)")))))

(deftest parse-evidence-test
  (testing "unset nginx client_max_body_size falls back to the 1M default"
    (let [evidence (phpstack/parse-evidence {:component "app"
                                             :nginx-config "http { server { listen 80; } }"
                                             :php-info "upload_max_filesize => 2M => 2M\npost_max_size => 8M => 8M\nmemory_limit => 128M => 128M"})]
      (is (false? (:nginx-explicit? evidence)))
      (is (= 1048576 (:nginx-body-size evidence)))
      (is (= 2097152 (:upload-max evidence)))
      (is (:found-php? evidence)))))
