(ns smoke-test
  (:require
    [clojure.test :refer [deftest testing is]]))

(deftest smoke-test
  (testing "1 plus 1 still equals 2"
    (is (= 2 (+ 1 1)))))
