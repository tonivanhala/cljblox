(ns interpreter-test
  (:require
    [clojure.java.io :as io]
    [scanner :refer [reader->stream-scanner tokens]]
    [parser :as p]
    [interpreter :as i]
    [clojure.test :refer [deftest testing is]]))

(deftest interpret-arithmetic
  (testing "evaluates arithmetic expression"
    (let [test-file (io/reader (io/file (io/resource "arithmetic.blox")))
          scanner (reader->stream-scanner test-file)
          tokens-from-file (tokens scanner)
          syntax-tree (p/parse tokens-from-file)]
      (is (= 2 (i/eval-node syntax-tree))))))
