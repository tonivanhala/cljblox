(ns scanner-test
  (:require
    [clojure.java.io :as io]
    [scanner :refer [reader->stream-scanner tokens]]
    [tokens :as t]
    [clojure.test :refer [deftest testing is]]))

(def +expected-number-tokens+
  [{:type ::t/INTEGER :lexeme "123" :literal 123}
   {:type ::t/DECIMAL :lexeme "123.456" :literal 123.456}
   {:type ::t/DECIMAL :lexeme ".456" :literal 0.456}
   {:type ::t/DECIMAL :lexeme "123." :literal 123.0}
   {:type ::t/EOF}])

(defn token-matches?
  [expected actual]
  (let [a (select-keys actual (keys expected))]
    (= expected a)))

(deftest parse-numbers
  (testing "extracts number tokens from a file"
    (let [test-file (io/reader (io/file (io/resource "numbers.blox")))
          scanner (reader->stream-scanner test-file)
          tokens-from-file (tokens scanner)]
      (doseq [[expected actual] (map #(list %1 %2) +expected-number-tokens+ tokens-from-file)]
        (is (token-matches? expected actual))))))
