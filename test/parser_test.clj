(ns parser-test
  (:require
    [clojure.java.io :as io]
    [scanner :refer [reader->stream-scanner tokens]]
    [parser]
    [clojure.test :refer [deftest testing is]]))

(def +equality-expected-tree+
  {:type :binary :operator :+
   :left
   {:type :group
    :child
    {:type :binary
     :operator :-
     :left {:type :decimal :value 5.0}
     :right
     {:type :group
      :child
      {:type :binary :operator :-
       :left {:type :decimal :value 3.0}
       :right {:type :decimal :value 1.0}}}}}
   :right
   {:type :group
    :child
    {:type :unary :operator :-
     :child {:type :decimal :value 1.0}}}})

(deftest parse-numbers
  (testing "extracts number tokens from a file"
    (let [test-file (io/reader (io/file (io/resource "numbers.blox")))
          scanner (reader->stream-scanner test-file)
          tokens-from-file (tokens scanner)
          syntax-tree (parser/parse tokens-from-file)]
      (is (= +equality-expected-tree+ syntax-tree)))))
