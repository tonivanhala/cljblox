(ns parser-test
  (:require
    [clojure.java.io :as io]
    [scanner :refer [reader->stream-scanner tokens]]
    [parser :as p]
    [clojure.test :refer [deftest testing is]]))

(def +equality-expected-tree+
  {:type ::p/BINARY :operator :+
   :left
   {:type ::p/GROUP
    :child
    {:type ::p/BINARY
     :operator :-
     :left {:type ::p/DECIMAL :value 5.0}
     :right
     {:type ::p/GROUP
      :child
      {:type ::p/BINARY :operator :-
       :left {:type ::p/DECIMAL :value 3.0}
       :right {:type ::p/DECIMAL :value 1.0}}}}}
   :right
   {:type ::p/GROUP
    :child
    {:type ::p/UNARY :operator :-
     :child {:type ::p/DECIMAL :value 1.0}}}})

(deftest parse-numbers
  (testing "extracts number tokens from a file"
    (let [test-file (io/reader (io/file (io/resource "numbers.blox")))
          scanner (reader->stream-scanner test-file)
          tokens-from-file (tokens scanner)
          syntax-tree (p/parse tokens-from-file)]
      (is (= +equality-expected-tree+ syntax-tree)))))
