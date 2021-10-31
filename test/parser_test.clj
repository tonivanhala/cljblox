(ns parser-test
  (:require
    [clojure.java.io :as io]
    [scanner :refer [reader->stream-scanner tokens]]
    [parser :as p]
    [clojure.test :refer [deftest testing is]]))

(def +equality-expected-tree+
  {:type ::p/BINARY :operator :tokens/PLUS
   :left
   {:type ::p/GROUP
    :child
    {:type ::p/BINARY
     :operator :tokens/MINUS
     :left {:type ::p/INTEGER :value 5}
     :right
     {:type ::p/GROUP
      :child
      {:type ::p/BINARY :operator :tokens/MINUS
       :left {:type ::p/DECIMAL :value 3.0}
       :right {:type ::p/INTEGER :value 1}}}}}
   :right
   {:type ::p/UNARY :operator :tokens/MINUS
    :child {:type ::p/INTEGER :value 1}}})

(deftest parse-numbers
  (testing "extracts number tokens from a file"
    (let [test-file (io/reader (io/file (io/resource "equality.blox")))
          scanner (reader->stream-scanner test-file)
          tokens-from-file (tokens scanner)
          syntax-tree (p/parse tokens-from-file)]
      (is (= +equality-expected-tree+ syntax-tree)))))
