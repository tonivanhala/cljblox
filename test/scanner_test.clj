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
   {:type ::t/OCTAL :lexeme "077" :literal 63}
   {:type ::t/HEXADECIMAL :lexeme "0xFF" :literal 255}
   {:type ::t/EOF}])

(def +expected-identifiers+
  ["andy"
   "formless"
   "fo"
   "_"
   "_123"
   "_abc"
   "ab123"
   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"])

(def +expected-keywords+
  [::t/AND
   ::t/CLASS
   ::t/ELSE
   ::t/FALSE
   ::t/FOR
   ::t/FUN
   ::t/IF
   ::t/NIL
   ::t/OR
   ::t/RETURN
   ::t/SUPER
   ::t/THIS
   ::t/TRUE
   ::t/VAR
   ::t/WHILE
   ::t/EOF])

(def +expected-punctuators+
  [::t/LEFT-PAREN
   ::t/RIGHT-PAREN
   ::t/LEFT-BRACE
   ::t/RIGHT-BRACE
   ::t/SEMICOLON
   ::t/COMMA
   ::t/PLUS
   ::t/MINUS
   ::t/STAR
   ::t/BANG-EQUAL
   ::t/EQUAL-EQUAL
   ::t/LESS-EQUAL
   ::t/GREATER-EQUAL
   ::t/BANG-EQUAL
   ::t/LESS
   ::t/GREATER
   ::t/SLASH
   ::t/DOT
   ::t/EOF])

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

(deftest parse-identifiers
  (testing "extracts identifiers"
    (let [test-file (io/reader (io/file (io/resource "identifiers.blox")))
          scanner (reader->stream-scanner test-file)
          tokens-from-file (tokens scanner)]
      (doseq [[expected actual]
              (map
                #(list {:lexeme %1 :type ::t/IDENTIFIER} %2) +expected-identifiers+ tokens-from-file)]
        (is (token-matches? expected actual))))))

(deftest parse-keywords
  (testing "extracts keywords"
    (let [test-file (io/reader (io/file (io/resource "keywords.blox")))
          scanner (reader->stream-scanner test-file)
          tokens-from-file (tokens scanner)]
      (doseq [[expected actual]
              (map
                #(list {:type %1} %2) +expected-keywords+ tokens-from-file)]
        (is (token-matches? expected actual))))))

(deftest parse-punctuators
  (testing "extracts punctuators"
    (let [test-file (io/reader (io/file (io/resource "punctuators.blox")))
          scanner (reader->stream-scanner test-file)
          tokens-from-file (tokens scanner)]
      (doseq [[expected actual]
              (map
                #(list {:type %1} %2) +expected-punctuators+ tokens-from-file)]
        (is (token-matches? expected actual))))))
