(ns parser
  (:require
    [parser :as p]
    [tokens :as t]))

(declare parse-expression)

(defn parse-primary
  [prev-tokens token-stream]
  (let [next-token (first token-stream)
        next-type (:type next-token)
        -prev-tokens (conj prev-tokens next-token)
        -token-stream (rest token-stream)]
    (or (case next-type
          ::t/FALSE [-prev-tokens
                     -token-stream
                     {:type ::p/BOOLEAN
                      :value false}]
          ::t/TRUE [-prev-tokens
                    -token-stream
                    {:type ::p/BOOLEAN
                     :value true}]
          ::t/NIL [-prev-tokens
                   -token-stream
                   {:type ::p/BOOLEAN
                    :value nil}]
          false)
        (when (= ::t/DECIMAL next-type)
          [-prev-tokens
           -token-stream
           {:type ::p/DECIMAL
            :value (:literal next-token)}])
        (when (= ::t/STRING-LITERAL next-type)
          [-prev-tokens
           -token-stream
           {:type ::p/STRING-LITERAL
            :value (:literal next-token)}])
        (when (get
                #{::t/HEXADECIMAL ::t/INTEGER ::t/OCTAL}
                next-type)
          [-prev-tokens
           -token-stream
           {:type ::p/INTEGER
            :value (:literal next-token)}])
        (when (= ::t/LEFT-PAREN next-type)
          (let [[-prev -stream expr] (parse-expression (conj prev-tokens next-token) (rest token-stream))
                -maybe-paren (first -stream)]
            (if (= ::t/RIGHT-PAREN (:type -maybe-paren))
              [(conj -prev -maybe-paren)
               (rest -stream)
               {:type ::p/GROUP
                :child expr}]
              (throw
                (ex-info
                  "Expected ')' after expression"
                  {:expected ")"
                   :actual -maybe-paren}))))))))


(defn parse-unary
  [prev-tokens token-stream]
  (let [next-token (first token-stream)
        next-type (:type next-token)]
    (if-let [op (get #{::t/BANG ::t/MINUS} next-type)]
      (let [[--prev-tokens --token-stream --expr]
            (parse-unary
              (conj prev-tokens next-token)
              (rest token-stream))]
        [--prev-tokens
         --token-stream
         {:type ::p/UNARY
          :operator op
          :child --expr}])
      (parse-primary prev-tokens token-stream))))

(defn ->parse-binary-expressions
  [parse-fn & -operators]
  (let [operators (into #{} -operators)]
    (fn -parse-expressions
      [prev-tokens token-stream]
      (let [[-prev -stream -expr] (parse-fn prev-tokens token-stream)]
        (loop [prev -prev
               stream -stream
               expr -expr]
          (let [next-token (first stream)
                next-type (:type next-token)]
            (if-let [op (get operators next-type)]
              (let [[--prev --stream --expr]
                    (parse-fn (conj prev next-token) (rest stream))]
                (recur --prev --stream {:type ::p/BINARY
                                        :operator op
                                        :left expr
                                        :right --expr}))
              [prev stream expr])))))))

(def parse-factor (->parse-binary-expressions parse-unary ::t/SLASH ::t/STAR))

(def parse-term (->parse-binary-expressions parse-factor ::t/MINUS ::t/PLUS))

(def parse-comparison
  (->parse-binary-expressions
    parse-term
    ::t/GREATER
    ::t/GREATER-EQUAL
    ::t/LESS
    ::t/LESS-EQUAL))

(def parse-equality
  (->parse-binary-expressions
    parse-comparison
    ::t/BANG-EQUAL
    ::t/EQUAL-EQUAL))

(defn parse-expression
  [prev-tokens token-stream]
  (parse-equality prev-tokens token-stream))

(defn parse
  [token-stream]
  (let [[_prev _stream expr] (parse-expression [] token-stream)]
    expr))
