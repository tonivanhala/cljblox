(ns parser
  (:require
    [parser :as p]
    [tokens :as t]))

(declare parse-expression)

(defn parse-primary
  [prev-tokens token-stream]
  (let [next-token (first token-stream)
        next-type (:type next-token)]
    (or (case next-type
          ::t/FALSE {:type ::p/LITERAL
                     :value false}
          ::t/TRUE {:type ::p/LITERAL
                    :value true}
          ::t/NIL {:type ::p/LITERAL
                   :value nil}
          false)
        (when (get
                #{::t/DECIMAL ::t/HEXADECIMAL ::t/INTEGER ::t/OCTAL ::t/STRING-LITERAL}
                next-type)
          [(conj prev-tokens next-token)
           (rest token-stream)
           {:type ::p/LITERAL
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
      [(conj prev-tokens next-token)
       (rest token-stream)
       {:type ::p/UNARY
        :operator op
        :right (parse-unary prev-tokens token-stream)}]
      (parse-primary prev-tokens token-stream))))

(defn parse-factor
  [prev-tokens token-stream]
  (let [[-prev -stream -expr]
        (parse-unary prev-tokens token-stream)]
    (loop [prev -prev
           stream -stream
           expr -expr]
      (let [prev-token (last prev)
            prev-type (:type prev-token)
            [--prev --stream right]
            (parse-unary prev stream)]
        (if-let [op (get #{::t/SLASH ::t/STAR} prev-type)]
          (recur --prev --stream {:type ::p/BINARY
                                  :operator op
                                  :left expr
                                  :right right})
          [prev stream expr])))))

(defn parse-term
  [prev-tokens token-stream]
  (let [[-prev -stream -expr]
        (parse-factor prev-tokens token-stream)]
    (loop [prev -prev
           stream -stream
           expr -expr]
      (let [prev-token (last prev)
            prev-type (:type prev-token)
            [--prev --stream right]
            (parse-factor prev stream)]
        (if-let [op (get #{::t/MINUS ::t/PLUS} prev-type)]
          (recur --prev --stream {:type ::p/BINARY
                                  :operator op
                                  :left expr
                                  :right right})
          [prev stream expr])))))


(defn parse-comparison
  [prev-tokens token-stream]
  (let [[-prev -stream -expr]
        (parse-term prev-tokens token-stream)]
    (loop [prev -prev
           stream -stream
           expr -expr]
      (let [prev-token (last prev)
            prev-type (:type prev-token)
            [--prev --stream right]
            (parse-term prev stream)]
        (if-let [op
                 (get
                   #{::t/GREATER
                     ::t/GREATER-EQUAL
                     ::t/LESS
                     ::t/LESS-EQUAL}
                   prev-type)]
          (recur
            --prev
            --stream
            {:type ::p/BINARY
             :operator op
             :left expr
             :right right})
          [prev stream expr])))))


(defn parse-equality
  [prev-tokens token-stream]
  (let [[-prev -stream -expr]
        (parse-comparison prev-tokens token-stream)]
    (loop [prev -prev
           stream -stream
           expr -expr]
      (let [prev-token (last prev)
            prev-type (:type prev-token)
            [--prev --stream right]
            (parse-comparison prev stream)]
        (if (or (= ::t/BANG-EQUAL prev-type) (= ::t/EQUAL-EQUAL prev-type))
          (recur
            --prev
            --stream
            {:type ::p/BINARY :operator prev-type
             :left expr
             :right right})
          [prev stream expr])))))

(defn parse-expression
  [prev-tokens token-stream]
  (parse-equality prev-tokens token-stream))

(defn parse
  [token-stream]
  (let [[_prev _stream expr] (parse-expression [] token-stream)]
    expr))
