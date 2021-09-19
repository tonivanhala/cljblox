(ns tokens)

(defrecord Token
  [type lexeme literal line column])
