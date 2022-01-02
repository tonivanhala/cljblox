(ns tokens)

(defrecord Token
  [type lexeme literal line column])

(defn paren?
  [{:keys [type]}]
  (or
    (= ::LEFT-BRACE type)
    (= ::RIGHT-BRACE type)
    (= ::LEFT-PAREN type)
    (= ::RIGHT-PAREN type)))

(defn open-paren?
  [{:keys [type]}]
  (or
    (= ::LEFT-BRACE type)
    (= ::LEFT-PAREN type)))
