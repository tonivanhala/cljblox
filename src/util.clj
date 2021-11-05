(ns util)

(defn assert-number!
  [x]
  (when (not (number? x))
    (throw (ex-info "Not a number" {:value x}))))

(defn assert-boolean!
  [x]
  (when (not (boolean? x))
    (throw (ex-info "Not a boolean" {:value x}))))
