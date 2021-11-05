(ns interpreter
  (:require [parser :as p]
            [tokens :as t]
            [util :refer [assert-boolean!
                          assert-number!]]))

(defprotocol BinaryTerm
  (coerce-binary [this right]))

(extend-protocol BinaryTerm
  Double
  (coerce-binary
    [this right]
    (cond
      (double? right) [this right]
      (integer? right) [(double ^double this) (double ^Integer right)]
      (string? right) [(str this) right]))
  Long
  (coerce-binary
    [this right]
    (cond
      (double? right) [(double ^Integer this) (double ^Double right)]
      (integer? right) [this right]
      (string? right) [(double ^Integer this) right]))
  Integer
  (coerce-binary [this right] (coerce-binary (.longValue ^Integer this) right))
  String
  (coerce-binary
    [this right]
    [this (str right)]))


(defmulti eval-node :type)

(defmethod eval-node ::p/DECIMAL
  [node]
  (:value node))

(defmethod eval-node ::p/INTEGER
  [node]
  (:value node))

(defmethod eval-node ::p/STRING-LITERAL
  [node]
  (:value node))

(defmethod eval-node ::p/BOOLEAN
  [node]
  (:value node))

(defmethod eval-node ::p/GROUP
  [node]
  (eval-node (:child node)))

(defmethod eval-node ::p/UNARY
  [node]
  (let [child (:child node)
        child-value (eval-node child)]
    (case (:operator node)
      ::t/MINUS (do (assert-number! child-value)
                    (- child-value))
      ::t/BANG (do (assert-boolean! child-value)
                   (not child-value)))))

(defmethod eval-node ::p/BINARY
  [node]
  (let [left (:left node)
        right (:right node)
        operator (:operator node)
        left-value (eval-node left)
        right-value (eval-node right)
        [left-coerced right-coerced] (coerce-binary left-value right-value)]
    (case operator
      ::t/MINUS (- left-coerced right-coerced)
      ::t/SLASH (/ left-coerced right-coerced)
      ::t/STAR (* left-coerced right-coerced)
      ::t/PLUS (if (string? left-coerced)
                 (str left-coerced right-coerced)
                 (+ left-coerced right-coerced))
      ::t/GREATER (> left-coerced right-coerced)
      ::t/GREATER-EQUAL (>= left-coerced right-coerced)
      ::t/LESS (< left-coerced right-coerced)
      ::t/LESS-EQUAL (<= left-coerced right-coerced)
      ::t/BANG-EQUAL (not= left-coerced right-coerced)
      ::t/EQUAL-EQUAL (= left-coerced right-coerced))))
