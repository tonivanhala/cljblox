(ns errors
  (:require [state :refer [RunningState]]))

(defrecord Error
  [line column message])

(defn add-error
  [^RunningState state error]
  (update state :state #(update % conj error)))

(defn has-errors
  [^RunningState state]
  (->> state :errors count (< 0)))

(defn report-error
  [^Error error]
  (println
    (str "[" (:line error) "," (:column error) "]: " (:message error))))

(defn report-errors
  [^RunningState state]
  (doseq [error (:errors state)]
    (report-error error)))
