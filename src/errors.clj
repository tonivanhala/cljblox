(ns errors
  (:import (clojure.lang IPersistentMap)))

(defrecord ScanError
  [line column type message])

(defn add-error
  [state ^IPersistentMap error]
  (update state :errors #(conj % (map->ScanError error))))

(defn has-errors
  [state]
  (->> state :errors count (< 0)))

(defn report-error
  [error]
  (println
    (str "[" (:line error) "," (:column error) "]: " (:message error))))

(defn report-errors
  [state]
  (doseq [error (:errors state)]
    (report-error error)))
