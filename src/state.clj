(ns state)

(defrecord RunningState [state])

(defn init-state
  []
  (->
    {:errors []}
    (RunningState.)))
