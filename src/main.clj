(ns main
  (:gen-class)
  (:require
    [clojure.java.io :as io]
    [clojure.tools.cli :refer [parse-opts]])
  (:import (java.io ByteArrayInputStream InputStreamReader BufferedReader)))

(defrecord RunningState [state-atom])

(def +cli-options+
  [])

(defn show-prompt
  ([]
   (show-prompt false))
  ([is-block-open]
   (if is-block-open
     (print "... ")
     (print "> "))
   (flush)))

(defn process-stream
  [^BufferedReader reader]
  (loop [ch (.read reader)]
    (when-not (= -1 ch)
      (print (char ch))
      (recur (.read reader)))))

(defn process-line
  [^String line]
  (-> line
      (.getBytes)
      (ByteArrayInputStream.)
      (InputStreamReader.)
      (BufferedReader.)
      (process-stream))
  (println))

(defn run-prompt
  []
  (let [_state (RunningState. (atom {}))]
    (show-prompt)
    (loop [line (read-line)]
      (when (some? line)
        (process-line line)
        (show-prompt)
        (recur (read-line))))))

(defn exec-file
  [filename]
  (let [_state (RunningState. (atom {}))]
    (with-open [reader (io/reader (io/file filename))]
      (process-stream reader))))

(defn -main
  [& args]
  (let [parsed-args (parse-opts args +cli-options+)
        errors (:errors parsed-args)
        arguments (:arguments parsed-args)]
    (when (not-empty errors)
      (doseq [error errors]
        (println error))
      (System/exit 64))
    (case (count arguments)
      0 (run-prompt)
      1 (exec-file (first arguments))
      (println
        "Too many arguments. Provide filename to execute or none for interactive prompt."))))