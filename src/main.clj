(ns main
  (:gen-class)
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.tools.cli :refer [parse-opts]]
    [state :refer [init-state]])
  (:import (java.io ByteArrayInputStream InputStreamReader BufferedReader)))

(def +cli-options+
  [])

(defn print-banner
  []
  (let [print-multiline
        (fn -print-multiline [& strings]
          (->> strings
            (str/join (System/lineSeparator))
            (println)))]
    (print-multiline
      "BBBB  L     OOO  XX XX"
      "B   B L    O   O  X X "
      "BBBB  L    O   O   X  "
      "B   B L    O   O  X X "
      "BBBB  LLLL  OOO  XX XX"
      "Blox interpreter - \"Bacon-flavored Lox\"")))

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
  (let [_state (init-state)]
    (show-prompt)
    (loop [line (read-line)]
      (when (some? line)
        (process-line line)
        (show-prompt)
        (recur (read-line))))))

(defn exec-file
  [filename]
  (let [_state (init-state)]
    (with-open [reader (io/reader (io/file filename))]
      (process-stream reader))))

(defn -main
  [& args]
  (let [parsed-args (parse-opts args +cli-options+)
        errors (:errors parsed-args)
        arguments (:arguments parsed-args)]
    (print-banner)
    (when (not-empty errors)
      (doseq [error errors]
        (println error))
      (System/exit 64))
    (case (count arguments)
      0 (run-prompt)
      1 (exec-file (first arguments))
      (println
        "Too many arguments. Provide filename to execute or none for interactive prompt."))))
