(ns main
  (:gen-class)
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.tools.cli :refer [parse-opts]]
    [scanner :refer [reader->stream-scanner tokens]])
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
  [scanner]
  (doseq [token (tokens scanner)]
    (println token)))

(defn line->reader
  [^String line]
  (-> line
      (.getBytes)
      (ByteArrayInputStream.)
      (InputStreamReader.)
      (BufferedReader.)))

(defn get-user-input
  []
  (some-> (read-line) (str \newline) (line->reader)))

(defn run-prompt
  []
  (show-prompt)
  (loop [scanner (some-> (get-user-input) (reader->stream-scanner))]
    (when (some? scanner)
      (process-stream scanner)
      (show-prompt)
      (recur (some-> (get-user-input) (reader->stream-scanner scanner))))))

(defn exec-file
  [filename]
  (with-open [reader (io/reader (io/file filename))]
    (let [scanner (reader->stream-scanner reader)]
      (process-stream scanner))))

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
