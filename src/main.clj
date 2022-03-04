(ns main
  (:gen-class)
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.tools.cli :refer [parse-opts]]
    [interpreter :as i]
    [parser :as p]
    [scanner :refer [reader->stream-scanner tokens]]
    [tokens :as t])
  (:import (java.io ByteArrayInputStream InputStreamReader BufferedReader)
           (clojure.lang ExceptionInfo)))

(def +cli-options+
  [])

(def +initial-prompt-state+
  {:open-parens '()
   :tokens []})

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

(defn block-open?
  [{:keys [open-parens]}]
  (seq open-parens))

(defn show-prompt
  ([]
   (show-prompt +initial-prompt-state+))
  ([prompt-state]
   (if (block-open? prompt-state)
     (print "... ")
     (print "> "))
   (flush)
   prompt-state))

(defn process-line
  [prompt-state]
  (if (not (block-open? prompt-state))
    (do
      (some->
        prompt-state
        :tokens
        (p/parse)
        (i/eval-node)
        (println))
      +initial-prompt-state+)
    prompt-state))

(defn safe-process-line
  [prompt-state]
  (try
    (process-line prompt-state)
    (catch ExceptionInfo e
      (println (ex-message e))
      (println (ex-data e))
      +initial-prompt-state+)))

(defn closing-paren?
  [{prev :type} {this :type}]
  (or
    (and (= ::t/LEFT-PAREN prev) (= ::t/RIGHT-PAREN this))
    (and (= ::t/LEFT-BRACE prev) (= ::t/RIGHT-BRACE this))))

(defn process-paren
  [{:keys [open-parens] :as prompt-state} paren]
  (if (t/open-paren? paren)
    (-> prompt-state
        (update :open-parens conj paren)
        (update :tokens conj paren))
    (let [latest-paren (first open-parens)]
      (if (closing-paren? latest-paren paren)
        (-> prompt-state
            (update :open-parens rest)
            (update :tokens conj paren))
        (throw (ex-info "Mismatched parenthesis." {:token paren}))))))

(defn process-stream
  [scanner prompt-state]
  (reduce
    (fn [-prompt-state {:keys [type] :as token}]
      (cond
        (t/paren? token) (process-paren -prompt-state token)
        (not= ::t/EOF type) (update -prompt-state :tokens conj token)
        :else -prompt-state))
    prompt-state
    (tokens scanner)))

(defn safe-process-stream
  "Process stream while catching and printing out exceptions"
  [& args]
  (try
    (apply process-stream args)
    (catch ExceptionInfo e
      (println (ex-message e))
      (println (ex-data e))
      +initial-prompt-state+)))

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
  (loop [prompt-state (show-prompt)
         scanner (some-> (get-user-input) (reader->stream-scanner))]
    (when (some? scanner)
      (let [-prompt-state (safe-process-stream scanner prompt-state)]
        (-> -prompt-state
            (safe-process-line)
            (show-prompt)
            (recur
              (some-> (get-user-input) (reader->stream-scanner scanner))))))))

(defn exec-file
  [filename]
  (with-open [reader (io/reader (io/file filename))]
    (let [scanner (reader->stream-scanner reader)
          result (process-stream scanner +initial-prompt-state+)]
      (when (block-open? result)
        (throw
          (ex-info
            "Unclosed block in file."
            {:token (first (:open-parens result))})))
      (-> result
          :tokens
          p/parse
          i/eval-node))))

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
