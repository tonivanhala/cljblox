(ns scanner
  (:require
    [clojure.string :as str]
    [errors :as e]
    [tokens :as t])
  (:import (java.io BufferedReader)))

(defprotocol Scanner
  (next-token [this]))

(defn buffer->single-char-token
  [buffer]
  (condp = (first @buffer)
    \( ::t/LEFT-PAREN
    \) ::t/RIGHT-PAREN
    \{ ::t/LEFT-BRACE
    \} ::t/RIGHT-BRACE
    \, ::t/COMMA
    \- ::t/MINUS
    \+ ::t/PLUS
    \; ::t/SEMICOLON
    \* ::t/STAR
    nil))

(def +reserved-words+
  {"and" ::t/AND
   "class" ::t/CLASS
   "else" ::t/ELSE
   "false" ::t/false
   "for" ::t/FOR
   "fun" ::t/FUN
   "if" ::t/IF
   "nil" ::t/NIL
   "or" ::t/OR
   "print" ::t/PRINT
   "return" ::t/RETURN
   "super" ::t/SUPER
   "this" ::t/THIS
   "true" ::t/TRUE
   "var" ::t/VAR
   "while" ::t/WHILE})

(defn get-scanner-state
  [state]
  (:scanner @state))

(defn set-scanner-state
  [state scanner-state]
  (swap! state #(assoc % :scanner scanner-state)))

(defn whitespace?
  [ch]
  (or (= \space ch)
      (= \tab ch)
      (= \return ch)))

(defn digit?
  [ch]
  (<= (int \0) (int ch) (int \9)))

(defn alpha?
  [ch]
  (or (<= (int \a) (int ch) (int \z))
      (<= (int \A) (int ch) (int \Z))
      (= \_ ch)))

(defn alpha-numeric?
  [ch]
  (or (alpha? ch) (digit? ch)))

(defn handle-token-break
  [line column buffer state]
  (let [first-char (str (first @buffer))]
    (case (get-scanner-state state)
      ::STRING-LITERAL
      (do
        (swap! state e/add-error {:line @line
                                  :column @column
                                  :type ::e/UNTERMINATED-STRING-LITERAL})
        nil)
      ::EQUAL
      (t/map->Token {:type ::t/EQUAL
                     :lexeme first-char
                     :line @line
                     :column (- @column (count @buffer))})
      ::BANG
      (t/map->Token {:type ::t/BANG
                     :lexeme first-char
                     :line @line
                     :column (- @column (count @buffer))})
      ::LESS
      (t/map->Token {:type ::t/LESS
                     :lexeme first-char
                     :line @line
                     :column (- @column (count @buffer))})
      ::GREATER
      (t/map->Token {:type ::t/GREATER
                     :lexeme first-char
                     :line @line
                     :column (- @column (count @buffer))})
      ::SLASH
      (t/map->Token {:type ::t/SLASH
                     :lexeme first-char
                     :line @line
                     :column (dec @column)})
      ::DOT
      (t/map->Token {:type ::t/DOT
                     :lexeme first-char
                     :line @line
                     :column (dec @column)})
      ::NUMBER
      (let [lexeme (->> @buffer
                        (str/join ""))
            number (Integer/valueOf ^String lexeme)]
        (t/map->Token {:type ::t/INTEGER
                       :lexeme lexeme
                       :literal number
                       :line @line
                       :column @column}))
      ::DECIMAL
      (let [^String lexeme (->> @buffer
                                (str/join ""))
            number
            (Double/valueOf ^String (cond->> lexeme
                                       (= \. (first lexeme))
                                       (str "0")))]
        (t/map->Token {:type ::t/DECIMAL
                       :lexeme lexeme
                       :literal number
                       :line @line
                       :column @column}))
      ::IDENTIFIER
      (let [lexeme (->> @buffer
                       (str/join ""))]
        (t/map->Token {:type (get +reserved-words+ lexeme ::t/IDENTIFIER)
                       :lexeme lexeme
                       :line @line
                       :column (dec @column)}))
      nil)))

(defn buffer->token
  [line column buffer state]
  (when (not-empty @buffer)
    (let [single-char-token (buffer->single-char-token buffer)]
      (case (get-scanner-state state)
        ::FRESH
        (let [first-char (first @buffer)]
          (cond
            (some? single-char-token)
            (let [token (t/map->Token {:type single-char-token
                                       :lexeme @buffer
                                       :line @line
                                       :column (- @column (count @buffer))})]
              (reset! buffer "")
              token)

            (= \" first-char)
            (do
              (set-scanner-state state ::STRING-LITERAL)
              nil)

            (= \= first-char)
            (do
              (set-scanner-state state ::EQUAL)
              nil)

            (= \! first-char)
            (do
              (set-scanner-state state ::BANG)
              nil)

            (= \< first-char)
            (do
              (set-scanner-state state ::LESS)
              nil)

            (= \> first-char)
            (do
              (set-scanner-state state ::GREATER)
              nil)

            (= \/ first-char)
            (do
              (set-scanner-state state ::SLASH)
              nil)

            (= \tab first-char)
            (do
              (set-scanner-state state ::WHITESPACE)
              nil)

            (= \return first-char)
            (do
              (set-scanner-state state ::WHITESPACE)
              nil)

            (= \space first-char)
            (do
              (set-scanner-state state ::WHITESPACE)
              nil)

            (digit? first-char)
            (do
              (set-scanner-state state ::NUMBER)
              nil)

            (= \. first-char)
            (do
              (set-scanner-state state ::DOT)
              nil)

            (alpha? first-char)
            (do
              (set-scanner-state state ::IDENTIFIER)
              nil)))

        ::STRING-LITERAL
        (when
          (= \" (last @buffer))
          (let [token (t/map->Token {:type ::t/STRING-LITERAL
                                     :lexeme @buffer
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (reset! buffer "")
            (set-scanner-state state ::FRESH)
            token))

        ::EQUAL
        (if (= "==" @buffer)
          (let [token (t/map->Token {:type ::t/EQUAL-EQUAL
                                     :lexeme @buffer
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (reset! buffer "")
            (set-scanner-state state ::FRESH)
            token)
          (let [token (t/map->Token {:type ::t/EQUAL
                                     :lexeme (str (first @buffer))
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (swap! buffer #(str/join "" (rest %)))
            (set-scanner-state state ::FRESH)
            token))

        ::BANG
        (if (= "!=" @buffer)
          (let [token (t/map->Token {:type ::t/BANG-EQUAL
                                     :lexeme @buffer
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (reset! buffer "")
            (set-scanner-state state ::FRESH)
            token)
          (let [token (t/map->Token {:type ::t/BANG
                                     :lexeme (str (first @buffer))
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (swap! buffer #(str/join "" (rest %)))
            (set-scanner-state state ::FRESH)
            token))

        ::LESS
        (if (= "<=" @buffer)
          (let [token (t/map->Token {:type ::t/LESS-EQUAL
                                     :lexeme @buffer
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (reset! buffer "")
            (set-scanner-state state ::FRESH)
            token)
          (let [token (t/map->Token {:type ::t/LESS
                                     :lexeme (str (first @buffer))
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (swap! buffer #(str/join "" (rest %)))
            (set-scanner-state state ::FRESH)
            token))

        ::GREATER
        (if (= ">=" @buffer)
          (let [token (t/map->Token {:type ::t/GREATER-EQUAL
                                     :lexeme @buffer
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (reset! buffer "")
            (set-scanner-state state ::FRESH)
            token)
          (let [token (t/map->Token {:type ::t/GREATER
                                     :lexeme (str (first @buffer))
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (swap! buffer #(str/join "" (rest %)))
            (set-scanner-state state ::FRESH)
            token))

        ::SLASH
        (if (= "//" @buffer)
          (do
            (set-scanner-state state ::COMMENT)
            nil)
          (let [token (t/map->Token {:type ::t/SLASH
                                     :lexeme (str (first @buffer))
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (swap! buffer #(str/join "" (rest %)))
            (set-scanner-state state ::FRESH)
            token))

        ::COMMENT
        nil

        ::WHITESPACE
        (let [ch (last @buffer)]
          (when (not (whitespace? ch))
            (reset! buffer (str ch))
            (set-scanner-state state ::FRESH)
            (buffer->token line column buffer state)))

        ::NUMBER
        (let [ch (last @buffer)]
          (cond
            (= \. ch)
            (do
              (set-scanner-state state ::DECIMAL)
              nil)

            (not (digit? ch))
            (let [lexeme (->> @buffer
                              (drop-last)
                              (str/join ""))
                  number (Integer/valueOf ^String lexeme)]
              (swap! buffer #(str (last %)))
              (set-scanner-state state ::FRESH)
              (t/map->Token {:type ::t/INTEGER
                             :lexeme lexeme
                             :literal number
                             :line @line
                             :column @column}))
            :else
            nil))

        ::DECIMAL
        (let [ch (last @buffer)]
          (when (not (digit? ch))
            (let [^String lexeme (->> @buffer
                                      (drop-last)
                                      (str/join ""))
                  number
                  (Double/valueOf ^String (cond->> lexeme
                                            (= \. (first lexeme))
                                            (str "0")))]
              (swap! buffer #(str (last %)))
              (set-scanner-state state ::FRESH)
              (t/map->Token {:type ::t/DECIMAL
                             :lexeme lexeme
                             :literal number
                             :line @line
                             :column @column}))))

        ::DOT
        (let [ch (last @buffer)]
          (if (digit? ch)
            (do
              (set-scanner-state state ::DECIMAL)
              nil)
            (let [token (t/map->Token {:type ::t/DOT
                                       :lexeme (str (first @buffer))
                                       :line @line
                                       :column (dec @column)})]
              (swap! buffer #(str/join "" (rest %)))
              (set-scanner-state state ::FRESH)
              token)))

        ::IDENTIFIER
        (let [ch (last @buffer)]
          (when (not (alpha-numeric? ch))
            (let [lexeme (->> @buffer
                              (drop-last)
                              (str/join ""))]
              (swap! buffer #(str (last %)))
              (set-scanner-state state ::FRESH)
              (t/map->Token {:type (get +reserved-words+ lexeme ::t/IDENTIFIER)
                             :lexeme lexeme
                             :line @line
                             :column (dec @column)}))))))))


(defrecord StreamScanner
  [line column buffer state ^BufferedReader reader]
  Scanner
  (next-token [_this]
    (or
      (buffer->token line column buffer state)
      (loop [ch (.read reader)]
        (if (= -1 ch)
          (if-let [token (and (not-empty @buffer) (handle-token-break line column buffer state))]
            (do
              (reset! buffer "")
              token)
            (t/->Token ::t/EOF nil nil @line @column))
          (if (not= ch 10)
            (do (swap! column inc)
                (swap! buffer str (char ch))
                (or (buffer->token line column buffer state)
                    (recur (.read reader))))
            (let [token (and (not-empty @buffer) (handle-token-break line column buffer state))]
              (swap! line inc)
              (reset! column 1)
              (reset! buffer "")
              (set-scanner-state state ::FRESH)
              (or token
                (recur (.read reader))))))))))

(defn reader->stream-scanner
  ([^BufferedReader reader]
   (reader->stream-scanner reader {:line (atom 1)
                                   :column (atom 1)
                                   :buffer (atom "")
                                   :state (atom {:errors [] :scanner ::FRESH})}))
  ([^BufferedReader reader scanner-params]
   (map->StreamScanner
     (assoc scanner-params :reader reader))))

(defn tokens
  [^StreamScanner scanner]
  (lazy-seq
    (when-let [token (next-token scanner)]
      (if (= ::t/EOF (:type token))
        (list token)
        (cons token (tokens scanner))))))
