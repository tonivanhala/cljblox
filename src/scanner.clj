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
   "false" ::t/FALSE
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

(defn octal?
  [ch]
  (<= (int \0) (int ch) (int \7)))

(defn hexadecimal?
  [ch]
  (or
    (digit? ch)
    (<= (int \a) (int ch) (int \f))
    (<= (int \A) (int ch) (int \F))))

(defn handle-token-break
  [line column buffer state]
  (let [first-char (str (first @buffer))]
    (case @state
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
      ::OCTAL
      (let [lexeme (->> @buffer
                        (str/join ""))
            number (Integer/valueOf ^String lexeme 8)]
        (t/map->Token {:type ::t/OCTAL
                       :lexeme lexeme
                       :literal number
                       :line @line
                       :column @column}))
      ::HEX
      (let [lexeme (->> @buffer
                        (str/join ""))
            number-part (.substring lexeme 2)
            number (Integer/valueOf ^String number-part 16)]
        (t/map->Token {:type ::t/HEXADECIMAL
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

      ::ZERO
      (t/map->Token {:type ::t/INTEGER
                     :lexeme first-char
                     :literal 0
                     :line @line
                     :column @column})

      nil)))


(defn buffer->token
  [line column buffer state]
  (when (not-empty @buffer)
    (let [single-char-token (buffer->single-char-token buffer)]
      (case @state
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
              (reset! state ::STRING-LITERAL)
              nil)

            (= \= first-char)
            (do
              (reset! state ::EQUAL)
              nil)

            (= \! first-char)
            (do
              (reset! state ::BANG)
              nil)

            (= \< first-char)
            (do
              (reset! state ::LESS)
              nil)

            (= \> first-char)
            (do
              (reset! state ::GREATER)
              nil)

            (= \/ first-char)
            (do
              (reset! state ::SLASH)
              nil)

            (= \tab first-char)
            (do
              (reset! state ::WHITESPACE)
              nil)

            (= \return first-char)
            (do
              (reset! state ::WHITESPACE)
              nil)

            (= \space first-char)
            (do
              (reset! state ::WHITESPACE)
              nil)

            (= \0 first-char)
            (do
              (reset! state ::ZERO)
              nil)

            (digit? first-char)
            (do
              (reset! state ::NUMBER)
              nil)

            (= \. first-char)
            (do
              (reset! state ::DOT)
              nil)

            (alpha? first-char)
            (do
              (reset! state ::IDENTIFIER)
              nil)))

        ::STRING-LITERAL
        (when
          (= \" (last @buffer))
          (let [length (count @buffer)
                token (t/map->Token {:type ::t/STRING-LITERAL
                                     :lexeme @buffer
                                     :literal (.substring @buffer 1 (- length 1))
                                     :line @line
                                     :column (- @column length)})]
            (reset! buffer "")
            (reset! state ::FRESH)
            token))

        ::EQUAL
        (if (= "==" @buffer)
          (let [token (t/map->Token {:type ::t/EQUAL-EQUAL
                                     :lexeme @buffer
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (reset! buffer "")
            (reset! state ::FRESH)
            token)
          (let [token (t/map->Token {:type ::t/EQUAL
                                     :lexeme (str (first @buffer))
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (swap! buffer #(str/join "" (rest %)))
            (reset! state ::FRESH)
            token))

        ::BANG
        (if (= "!=" @buffer)
          (let [token (t/map->Token {:type ::t/BANG-EQUAL
                                     :lexeme @buffer
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (reset! buffer "")
            (reset! state ::FRESH)
            token)
          (let [token (t/map->Token {:type ::t/BANG
                                     :lexeme (str (first @buffer))
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (swap! buffer #(str/join "" (rest %)))
            (reset! state ::FRESH)
            token))

        ::LESS
        (if (= "<=" @buffer)
          (let [token (t/map->Token {:type ::t/LESS-EQUAL
                                     :lexeme @buffer
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (reset! buffer "")
            (reset! state ::FRESH)
            token)
          (let [token (t/map->Token {:type ::t/LESS
                                     :lexeme (str (first @buffer))
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (swap! buffer #(str/join "" (rest %)))
            (reset! state ::FRESH)
            token))

        ::GREATER
        (if (= ">=" @buffer)
          (let [token (t/map->Token {:type ::t/GREATER-EQUAL
                                     :lexeme @buffer
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (reset! buffer "")
            (reset! state ::FRESH)
            token)
          (let [token (t/map->Token {:type ::t/GREATER
                                     :lexeme (str (first @buffer))
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (swap! buffer #(str/join "" (rest %)))
            (reset! state ::FRESH)
            token))

        ::SLASH
        (if (= "//" @buffer)
          (do
            (reset! state ::COMMENT)
            nil)
          (let [token (t/map->Token {:type ::t/SLASH
                                     :lexeme (str (first @buffer))
                                     :line @line
                                     :column (- @column (count @buffer))})]
            (swap! buffer #(str/join "" (rest %)))
            (reset! state ::FRESH)
            token))

        ::COMMENT
        nil

        ::WHITESPACE
        (let [ch (last @buffer)]
          (when (not (whitespace? ch))
            (reset! buffer (str ch))
            (reset! state ::FRESH)
            (buffer->token line column buffer state)))

        ::NUMBER
        (let [ch (last @buffer)]
          (cond
            (= \. ch)
            (do
              (reset! state ::DECIMAL)
              nil)

            (not (digit? ch))
            (let [lexeme (->> @buffer
                              (drop-last)
                              (str/join ""))
                  number (Integer/valueOf ^String lexeme)]
              (swap! buffer #(str (last %)))
              (reset! state ::FRESH)
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
              (reset! state ::FRESH)
              (t/map->Token {:type ::t/DECIMAL
                             :lexeme lexeme
                             :literal number
                             :line @line
                             :column @column}))))

        ::DOT
        (let [ch (last @buffer)]
          (if (digit? ch)
            (do
              (reset! state ::DECIMAL)
              nil)
            (let [token (t/map->Token {:type ::t/DOT
                                       :lexeme (str (first @buffer))
                                       :line @line
                                       :column (dec @column)})]
              (swap! buffer #(str/join "" (rest %)))
              (reset! state ::FRESH)
              token)))

        ::IDENTIFIER
        (let [ch (last @buffer)]
          (when (not (alpha-numeric? ch))
            (let [lexeme (->> @buffer
                              (drop-last)
                              (str/join ""))]
              (swap! buffer #(str (last %)))
              (reset! state ::FRESH)
              (t/map->Token {:type (get +reserved-words+ lexeme ::t/IDENTIFIER)
                             :lexeme lexeme
                             :line @line
                             :column (dec @column)}))))

        ::ZERO
        (let [ch (last @buffer)]
          (cond
            (digit? ch)
            (do
              (reset! state ::OCTAL)
              nil)

            (= \x ch)
            (do
              (reset! state ::HEX)
              nil)

            (= \. ch)
            (do
              (reset! state ::DECIMAL)
              nil)

            :else
            (do
              (reset! state ::FRESH)
              (swap! buffer #(str/join "" (rest %)))
              (t/map->Token {:type ::t/INTEGER
                             :lexeme "0"
                             :literal 0
                             :line @line
                             :column @column}))))

        ::OCTAL
        (let [ch (last @buffer)]
          (cond
            (octal? ch)
            nil
            (digit? ch)
            (do
              (swap! state e/add-error {:line @line
                                        :column @column
                                        :type ::e/INVALID-OCTAL-NUMBER})
              (reset! buffer "")
              (reset! state ::FRESH)
              nil)
            :else
            (let [lexeme (->> @buffer
                              (drop-last)
                              (str/join ""))
                  number-part (.substring lexeme 1)
                  number (Integer/parseInt ^String number-part 8)]
              (reset! state ::FRESH)
              (swap! buffer #(str (last %)))
              (t/map->Token {:type ::t/OCTAL
                             :lexeme lexeme
                             :literal number
                             :line @line
                             :column @column}))))

        ::HEX
        (let [ch (last @buffer)]
          (cond
            (hexadecimal? ch)
            nil
            (alpha-numeric? ch)
            (do
              (swap! state e/add-error {:type ::e/INVALID-HEXADECIMAL
                                        :line @line
                                        :column @column})
              (reset! buffer "")
              (reset! state ::FRESH))
            :else
            (let [lexeme (->> @buffer
                              (drop-last)
                              (str/join ""))
                  number-part (.substring lexeme 2)
                  number (Integer/parseInt ^String number-part 16)]
              (reset! state ::FRESH)
              (swap! buffer #(str (last %)))
              (t/map->Token {:type ::t/HEXADECIMAL
                             :lexeme lexeme
                             :literal number
                             :line @line
                             :column @column}))))))))






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
              (reset! state ::FRESH)
              (or token
                (recur (.read reader))))))))))

(defn reader->stream-scanner
  ([^BufferedReader reader]
   (reader->stream-scanner reader {:line (atom 1)
                                   :column (atom 1)
                                   :buffer (atom "")
                                   :state (atom ::FRESH)}))
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
