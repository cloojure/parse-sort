(ns demo.core
  (:use tupelo.core)
  (:require
    [camel-snake-kebab.core :as csk]
    [clojure.java.io :as io]
    [clojure.walk :as walk]
    [org.httpkit.client :as http]
    [schema.core :as s]
    [tupelo.string :as str]
    [tupelo.schema :as tsk]
    )
  (:import
    [java.time LocalDate]
    [java.time.format DateTimeFormatter ]
    ))

;---------------------------------------------------------------------------------------------------
; Assumptions:
;   - well-formatted data (legal dates & charsets, etc)
;   - no whitespace within any field (esp. name & color fields)
;   - file suffix indicates format:  *.csv *.psv *.wsv
;   - the problem definition did not mention a header row in the *SV files, so we assume there isn't one
;---------------------------------------------------------------------------------------------------

;---------------------------------------------------------------------------------------------------
(def global-state (atom {}))
(defn entities-reset!
  [] (swap! global-state glue {:entities []}))
(entities-reset!) ; initial setup
(s/defn entities-add!
  [entities-new :- [tsk/KeyMap]]
  (swap! global-state update-in [:entities] glue entities-new))
(defn entities-get
  [] (grab :entities @global-state))

;---------------------------------------------------------------------------------------------------
; Date parsing/formatting

; will output dates like `1/2/1999` and `11/12/1999`
(def date-time-formatter-compact (DateTimeFormatter/ofPattern "M/d/yyyy"))

(s/defn mdy-str->LocalDate :- LocalDate
  "Parse a sloppy date string like `M/D/YYYY` or `M-D-YYYY` into a LocalDate"
  [date-str :- s/Str]
  ; #todo add more error checking for invalid strings
  (let [parts  (str/split date-str #"/|-")
        month  (str/pad-left (nth parts 0) 2 \0)
        day    (str/pad-left (nth parts 1) 2 \0)
        year   (nth parts 2)
        ldstr  (format "%s-%s-%s" year month day)
        result (LocalDate/parse ldstr)]
    result))

(s/defn format-LocalDate-compact :- s/Str
  "Formats a LocalDate into a compact format like  `1/2/1999` and `11/12/1999`"
  [localdate :- LocalDate]
  (.format date-time-formatter-compact localdate))

; helper functions for testing
(s/defn LocalDate->tagstr :- s/Str
  [arg] (str "<LocalDate " arg ">"))

(defn walk-LocalDate->str
  [data]
  (walk/postwalk (fn [item]
                   (cond-it-> item
                     (instance? LocalDate it) (LocalDate->tagstr it)))
    data))

(defn walk-format-LocalDate-compact
  [data]
  (walk/postwalk (fn [item]
                   (cond-it-> item
                     (instance? LocalDate it) (format-LocalDate-compact it)))
    data))

;---------------------------------------------------------------------------------------------------
(def field-names-orig
  ["LastName"
   "FirstName"
   "Email"
   "FavoriteColor"
   "DateOfBirth"
   "LastName"])

(def field-names
  (mapv csk/->kebab-case-keyword field-names-orig))

(s/defn attempt-tokenize-psv-line :- (s/->Maybe [s/Str])
  "Will attempt to tokenize a line as PSV data. Upon success will return a
  5-vec of strings, else nil."
  [line :- s/Str]
  (let [tokens (it-> line
                 (str/split it #"\|")
                 (mapv str/whitespace-collapse it))]
    (when (= 5 (count tokens))
      tokens)))

(s/defn attempt-tokenize-csv-line :- (s/->Maybe [s/Str])
  "Will attempt to tokenize a line as CSV data. Upon success will return a
  5-vec of strings, else nil."
  [line :- s/Str]
  (let [tokens (it-> line
                 (str/split it #",")
                 (mapv str/whitespace-collapse it))]
    (when (= 5 (count tokens))
      tokens)))

(s/defn attempt-tokenize-wsv-line :- (s/->Maybe [s/Str])
  "Will attempt to tokenize a line as WSV data. Upon success will return a
  5-vec of strings, else nil."
  [line :- s/Str]
  (let [tokens (it-> line
                 (str/split it #"\s+")
                 (mapv str/whitespace-collapse it))]
    (when (= 5 (count tokens))
      tokens)))

(s/defn parse-line-csv
  "Parse PSV input line into an entity map"
  [line :- s/Str]
  (let [[last first email color dob-str] (attempt-tokenize-csv-line line)
        dob    (mdy-str->LocalDate dob-str)
        result (vals->map last first email color dob)]
    result))

(s/defn parse-line-psv
  "Parse PSV input line into an entity map"
  [line :- s/Str]
  (let [[last first email color dob-str] (attempt-tokenize-psv-line line)
        dob    (mdy-str->LocalDate dob-str)
        result (vals->map last first email color dob)]
    result))

(s/defn parse-line-wsv
  "Parse WSV input line into an entity map"
  [line :- s/Str]
  (let [[last first email color dob-str] (attempt-tokenize-wsv-line line)
        dob    (mdy-str->LocalDate dob-str)
        result (vals->map last first email color dob)]
    result))

(s/defn parse-line-generic
  "Parse any CSV, PSV, or WSV input line into an entity map"
  [line :- s/Str]
  (let [tokens (cond-it-> (attempt-tokenize-csv-line line)
                 (nil? it) (attempt-tokenize-psv-line line)
                 (nil? it) (attempt-tokenize-wsv-line line)
                 (nil? it) (throw (ex-info "Could not parse line:" (vals->map line))))
        [last first email color dob-str] tokens ; destructure
        dob    (mdy-str->LocalDate dob-str)
        result (vals->map last first email color dob)]
    result))


(s/defn file-name->parse-line-fn :- tsk/Fn
  [fname :- s/Str]
  (let [suffix (last (str/split (str/trim fname) #"\."))]
    (cond
      (= suffix "csv") parse-line-csv
      (= suffix "psv") parse-line-psv
      (= suffix "wsv") parse-line-wsv
      :else (throw (ex-info "unrecognized file suffix" (vals->map fname suffix))))))

(s/defn parse-file :- [tsk/KeyMap]
  "Given a filename, loads & parses into global state"
  [fname :- s/Str]
  (let [parse-line-fn (file-name->parse-line-fn fname)]
    (it-> fname
      (io/resource it)
      (slurp it)
      (str/split-lines it)
      (mapv str/whitespace-collapse it)
      (mapv parse-line-fn it))))

(s/defn load-entities-from-file!
  [fnames :- [s/Str]]
  (doseq [fname fnames]
    (entities-add! (parse-file fname))))

(s/defn load-data-line
  "parses and loads a single data line into global state"
  [line :- s/Str]
  (it-> line
    (str/whitespace-collapse it)
    (parse-line-generic it)
    (vector it)
    (entities-add! it)))

(s/defn compare-email-asc :- s/Int
  [a :- tsk/KeyMap
   b :- tsk/KeyMap]
  (compare (grab :email a) (grab :email b))) ; negated for desc sort

(s/defn compare-email-desc :- s/Int
  [a :- tsk/KeyMap
   b :- tsk/KeyMap]
  (- (compare-email-asc a b)))

(s/defn compare-last-asc :- s/Int
  [a :- tsk/KeyMap
   b :- tsk/KeyMap]
  (compare (grab :last a) (grab :last b)))

(s/defn compare-last-desc :- s/Int
  [a :- tsk/KeyMap
   b :- tsk/KeyMap]
  (-  (compare-last-asc a b)))

(s/defn compare-dob-asc :- s/Int
  [a :- tsk/KeyMap
   b :- tsk/KeyMap]
  (compare (grab :dob a) (grab :dob b)))

(s/defn compare-email-desc-last-asc :- s/Int
  [a :- tsk/KeyMap
   b :- tsk/KeyMap]
  (cond-it-> (compare-email-desc a b)
    (zero? it) (compare-last-asc a b)))

(s/defn entities-get-email-desc-last-asc :- [tsk/KeyMap]
  "Returns entities sorted by email descending, then lastname ascending"
  [] (vec (sort-by identity compare-email-desc-last-asc
            (entities-get))))

(s/defn entities-get-dob-asc :- [tsk/KeyMap]
  "Returns entities sorted by date-of-birth ascending"
  [] (vec (sort-by identity compare-dob-asc
            (entities-get))))

(s/defn entities-get-last-desc :- [tsk/KeyMap]
  "Returns entities sorted by lastname descending"
  [] (vec (sort-by identity compare-last-desc
            (entities-get))))

(s/defn format-output-lines :- s/Str
  "Return sorted entities as a single multi-line string"
  [entities :- [tsk/KeyMap]]
  (str/join \newline
    (forv [entity entities]
      (with-map-vals entity [last first email color dob]
        (let [email (str/clip-text 20 email)
              last  (str/clip-text 20 last)
              first (str/clip-text 20 first)
              color (str/clip-text 20 color)
              dob   (format-LocalDate-compact dob)
              line  (format "%20s %15s %20s %20s %20s " email dob last first color)]
          line)))))

(defn -main [& args]
  (println "main - enter")
  )











