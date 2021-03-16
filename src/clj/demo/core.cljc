(ns demo.core
  (:use tupelo.core)
  (:require
    [camel-snake-kebab.core :as csk]
    [org.httpkit.client :as http]
    [schema.core :as s]
    [tupelo.string :as str]
    [clojure.java.io :as io]
    [tupelo.schema :as tsk])
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
(def field-names-orig
  ["LastName"
   "FirstName"
   "Email"
   "FavoriteColor"
   "DateOfBirth"
   "LastName"])

(def field-names
  (mapv csk/->kebab-case-keyword field-names-orig))

; will output dates like `1/2/1999` and `11/12/1999`
(def dtf (DateTimeFormatter/ofPattern "M/d/yyyy"))

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

(s/defn wsv-parse-line
  "Parse WSV input line into an entity map"
  [line :- s/Str]
  (let [[last first email color dob-str] (str/split line #"\s+")
        dob    (mdy-str->LocalDate dob-str)
        result (vals->map last first email color dob)]
    result))

(s/defn psv-parse-line
  "Parse PSV input line into an entity map"
  [line :- s/Str]
  (let [fields (it-> line
                 (str/split it #"\|")
                 (mapv str/whitespace-collapse it))
        [last first email color dob-str] fields
        dob    (mdy-str->LocalDate dob-str)
        result (vals->map last first email color dob)]
    result))

(s/defn csv-parse-line
  "Parse CSV input line into an entity map"
  [line :- s/Str]
  (let [fields (it-> line
                 (str/split it #",")
                 (mapv str/whitespace-collapse it))
        [last first email color dob-str] fields
        dob    (mdy-str->LocalDate dob-str)
        result (vals->map last first email color dob)]
    result))

(s/defn file-name->parse-line-fn :- tsk/Fn
  [fname :- s/Str]
  (let [suffix (last (str/split (str/trim fname) #"\."))]
    (cond
      (= suffix "csv") csv-parse-line
      (= suffix "psv") psv-parse-line
      (= suffix "wsv") wsv-parse-line
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

(defn -main [& args]
  (println "main - enter")
  )


