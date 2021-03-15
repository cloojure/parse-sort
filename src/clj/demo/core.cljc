(ns demo.core
  (:use tupelo.core)
  (:require
    [camel-snake-kebab.core :as csk]
    [org.httpkit.client :as http]
    [schema.core :as s]
    [tupelo.string :as str]
    [clojure.java.io :as io])
  (:import
    [java.time LocalDate]
    [java.time.format DateTimeFormatter ]
    ))

;---------------------------------------------------------------------------------------------------
; Assumptions:
;   - well-formatted data (legal dates & charsets, etc)
;   - no whitespace within any field (esp. name & color fields)
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
  "Parse WSV string into segments"
  [line :- s/Str]
  (let [[last first email color dob-str] (str/split (str/whitespace-collapse line) #"\s+")
        dob (mdy-str->LocalDate dob-str)
        result (vals->map last first email color dob)]
    result))

(s/defn wsv-parse
  [fname]
  (it-> fname
    (io/resource it)
    (slurp it)
    (str/split-lines it)
    (mapv str/whitespace-collapse it)
    (mapv wsv-parse-line it)
    )
  )

(defn -main [& args]
  (println "main - enter")
  )


