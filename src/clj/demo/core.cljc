(ns demo.core
  (:use tupelo.core)
  (:require
    [camel-snake-kebab.core :as csk]
    [org.httpkit.client :as http]
    [schema.core :as s]
    [tupelo.string :as str])
  (:import
    [java.time LocalDate]
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

(s/defn pad-leading-zero :- s/Str
  [s :- s/Str]
  (if (= 2 (count s))
    s
    (str \0 s)))

(s/defn str->LocalDate ; :- LocalDate
  "Parse a sloppy date string like `M/D/YYYY` or `M-D-YYYY` into a LocalDate"
  [date-str :- s/Str]
  (let [parts  (str/split date-str #"/|-")
        month  (pad-leading-zero (nth parts 0))
        day    (pad-leading-zero (nth parts 1))
        year   (nth parts 2)
        ldstr  (format "%s-%s-%s" year month day)
        result (LocalDate/parse ldstr)]
    result))


(defn -main []
  (println "main - enter")
  )


