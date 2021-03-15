(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test)
  (:require
    [camel-snake-kebab.core :as csk]
    [org.httpkit.client :as http]
    [schema.core :as s]
    [tupelo.string :as str]
    [clojure.java.io :as io])
  (:import
    [java.time LocalDate ]
    [java.time.format DateTimeFormatter ]
    ))

(def test-data-fnames
  ["data-1.psv"
   "data-2.csv"
   "data-3.wsv"] )

(dotest
  ; verify conversion of original column labels into keywords
  (is= field-names
    [:last-name
     :first-name
     :email
     :favorite-color
     :date-of-birth
     :last-name])

  ; verify pad-left
  (is= "02" (str/pad-left "2" 2 \0))
  (is= "23" (str/pad-left "23" 2 \0))

  ; verify can parse sloppy date format
  ; can accept 1 or 2 digit month/day, and either slash or hyphen separators
  (is= (LocalDate/parse "1999-01-02")
    (mdy-str->LocalDate "1/2/1999")
    (mdy-str->LocalDate "1-2-1999")
    (mdy-str->LocalDate "01/2/1999")
    (mdy-str->LocalDate "1-02-1999"))

  ; verify DateTimeFormatter
  (is= (.format dtf (mdy-str->LocalDate "1/2/1999")) "1/2/1999")
  (is= (.format dtf (mdy-str->LocalDate "11/12/1999")) "11/12/1999")

  (is= (wsv-parse-line "Last-3 First-3 lf33@aol.com C3 3/03/2003")
    {:last  "Last-3",
     :first "First-3",
     :email "lf33@aol.com",
     :color "C3",
     :dob   (mdy-str->LocalDate "3-3-2003")})

  ; verify localdates sort OK via standard comparator from clojure
  (let [d1 (LocalDate/parse "1999-01-01")
        d2 (LocalDate/parse "1999-01-02")]
    (is= (mapv str (sort [d2 d1]))
      ["1999-01-01"
       "1999-01-02"]))


  ; parse the WSV file
  (spyx-pretty
    (wsv-parse "data-3.wsv"))

  )


















