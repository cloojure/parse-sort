(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test)
  (:require
    [camel-snake-kebab.core :as csk]
    [org.httpkit.client :as http]
    [schema.core :as s]
    [tupelo.string :as str]
    [clojure.java.io :as io]
    [clojure.walk :as walk])
  (:import
    [java.time LocalDate ]
    [java.time.format DateTimeFormatter ]
    ))

(def test-data-fnames
  ["data-1.psv"
   "data-2.csv"
   "data-3.wsv"])

;---------------------------------------------------------------------------------------------------
; helper functions for testing
(s/defn LocalDate->tagstr :- s/Str
  [arg] (str "<LocalDate " arg ">"))

(defn walk-LocalDate->str
  [data]
  (walk/postwalk (fn [item]
                   (cond-it-> item
                     (instance? LocalDate it) (LocalDate->tagstr it)))
    data))

(dotest
  (let [ld       (LocalDate/parse "1999-01-02")
        orig     [1 {:b ld}]
        expected [1 {:b "<LocalDate 1999-01-02>"}]]
    (isnt (instance? LocalDate "1999-01-02"))
    (is (instance? LocalDate ld))
    (is= "<LocalDate 1999-01-02>" (LocalDate->tagstr ld))
    (is= expected (walk-LocalDate->str orig))))

;---------------------------------------------------------------------------------------------------
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

  ; verify LocalDate values sort OK via standard comparator from clojure
  (let [d1 (LocalDate/parse "1999-01-01")
        d2 (LocalDate/parse "1999-01-02")]
    (is= (mapv str (sort [d2 d1]))
      ["1999-01-01"
       "1999-01-02"]))

  (is= (wsv-parse-line "Last-3 First-3 lf33@aol.com C3 3/03/2003")
    {:last  "Last-3"
     :first "First-3"
     :email "lf33@aol.com"
     :color "C3"
     :dob   (mdy-str->LocalDate "3-3-2003")})

  (is= (psv-parse-line "Last-3 |   First-3 |   lf33@aol.com |   C3|   3/03/2003")
    {:last  "Last-3"
     :first "First-3"
     :email "lf33@aol.com"
     :color "C3"
     :dob   (mdy-str->LocalDate "3-3-2003")})

  (is= (csv-parse-line "Last-3,  First-3 ,   lf33@aol.com ,   C3,   3/03/2003")
    {:last  "Last-3"
     :first "First-3"
     :email "lf33@aol.com"
     :color "C3"
     :dob   (mdy-str->LocalDate "3-3-2003")})

  (throws? (file-name->parse-line-fn "aaa.csv9") )
  (is= (file-name->parse-line-fn "aaa.csv") csv-parse-line)
  (is= (file-name->parse-line-fn "aaa.psv") psv-parse-line)
  (is= (file-name->parse-line-fn "aaa.wsv") wsv-parse-line)
  (is= (file-name->parse-line-fn "aaa.bbb.ccc.wsv") wsv-parse-line)

  ; helper fn
  (is= (file-ingest-prep "data-1.psv")
    ["Alpha | Alan | aalpha@demo.com | Red | 1/1/1911"
     "Bravo | Bob | bbravo@demo.com | Green | 2/2/1912"
     "Charlie | Chris | ccharlie@demo.com | Blue | 3/3/1913"
     "Delta | Don | ddelta@demo.com | DarkBlue | 4/4/1914"
     "Echo | Eve | eecho@demo.com | Emerald | 5/5/1915"])

  ; parse the 3 file types
  (is= (walk-LocalDate->str (parse-file "data-1.psv"))
    [{:last "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob "<LocalDate 1911-01-01>"}
     {:last "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob "<LocalDate 1912-02-02>"}
     {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "<LocalDate 1913-03-03>"}
     {:last "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob "<LocalDate 1914-04-04>"}
     {:last "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob "<LocalDate 1915-05-05>"}])
  (is= (walk-LocalDate->str (parse-file "data-2.csv"))
    [{:last "Allen", :first "Alpha", :email "aallen@gmail.com", :color "Color1", :dob "<LocalDate 1912-12-12>"}
     {:last "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob "<LocalDate 1911-11-11>"}
     {:last "Case", :first "Carl", :email "ccase@gmail.com", :color "Color3", :dob "<LocalDate 1910-10-10>"}
     {:last "Davis", :first "Dan", :email "addavis@gmail.com", :color "Color4", :dob "<LocalDate 1909-09-09>"}
     {:last "Edam", :first "Elon", :email "eedam@gmail.com", :color "Color5", :dob "<LocalDate 1908-08-08>"}])
  (is= (walk-LocalDate->str (parse-file "data-3.wsv"))
    [{:last "Last-1" :first "First-1" :email "lf11@aol.com" :color "C1" :dob "<LocalDate 2001-01-01>"}
     {:last "Last-2" :first "First-2" :email "lf22@aol.com" :color "C2" :dob "<LocalDate 2002-02-02>"}
     {:last "Last-3" :first "First-3" :email "lf33@aol.com" :color "C3" :dob "<LocalDate 2003-03-03>"}
     {:last "Last-4" :first "First-4" :email "lf44@aol.com" :color "C4" :dob "<LocalDate 2004-04-04>"}
     {:last "Last-5" :first "First-5" :email "lf55@aol.com" :color "C5" :dob "<LocalDate 2005-05-05>"}])

  )


















