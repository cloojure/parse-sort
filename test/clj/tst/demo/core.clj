(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test)
  (:require
    [camel-snake-kebab.core :as csk]
    [clojure.java.io :as io]
    [clojure.walk :as walk]
    [org.httpkit.client :as http]
    [schema.core :as s]
    [tupelo.java-time :as tjt]
    [tupelo.schema :as tsk]
    [tupelo.string :as str])
(:import
    [java.time LocalDate ]
    [java.time.format DateTimeFormatter ]
    ))

(def test-data-fnames
  ["data-1.psv"
   "data-2.csv"
   "data-3.wsv"])

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
  (is= (.format date-time-formatter-compact (mdy-str->LocalDate "1/2/1999")) "1/2/1999")
  (is= (.format date-time-formatter-compact (mdy-str->LocalDate "11/12/1999")) "11/12/1999")

  ; verify LocalDate values sort OK via standard comparator from clojure
  (let [d1 (LocalDate/parse "1999-01-01")
        d2 (LocalDate/parse "1999-01-02")]
    (is= (mapv str (sort [d2 d1]))
      ["1999-01-01"
       "1999-01-02"])))

(dotest
  (entities-reset!)
  (entities-add! [{:a 1}])
  (entities-add! [{:b 2} {:c 3}])
  (is= (entities-get) [{:a 1}
                       {:b 2}
                       {:c 3}])
  (entities-reset!)
  (is= [] (entities-get)))

(dotest
  (is= (parse-line-wsv "Last-3 First-3 lf33@aol.com C3 3/03/2003")
    {:last  "Last-3" :first "First-3" :email "lf33@aol.com" :color "C3" :dob   (mdy-str->LocalDate "3-3-2003")})

  (is= (parse-line-psv "Last-3 |   First-3 |   lf33@aol.com |   C3|   3/03/2003")
    {:last  "Last-3" :first "First-3" :email "lf33@aol.com" :color "C3" :dob   (mdy-str->LocalDate "3-3-2003")})

  (is= (parse-line-csv "Last-3,  First-3 ,   lf33@aol.com ,   C3,   3/03/2003")
    {:last  "Last-3" :first "First-3" :email "lf33@aol.com" :color "C3" :dob   (mdy-str->LocalDate "3-3-2003")})

  (throws? (file-name->parse-line-fn "aaa.xxx"))
  (is= (file-name->parse-line-fn "aaa.csv") parse-line-csv)
  (is= (file-name->parse-line-fn "aaa.psv") parse-line-psv)
  (is= (file-name->parse-line-fn "aaa.wsv") parse-line-wsv)
  (is= (file-name->parse-line-fn "aaa.bbb.ccc.wsv") parse-line-wsv)

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
    [{:last "Last-1" :first "First-1" :email "lastFirstX@aol.com" :color "C1" :dob "<LocalDate 2001-01-01>"}
     {:last "Last-2" :first "First-2" :email "lastFirstX@aol.com" :color "C2" :dob "<LocalDate 2002-02-02>"}
     {:last "Last-3" :first "First-3" :email "lastFirstY@aol.com" :color "C3" :dob "<LocalDate 2003-03-03>"}
     {:last "Last-4" :first "First-4" :email "lastFirstY@aol.com" :color "C4" :dob "<LocalDate 2004-04-04>"}
     {:last "Last-5" :first "First-5" :email "lastFirstY@aol.com" :color "C5" :dob "<LocalDate 2005-05-05>"}])

  ;*****************************************************************************
  ;*** Step 1 Input: Load the 3 file types
  ;*****************************************************************************
  (entities-reset!)
  (load-entities-from-file! ["data-1.psv" "data-2.csv" "data-3.wsv"])
  (is= (walk-LocalDate->str (entities-get))
    [{:last "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob "<LocalDate 1911-01-01>"}
     {:last "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob "<LocalDate 1912-02-02>"}
     {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "<LocalDate 1913-03-03>"}
     {:last "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob "<LocalDate 1914-04-04>"}
     {:last "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob "<LocalDate 1915-05-05>"}

     {:last "Allen", :first "Alpha", :email "aallen@gmail.com", :color "Color1", :dob "<LocalDate 1912-12-12>"}
     {:last "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob "<LocalDate 1911-11-11>"}
     {:last "Case", :first "Carl", :email "ccase@gmail.com", :color "Color3", :dob "<LocalDate 1910-10-10>"}
     {:last "Davis", :first "Dan", :email "addavis@gmail.com", :color "Color4", :dob "<LocalDate 1909-09-09>"}
     {:last "Edam", :first "Elon", :email "eedam@gmail.com", :color "Color5", :dob "<LocalDate 1908-08-08>"}

     {:last "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob "<LocalDate 2001-01-01>"}
     {:last "Last-2", :first "First-2", :email "lastFirstX@aol.com", :color "C2", :dob "<LocalDate 2002-02-02>"}
     {:last "Last-3", :first "First-3", :email "lastFirstY@aol.com", :color "C3", :dob "<LocalDate 2003-03-03>"}
     {:last "Last-4", :first "First-4", :email "lastFirstY@aol.com", :color "C4", :dob "<LocalDate 2004-04-04>"}
     {:last "Last-5", :first "First-5", :email "lastFirstY@aol.com", :color "C5", :dob "<LocalDate 2005-05-05>"}])

  )

(dotest
  (let [greeks [{:last "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob "<LocalDate 1911-01-01>"}
                {:last "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob "<LocalDate 1912-02-02>"}
                {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "<LocalDate 1913-03-03>"}
                {:last "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob "<LocalDate 1914-04-04>"}
                {:last "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob "<LocalDate 1915-05-05>"}]]

    (is= (vec (sort-by identity compare-email-asc greeks))
      [{:last "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob "<LocalDate 1911-01-01>"}
       {:last "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob "<LocalDate 1912-02-02>"}
       {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "<LocalDate 1913-03-03>"}
       {:last "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob "<LocalDate 1914-04-04>"}
       {:last "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob "<LocalDate 1915-05-05>"}])
    (is= (vec (sort-by identity compare-email-desc greeks))
      [{:last "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob "<LocalDate 1915-05-05>"}
       {:last "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob "<LocalDate 1914-04-04>"}
       {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "<LocalDate 1913-03-03>"}
       {:last "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob "<LocalDate 1912-02-02>"}
       {:last "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob "<LocalDate 1911-01-01>"}])

    (is= (vec (sort-by identity compare-last-asc greeks))
      [{:last "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob "<LocalDate 1911-01-01>"}
       {:last "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob "<LocalDate 1912-02-02>"}
       {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "<LocalDate 1913-03-03>"}
       {:last "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob "<LocalDate 1914-04-04>"}
       {:last "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob "<LocalDate 1915-05-05>"}])
    (is= (vec (sort-by identity compare-last-desc greeks))
      [{:last "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob "<LocalDate 1915-05-05>"}
       {:last "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob "<LocalDate 1914-04-04>"}
       {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "<LocalDate 1913-03-03>"}
       {:last "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob "<LocalDate 1912-02-02>"}
       {:last "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob "<LocalDate 1911-01-01>"}])

    (is= (vec (sort-by identity compare-dob-asc greeks))
      [{:last "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob "<LocalDate 1911-01-01>"}
       {:last "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob "<LocalDate 1912-02-02>"}
       {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "<LocalDate 1913-03-03>"}
       {:last "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob "<LocalDate 1914-04-04>"}
       {:last "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob "<LocalDate 1915-05-05>"}]))

  (let [sharers [{:last "Last-1" :first "First-1" :email "lastFirstX@aol.com" :color "C1" :dob "<LocalDate 2001-01-01>"}
                 {:last "Last-2" :first "First-2" :email "lastFirstX@aol.com" :color "C2" :dob "<LocalDate 2002-02-02>"}
                 {:last "Last-3" :first "First-3" :email "lastFirstY@aol.com" :color "C3" :dob "<LocalDate 2003-03-03>"}
                 {:last "Last-4" :first "First-4" :email "lastFirstY@aol.com" :color "C4" :dob "<LocalDate 2004-04-04>"}
                 {:last "Last-5" :first "First-5" :email "lastFirstY@aol.com" :color "C5" :dob "<LocalDate 2005-05-05>"}]]
    (is= (vec (sort-by identity compare-email-desc-last-asc sharers))
      [{:last "Last-3", :first "First-3", :email "lastFirstY@aol.com", :color "C3", :dob "<LocalDate 2003-03-03>"}
       {:last "Last-4", :first "First-4", :email "lastFirstY@aol.com", :color "C4", :dob "<LocalDate 2004-04-04>"}
       {:last "Last-5", :first "First-5", :email "lastFirstY@aol.com", :color "C5", :dob "<LocalDate 2005-05-05>"}
       {:last "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob "<LocalDate 2001-01-01>"}
       {:last "Last-2", :first "First-2", :email "lastFirstX@aol.com", :color "C2", :dob "<LocalDate 2002-02-02>"}])))

; Demonstrate sorting 3 ways
(dotest
  ; Load sample data from 3 files
  (entities-reset!)
  (load-entities-from-file! ["data-1.psv" "data-2.csv" "data-3.wsv"])

  ;*****************************************************************************
  ;*** Step 1 Output: Load the 3 file types
  ;*****************************************************************************

  ;---------------------------------------------------------------------------------------------------
  ; Sort by email descending, lastname ascending
  (is= (walk-LocalDate->str (entities-get-email-desc-last-asc))
    [{:last "Last-3", :first "First-3", :email "lastFirstY@aol.com", :color "C3", :dob "<LocalDate 2003-03-03>"}
     {:last "Last-4", :first "First-4", :email "lastFirstY@aol.com", :color "C4", :dob "<LocalDate 2004-04-04>"}
     {:last "Last-5", :first "First-5", :email "lastFirstY@aol.com", :color "C5", :dob "<LocalDate 2005-05-05>"}
     {:last "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob "<LocalDate 2001-01-01>"}
     {:last "Last-2", :first "First-2", :email "lastFirstX@aol.com", :color "C2", :dob "<LocalDate 2002-02-02>"}
     {:last "Edam", :first "Elon", :email "eedam@gmail.com", :color "Color5", :dob "<LocalDate 1908-08-08>"}
     {:last "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob "<LocalDate 1915-05-05>"}
     {:last "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob "<LocalDate 1914-04-04>"}
     {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "<LocalDate 1913-03-03>"}
     {:last "Case", :first "Carl", :email "ccase@gmail.com", :color "Color3", :dob "<LocalDate 1910-10-10>"}
     {:last "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob "<LocalDate 1912-02-02>"}
     {:last "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob "<LocalDate 1911-11-11>"}
     {:last "Davis", :first "Dan", :email "addavis@gmail.com", :color "Color4", :dob "<LocalDate 1909-09-09>"}
     {:last "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob "<LocalDate 1911-01-01>"}
     {:last "Allen", :first "Alpha", :email "aallen@gmail.com", :color "Color1", :dob "<LocalDate 1912-12-12>"}])

  ; text formatted version in file
  (is-nonblank-lines= (format-output-lines (entities-get-email-desc-last-asc))
    (slurp (io/resource "expected-email-desc-last-asc.txt")))

  ;---------------------------------------------------------------------------------------------------
  ; Sort by date-of-birth ascending
  (is= (walk-LocalDate->str (entities-get-dob-asc))
    [{:last  "Edam", :first "Elon", :email "eedam@gmail.com", :color "Color5", :dob   "<LocalDate 1908-08-08>"}
     {:last  "Davis", :first "Dan", :email "addavis@gmail.com", :color "Color4", :dob   "<LocalDate 1909-09-09>"}
     {:last  "Case", :first "Carl", :email "ccase@gmail.com", :color "Color3", :dob   "<LocalDate 1910-10-10>"}
     {:last  "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob   "<LocalDate 1911-01-01>"}
     {:last  "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob   "<LocalDate 1911-11-11>"}
     {:last  "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob   "<LocalDate 1912-02-02>"}
     {:last  "Allen", :first "Alpha", :email "aallen@gmail.com", :color "Color1", :dob   "<LocalDate 1912-12-12>"}
     {:last  "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob   "<LocalDate 1913-03-03>"}
     {:last  "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob   "<LocalDate 1914-04-04>"}
     {:last  "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob   "<LocalDate 1915-05-05>"}
     {:last  "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob   "<LocalDate 2001-01-01>"}
     {:last  "Last-2", :first "First-2", :email "lastFirstX@aol.com", :color "C2", :dob   "<LocalDate 2002-02-02>"}
     {:last  "Last-3", :first "First-3", :email "lastFirstY@aol.com", :color "C3", :dob   "<LocalDate 2003-03-03>"}
     {:last  "Last-4", :first "First-4", :email "lastFirstY@aol.com", :color "C4", :dob   "<LocalDate 2004-04-04>"}
     {:last  "Last-5", :first "First-5", :email "lastFirstY@aol.com", :color "C5", :dob   "<LocalDate 2005-05-05>"}])

  ; text formatted version in file
  (is-nonblank-lines= (format-output-lines (entities-get-dob-asc))
    (slurp (io/resource "expected-dob-asc.txt")))

  ;---------------------------------------------------------------------------------------------------
  ; Sort by lastname descending
  (is= (walk-LocalDate->str (entities-get-last-desc))
    [{:last  "Last-5", :first "First-5", :email "lastFirstY@aol.com", :color "C5", :dob   "<LocalDate 2005-05-05>"}
     {:last  "Last-4", :first "First-4", :email "lastFirstY@aol.com", :color "C4", :dob   "<LocalDate 2004-04-04>"}
     {:last  "Last-3", :first "First-3", :email "lastFirstY@aol.com", :color "C3", :dob   "<LocalDate 2003-03-03>"}
     {:last  "Last-2", :first "First-2", :email "lastFirstX@aol.com", :color "C2", :dob   "<LocalDate 2002-02-02>"}
     {:last  "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob   "<LocalDate 2001-01-01>"}
     {:last  "Edam", :first "Elon", :email "eedam@gmail.com", :color "Color5", :dob   "<LocalDate 1908-08-08>"}
     {:last  "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob   "<LocalDate 1915-05-05>"}
     {:last  "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob   "<LocalDate 1914-04-04>"}
     {:last  "Davis", :first "Dan", :email "addavis@gmail.com", :color "Color4", :dob   "<LocalDate 1909-09-09>"}
     {:last  "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob   "<LocalDate 1913-03-03>"}
     {:last  "Case", :first "Carl", :email "ccase@gmail.com", :color "Color3", :dob   "<LocalDate 1910-10-10>"}
     {:last  "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob   "<LocalDate 1912-02-02>"}
     {:last  "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob   "<LocalDate 1911-11-11>"}
     {:last  "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob   "<LocalDate 1911-01-01>"}
     {:last  "Allen", :first "Alpha", :email "aallen@gmail.com", :color "Color1", :dob   "<LocalDate 1912-12-12>"}])

  ; text formatted version in file
  (is-nonblank-lines= (format-output-lines (entities-get-last-desc))
    (slurp (io/resource "expected-last-desc.txt"))))

;---------------------------------------------------------------------------------------------------
(dotest
  ; If we know the line type, we can parse directly
  (is= {:last "Last-3" :first "First-3" :email "lf33@aol.com" :color "C3" :dob "<LocalDate 2003-03-03>"}
    (walk-LocalDate->str (parse-line-csv "Last-3,  First-3 ,   lf33@aol.com ,   C3,   3/03/2003"))
    (walk-LocalDate->str (parse-line-psv "Last-3 |   First-3 |   lf33@aol.com |   C3|   3/03/2003"))
    (walk-LocalDate->str (parse-line-wsv "Last-3 First-3 lf33@aol.com C3 3/03/2003")))

  ; tokenize a CSV line or return nil
  (is= nil (attempt-tokenize-csv-line "Last-3 First-3 lf33@aol.com C3 3/03/2003"))
  (is= nil (attempt-tokenize-csv-line "Last-3 |   First-3 |   lf33@aol.com |   C3|   3/03/2003"))
  (is= (attempt-tokenize-csv-line "Last-3,  First-3 ,   lf33@aol.com ,   C3,   3/03/2003")
    ["Last-3" "First-3" "lf33@aol.com" "C3" "3/03/2003"])

  ; tokenize a PSV line or return nil
  (is= nil (attempt-tokenize-psv-line "Last-3 First-3 lf33@aol.com C3 3/03/2003"))
  (is= nil (attempt-tokenize-psv-line "Last-3,  First-3 ,   lf33@aol.com ,   C3,   3/03/2003"))
  (is= (attempt-tokenize-psv-line "Last-3 |   First-3 |   lf33@aol.com |   C3|   3/03/2003")
    ["Last-3" "First-3" "lf33@aol.com" "C3" "3/03/2003"])

  ; tokenize a WSV line or return nil
  (is= nil (attempt-tokenize-wsv-line "Last-3 |   First-3 |   lf33@aol.com |   C3|   3/03/2003"))
  (is= nil (attempt-tokenize-wsv-line "Last-3,  First-3 ,   lf33@aol.com ,   C3,   3/03/2003"))
  (is= (attempt-tokenize-wsv-line "Last-3 First-3 lf33@aol.com C3 3/03/2003")
    ["Last-3" "First-3" "lf33@aol.com" "C3" "3/03/2003"])

  ; auto-detects line type using a cascade of attempt-tokenize-*-line functions
  (is= {:last "Last-3" :first "First-3" :email "lf33@aol.com" :color "C3" :dob "<LocalDate 2003-03-03>"}
    (walk-LocalDate->str (parse-line-generic "Last-3 |   First-3 |   lf33@aol.com |   C3|   3/03/2003"))
    (walk-LocalDate->str (parse-line-generic "Last-3,  First-3 ,   lf33@aol.com ,   C3,   3/03/2003"))
    (walk-LocalDate->str (parse-line-generic "Last-3 First-3 lf33@aol.com C3 3/03/2003")))
  )

(dotest
  ; Load sample data from 3 individual lines
  (entities-reset!)
  (let [lines ["Last-1 First-1 lastFirstX@aol.com C1 1/01/2001"
               "Baker, Bravo, bbaker@gmail.com, Color2, 11-11-1911 "
               "Charlie | Chris | ccharlie@demo.com | Blue | 3/3/1913   "]]
    (doseq [line lines]
      (load-data-line line))

    (is= (walk-LocalDate->str (entities-get))
      [{:last "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob "<LocalDate 2001-01-01>"}
       {:last "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob "<LocalDate 1911-11-11>"}
       {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "<LocalDate 1913-03-03>"}])

    ))















