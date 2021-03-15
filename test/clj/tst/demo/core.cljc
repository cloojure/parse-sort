(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test)
  (:require
    [camel-snake-kebab.core :as csk]
    [org.httpkit.client :as http]
    [schema.core :as s]
    [tupelo.string :as str] )
  (:import
    [java.time LocalDate]
    ))

(dotest
  ; verify conversion of original column labels into keywords
  (is= field-names
    [:last-name
     :first-name
     :email
     :favorite-color
     :date-of-birth
     :last-name])

  ; verify can parse sloppy date format
  (is= (pad-leading-zero "2") "02")
  (is= (pad-leading-zero "23") "23")

  ; can accept 1 or 2 digit month/day, and either slash or hyphen separators
  (is= (LocalDate/parse "1999-01-01")
    (str->LocalDate "1/1/1999")
    (str->LocalDate "1-1-1999")
    (str->LocalDate "01/1/1999")
    (str->LocalDate "1-01-1999"))

  )
