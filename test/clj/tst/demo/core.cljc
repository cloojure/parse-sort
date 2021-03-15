(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test)
  (:require
    [camel-snake-kebab.core :as csk]
    [org.httpkit.client :as http]
    [schema.core :as s]
    [tupelo.string :as str] ))

(dotest
  (is= (mapv csk/->kebab-case-keyword field-names-orig)
    [:last-name
     :first-name
     :email
     :favorite-color
     :date-of-birth
     :last-name])

  )
