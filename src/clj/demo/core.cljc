(ns demo.core
  (:use tupelo.core)
  (:require
    [camel-snake-kebab.core :as csk]
    [org.httpkit.client :as http]
    [schema.core :as s]
    [tupelo.string :as str]))

(def field-names-orig
  ["LastName"
   "FirstName"
   "Email"
   "FavoriteColor"
   "DateOfBirth"
   "LastName"])

(def field-names
  (mapv csk/->kebab-case-keyword field-names-orig))

(defn -main []
  (println "main - enter")
  )


