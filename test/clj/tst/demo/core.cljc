(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test)
  (:require
    [org.httpkit.client :as http]
    [schema.core :as s]
    [tupelo.string :as str] ))

(s/defn author-list :- [s/Str]
  [name :- s/Str]
  (loop [cum-titles []
         page       1]
    (let [url             (str "https://jsonmock.hackerrank.com/api/articles?author=" name "&page=" page)
          resp-raw        @(http/get url)
          body            (str/walk-clojurize-keys (json->edn (grab :body resp-raw)))
          total-pages     (grab :total-pages body)
          book-data-list  (grab :data body)
          cum-titles-next (glue cum-titles
                            (forv [book-data book-data-list]
                              (grab :title book-data)))
          page-next       (inc page)]
      (if (< total-pages page-next)
        cum-titles-next
        (recur cum-titles-next page-next)))))

(dotest
  (is= (author-list "epaga")
    ["A Message to Our Customers"
     "“Was isolated from 1999 to 2006 with a 486. Built my own late 80s OS”"
     "Apple’s declining software quality"])

  (is= (author-list "saintamh")
    ["Google Is Eating Our Mail"
     "Why I’m Suing the US Government"]))
