(ns tst.demo.http
  (:use demo.http tupelo.core tupelo.test)
  (:require
    [demo.core :as core]
    [io.pedestal.http :as http]
    [io.pedestal.http.route :as route]
    [io.pedestal.interceptor :as interceptor]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.test :as ptst]
    [org.httpkit.client :as http-client]
    [schema.core :as s]
    [tupelo.base64url :as b64url]
    [tupelo.pedestal :as tp :refer [service-fn]]
    [tupelo.pedestal.headers :as hdrs]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    ))

;---------------------------------------------------------------------------------------------------
(def tst-service-map
  (glue base-service-map {::http/join? false})) ; don't block the starting thread for tests

;---------------------------------------------------------------------------------------------------
(dotest
  (discarding-system-err
    ; ^^^ use this to discard Pedestal info msgs like:
    ;         [main] INFO io.pedestal.http - {:msg "POST /records", :line 80}

    (tp/with-service tst-service-map ; mock testing w/o actually starting jetty

      ; PUT a single line & verify it
      (let [data-line "Last-3 |   First-3 |   lf33@aol.com |   C3|   3/03/2003"
            ; native Pedestal way of testing response without spinning up a full Server
            resp      (ptst/response-for (service-fn)
                        :post (str "/records")
                        :headers {"Content-Type" "application/json"}
                        :body (edn->json {:line data-line}))]
        ; (nl) (spyx-pretty resp)
        (is= (grab :status resp) 200)
        (is-nonblank= (grab :body resp) "loaded"))

      ; Load sample data from 3 individual lines & verify
      (core/entities-reset!)

      ;*****************************************************************************
      ;*** Step 2: REST API - POST
      ;*****************************************************************************

      ; POST 3 records
      (let [lines ["Last-1 First-1 lastFirstX@aol.com C1 1/01/2001"
                   "Baker, Bravo, bbaker@gmail.com, Color2, 11-11-1911 "
                   "Charlie | Chris | ccharlie@demo.com | Blue | 3/3/1913   "]]
        (doseq [line lines]
          (let [resp (ptst/response-for (service-fn)
                       :post (str "/records")
                       :headers {"Content-Type" "application/json"}
                       :body (edn->json {:line line}))]
            ; (nl) (spyx-pretty resp)
            (is= (grab :status resp) 200)
            (is-nonblank= (grab :body resp) "loaded")))
        (is= (core/walk-LocalDate->str (core/entities-get))
          [{:last "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob "<LocalDate 2001-01-01>"}
           {:last "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob "<LocalDate 1911-11-11>"}
           {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "<LocalDate 1913-03-03>"}]))

      ; verify sorted by email desc, last asc
      (let [resp (ptst/response-for (service-fn) :get (str "/records/email"))
            body (json->edn (grab :body resp))]
        (is= (grab :status resp) 200)
        (is= body
          [{:last "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob "1/1/2001"}
           {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "3/3/1913"}
           {:last "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob "11/11/1911"}]
          ))

      ; verify sorted by DOB asc
      (let [resp (ptst/response-for (service-fn) :get (str "/records/birthdate"))
            body (json->edn (grab :body resp))]
        (is= (grab :status resp) 200)
        (is= body
          [{:last "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob "11/11/1911"}
           {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "3/3/1913"}
           {:last "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob "1/1/2001"}]))

      ; verify sorted by lastname asc
      (let [resp (ptst/response-for (service-fn) :get (str "/records/name"))
            body (json->edn (grab :body resp))]
        (is= (grab :status resp) 200)
        (is= body
          [{:last "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob "1/1/2001"}
           {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "3/3/1913"}
           {:last "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob "11/11/1911"}])))))


(dotest
  ; Load sample data from 3 files
  (core/entities-reset!)
  (core/load-entities-from-file! ["data-1.psv" "data-2.csv" "data-3.wsv"])

  (discarding-system-err
    ; ^^^ use this to discard Pedestal info msgs like:
    ;         [main] INFO io.pedestal.http - {:msg "POST /records", :line 80}

    (tp/with-service tst-service-map ; mock testing w/o actually starting jetty

      ;*****************************************************************************
      ;*** Step 2: REST API - GET
      ;*****************************************************************************

      ; verify sorted by email desc, last asc
      (let [resp (ptst/response-for (service-fn) :get (str "/records/email"))
            body (vec (json->edn (grab :body resp)))]
        (is= (grab :status resp) 200)
        (is= body
          [{:last  "Last-3", :first "First-3", :email "lastFirstY@aol.com", :color "C3", :dob   "3/3/2003"}
           {:last  "Last-4", :first "First-4", :email "lastFirstY@aol.com", :color "C4", :dob   "4/4/2004"}
           {:last  "Last-5", :first "First-5", :email "lastFirstY@aol.com", :color "C5", :dob   "5/5/2005"}
           {:last  "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob   "1/1/2001"}
           {:last  "Last-2", :first "First-2", :email "lastFirstX@aol.com", :color "C2", :dob   "2/2/2002"}
           {:last  "Edam", :first "Elon", :email "eedam@gmail.com", :color "Color5", :dob   "8/8/1908"}
           {:last  "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob   "5/5/1915"}
           {:last  "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob   "4/4/1914"}
           {:last  "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob   "3/3/1913"}
           {:last  "Case", :first "Carl", :email "ccase@gmail.com", :color "Color3", :dob   "10/10/1910"}
           {:last  "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob   "2/2/1912"}
           {:last  "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob   "11/11/1911"}
           {:last  "Davis", :first "Dan", :email "addavis@gmail.com", :color "Color4", :dob   "9/9/1909"}
           {:last  "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob   "1/1/1911"}
           {:last  "Allen", :first "Alpha", :email "aallen@gmail.com", :color "Color1", :dob   "12/12/1912"}]))

      ; verify sorted by DOB asc
      (let [resp (ptst/response-for (service-fn) :get (str "/records/birthdate"))
            body (vec (json->edn (grab :body resp)))]
        (is= (grab :status resp) 200)
        (is= body
          [{:last  "Edam", :first "Elon", :email "eedam@gmail.com", :color "Color5", :dob   "8/8/1908"}
           {:last  "Davis", :first "Dan", :email "addavis@gmail.com", :color "Color4", :dob   "9/9/1909"}
           {:last  "Case", :first "Carl", :email "ccase@gmail.com", :color "Color3", :dob   "10/10/1910"}
           {:last  "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob   "1/1/1911"}
           {:last  "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob   "11/11/1911"}
           {:last  "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob   "2/2/1912"}
           {:last  "Allen", :first "Alpha", :email "aallen@gmail.com", :color "Color1", :dob   "12/12/1912"}
           {:last  "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob   "3/3/1913"}
           {:last  "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob   "4/4/1914"}
           {:last  "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob   "5/5/1915"}
           {:last  "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob   "1/1/2001"}
           {:last  "Last-2", :first "First-2", :email "lastFirstX@aol.com", :color "C2", :dob   "2/2/2002"}
           {:last  "Last-3", :first "First-3", :email "lastFirstY@aol.com", :color "C3", :dob   "3/3/2003"}
           {:last  "Last-4", :first "First-4", :email "lastFirstY@aol.com", :color "C4", :dob   "4/4/2004"}
           {:last  "Last-5", :first "First-5", :email "lastFirstY@aol.com", :color "C5", :dob   "5/5/2005"}]))

      ; verify sorted by lastname asc
      (let [resp (ptst/response-for (service-fn) :get (str "/records/name"))
            body (vec (json->edn (grab :body resp)))]
        (is= (grab :status resp) 200)
        (is= body
          [{:last  "Last-5", :first "First-5", :email "lastFirstY@aol.com", :color "C5", :dob   "5/5/2005"}
           {:last  "Last-4", :first "First-4", :email "lastFirstY@aol.com", :color "C4", :dob   "4/4/2004"}
           {:last  "Last-3", :first "First-3", :email "lastFirstY@aol.com", :color "C3", :dob   "3/3/2003"}
           {:last  "Last-2", :first "First-2", :email "lastFirstX@aol.com", :color "C2", :dob   "2/2/2002"}
           {:last  "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob   "1/1/2001"}
           {:last  "Edam", :first "Elon", :email "eedam@gmail.com", :color "Color5", :dob   "8/8/1908"}
           {:last  "Echo", :first "Eve", :email "eecho@demo.com", :color "Emerald", :dob   "5/5/1915"}
           {:last  "Delta", :first "Don", :email "ddelta@demo.com", :color "DarkBlue", :dob   "4/4/1914"}
           {:last  "Davis", :first "Dan", :email "addavis@gmail.com", :color "Color4", :dob   "9/9/1909"}
           {:last  "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob   "3/3/1913"}
           {:last  "Case", :first "Carl", :email "ccase@gmail.com", :color "Color3", :dob   "10/10/1910"}
           {:last  "Bravo", :first "Bob", :email "bbravo@demo.com", :color "Green", :dob   "2/2/1912"}
           {:last  "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob   "11/11/1911"}
           {:last  "Alpha", :first "Alan", :email "aalpha@demo.com", :color "Red", :dob   "1/1/1911"}
           {:last  "Allen", :first "Alpha", :email "aallen@gmail.com", :color "Color1", :dob   "12/12/1912"}]))))
  )



