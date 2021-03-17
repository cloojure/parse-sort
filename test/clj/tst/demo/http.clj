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
  (tp/with-service tst-service-map ; mock testing w/o actually starting jetty
    (let [resp (ptst/response-for (service-fn) :get "/greet")]
      (is= (grab :status resp) 200)
      (is-nonblank= "Hello, World!" (grab :body resp))

      )))

(dotest
  (try
    (let [sys-err-str (with-system-err-str ; capture jetty logging from System/err
                        (tp/with-server tst-service-map ; test over http using jetty server
                          (let [resp @(http-client/get "http://localhost:8890/greet")]
                            (is (str/contains-str? (grab :body resp) "Hello, World!")))))]
      ; (spyx sys-err-str)
      (is (not-empty? (str/fgrep "GET /greet" sys-err-str)))))) ; eg '[qtp1379526008-32] INFO io.pedestal.http - {:msg "GET /greet", :line 80}'


(dotest-focus
  (discarding-system-err
    ; We want to discard Pedestal info msgs like:
    ;   [main] INFO io.pedestal.http - {:msg "POST /records", :line 80}

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
          [{:last "Baker", :first "Bravo", :email "bbaker@gmail.com", :color "Color2", :dob "11/11/1911"}
           {:last "Charlie", :first "Chris", :email "ccharlie@demo.com", :color "Blue", :dob "3/3/1913"}
           {:last "Last-1", :first "First-1", :email "lastFirstX@aol.com", :color "C1", :dob "1/1/2001"}])))))








