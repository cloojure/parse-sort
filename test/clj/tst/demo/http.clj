(ns tst.demo.http
  (:use demo.http tupelo.core tupelo.test)
  (:require
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
  (let [data-line    "Last-3 |   First-3 |   lf33@aol.com |   C3|   3/03/2003"
        encoded-line (b64url/encode-str data-line)]
    (is= encoded-line "TGFzdC0zIHwgICBGaXJzdC0zIHwgICBsZjMzQGFvbC5jb20gfCAgIEMzfCAgIDMvMDMvMjAwMw==")
    (nl)

    (comment
      (tp/with-service tst-service-map ; mock testing w/o actually starting jetty
        ; native Pedestal way of testing response without spinning up a full Server
        (let [resp (ptst/response-for (service-fn) :post (str "/records?line=" encoded-line))]
          ; (nl) (spyx-pretty resp)
          (is= (grab :status resp) 200)
          (is-nonblank= (grab :body resp) "accepted"))))

    (tp/with-service tst-service-map ; mock testing w/o actually starting jetty
      ; native Pedestal way of testing response without spinning up a full Server
      (let [resp (ptst/response-for (service-fn)
                   :post (str "/records")
                   :headers {"Content-Type" "application/json"}
                   :body (edn->json {:line data-line})
                   )
            ]
        ; (nl) (spyx-pretty resp)
        (is= (grab :status resp) 200)
        (is-nonblank= (grab :body resp) " accepted ")))

    ))

(dotest
  (discarding-system-err
    (tp/with-service tst-service-map ; mock testing w/o actually starting jetty

      ; native Pedestal way of testing response without spinning up a full Server
      (let [resp (ptst/response-for (service-fn) :get "/greet?name=Michael")]
        (is= (grab :status resp) 200)
        (is-nonblank= (grab :body resp) "Hello, Michael!"))

      ; Tupelo-Pedestal shortcut
      (let [resp (tp/service-get "/greet?name=")]
        (is= (grab :status resp) 200)
        (is-nonblank= (grab :body resp) "Hello, World!"))

      (let [resp (tp/service-get "/greet?name=  ")]
        (is= (grab :status resp) 200)
        (is-nonblank= (grab :body resp) "Hello, World!"))

      (let [resp (tp/service-get "/greet")]
        (is= (grab :status resp) 200)
        (is-nonblank= (grab :body resp) "Hello, World!"))

      )))






