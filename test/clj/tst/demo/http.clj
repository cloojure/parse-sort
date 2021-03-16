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
    [tupelo.pedestal :as tp :refer [service-fn]]
    [tupelo.pedestal.headers :as hdrs]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    ))

;---------------------------------------------------------------------------------------------------
; v1: Hello, World (http://pedestal.io/guides/hello-world)

(def tst-service-map
  (glue base-service-map {::http/join? false})) ; don't block the starting thread for tests

(s/defn invoke-interceptors ; #todo => tupelo.pedestal
  "Given a context and a vector of interceptor-maps, invokes the interceptor chain
  and returns the resulting output context."
  [ctx :- tsk/KeyMap
   interceptors :- [tsk/KeyMap]] ; #todo => tupelo.pedestal & specialize to interceptor maps
  (let [pedestal-interceptors (mapv interceptor/map->Interceptor interceptors)]
    (chain/execute ctx pedestal-interceptors)))

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

;---------------------------------------------------------------------------------------------------
; v2: Hello World, With Parameters (http://pedestal.io/guides/hello-world-query-parameters)

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






