(ns demo.http
  (:use tupelo.core)
  (:require
    [clojure.data.json :as json]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [demo.core :as core]
    [hiccup.core :as hiccup]
    [io.pedestal.http :as http]
    [io.pedestal.http.content-negotiation :as conneg]
    [io.pedestal.http.route :as route]
    [schema.core :as s]
    [tupelo.base64url :as b64url]
    [tupelo.parse :as tpar]
    [tupelo.pedestal :as tp]
    [tupelo.pedestal.headers :as hdrs]
    ))

; Prismatic Schema type definitions
(s/set-fn-validation! true) ; enforce fn schemas

(defn ok [body]
  {:status 200 :body body})

(defn not-found []
  {:status 404 :body "Not found\n"})

(tp/definterceptor echo-intc
  {:leave (fn [context]
            (assoc context :response (ok (pretty-str (tpar/edn-parsible context)))))})

(defn canonicalize-name
  [str-val]
  (-> str-val
    (str) ; convert nil => ""
    (str/lower-case)
    (str/trim)))

(def supported-types [ hdrs/application-json ] )

(def content-negotiation-intc
  (validate tp/interceptor?
    (conneg/negotiate-content supported-types)))

(defn negotiated-type
  "Returns the Content-Type determined by the content negotiation interceptor"
  [ctx]
  (get-in ctx [:request :accept :field] "text/plain"))

(defn transform-content
  [body-data content-type]
  (condp = content-type
    hdrs/text-html body-data
    hdrs/text-plain body-data
    hdrs/application-edn (pr-str body-data)
    hdrs/application-json (json/write-str body-data)))

(defn greeting-for
  "Returns a greeting for `user-name` if present, else a generic greeting. "
  [user-name]
  (let [user-name (str/trim (str user-name))] ; nil => ""
    (cond
      (str/blank? user-name) "Hello, World! \n"
      :else (format "Hello, %s!" user-name))))

(tp/definterceptor respond-hello-intc
  {:leave (fn [ctx]
            (let [user-name (get-in ctx [:request :query-params :name])
                  body-str  (greeting-for user-name)
                  response  (if body-str
                              (ok body-str)
                              (not-found))]
              (assoc ctx :response response)))})

(tp/definterceptor respond-hello-intc
  {:leave (fn [ctx]
            (let [user-name (get-in ctx [:request :query-params :name])
                  body-str  (greeting-for user-name)
                  response  (if body-str
                              (ok body-str)
                              (not-found))]
              (assoc ctx :response response)))})

(tp/definterceptor post-intc
  {:leave (fn [ctx]
            (let [request    (unlazy (fetch-in ctx [:request]))
                  body-edn   (json->edn (grab :body request))
                  input-line (grab :line body-edn)]
              (core/load-data-line input-line)
              (assoc ctx :response (ok "loaded"))))})

; NOTE!  a handler fn consumes a REQUEST (not a CONTEXT) !!!
; NOTE!  a handler fn produces a RESPONSE (not a :response in the CONTEXT) !!!

(def routes
  (route/expand-routes
    #{
      (tp/table-route {:verb :post :path "/records" :route-name :post-rec :interceptors [post-intc]})
      (tp/table-route {:verb :get :path "/echo" :route-name :echo :interceptors [echo-intc]})
      (tp/table-route {:verb :get :path "/greet" :route-name :greet :interceptors respond-hello-intc})
      }))

(def base-service-map
  {::http/routes routes
   ::http/join?  true}) ; true => block the starting thread (want this for supervisord in prod); false => don't block

(defn -main [& args]
  (println "main - enter")

  ; start the server in a separate thread, and wait until it is done (blocks the main thread)
  (let [service-map (cond-it-> base-service-map
                      (not-empty? args) (let [port-str (first args)
                                              port (tpar/parse-int port-str)]
                                          (glue it {::http/port port})))]
    (tp/with-server service-map
      ; nothing required here - main thread will block after starting server
      ))
  (println "main - exit")
  )








