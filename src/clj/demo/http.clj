(ns demo.http
  (:use tupelo.core)
  (:require
    [clojure.string :as str]
    [demo.core :as core]
    [io.pedestal.http :as http]
    [io.pedestal.http.route :as route]
    [schema.core :as s]
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

(def supported-types [ hdrs/application-json ] )

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

(tp/definterceptor email-intc
  {:leave (fn [ctx]
            (let [body-str (core/walk-format-LocalDate-compact
                             (core/entities-get-email-desc-last-asc))
                  resp     (it-> (ok (edn->json body-str))
                             (assoc-in it [:headers hdrs/content-type] hdrs/application-json))]
              (assoc ctx :response resp)))})

(tp/definterceptor dob-intc
  {:leave (fn [ctx]
            (let [body-str (core/walk-format-LocalDate-compact
                             (core/entities-get-dob-asc))
                  resp     (it-> (ok (edn->json body-str))
                             (assoc-in it [:headers hdrs/content-type] hdrs/application-json))]
              (assoc ctx :response resp)))})

(tp/definterceptor last-intc
  {:leave (fn [ctx]
            (let [body-str (core/walk-format-LocalDate-compact
                             (core/entities-get-dob-asc))
                  resp     (it-> (ok (edn->json body-str))
                             (assoc-in it [:headers hdrs/content-type] hdrs/application-json))]
              (assoc ctx :response resp)))})

(def routes
  (route/expand-routes
    #{
      (tp/table-route {:verb :post :path "/records" :route-name :post-rec :interceptors [post-intc]})
      (tp/table-route {:verb :get :path "/records/email" :route-name :email :interceptors [email-intc]})
      (tp/table-route {:verb :get :path "/records/birthdate" :route-name :dob :interceptors [dob-intc]})
      (tp/table-route {:verb :get :path "/records/name" :route-name :last :interceptors [last-intc]})
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








