(ns server.server
  (:require [clojure.tools.logging :as log]
            [reitit.ring :as ring]
            [reitit.http :as http]
            [reitit.http.coercion :as coercion]
            [reitit.coercion.malli :as malli]
            [reitit.http.interceptors.exception :as exception]
            [reitit.http.interceptors.parameters :as parameters]
            [reitit.http.interceptors.muuntaja :as muuntaja]
            [reitit.interceptor.sieppari :as sieppari]
            [muuntaja.core :as m]
            [ring.adapter.jetty :as jetty]
            [parser.parser :as p]
            [alu.alu :as alu]))

(defn respond
  ([data]
   (respond data 200))
  ([data status]
   {:status status
    :body   {:message data}}))

(defn health-check
  [_]
  (respond "Ok"))

(defn calculate
  [{{{:keys [expression]} :query} :parameters}]
  (try
    (let [dictionary    {"value" alu/byte>, "~" alu/not>, "&" alu/and>, "|" alu/or>}
          parser        (p/parser> p/grammar dictionary)
          [expr values] (parser expression)
          output        (alu/read> (eval expr) values)]
      (respond (select-keys output [:result])))
    (catch Exception e
      (respond (.getMessage e) 400))))

(def routes
  [["/health"    {:get {:handler health-check}}]
   ["/calculate" {:get {:parameters {:query [:map [:expression string?]]}
                        :handler calculate}}]])

(def server
  (http/ring-handler
    (http/router routes
                 {:data {:coercion malli/coercion
                         :muuntaja m/instance
                         :interceptors [(exception/exception-interceptor)
                                        (parameters/parameters-interceptor)
                                        (muuntaja/format-negotiate-interceptor)
                                        (muuntaja/format-response-interceptor)
                                        (muuntaja/format-request-interceptor)
                                        (coercion/coerce-exceptions-interceptor)
                                        (coercion/coerce-response-interceptor)
                                        (coercion/coerce-request-interceptor)]}})
    (ring/routes
      (ring/create-default-handler
        {:not-found (constantly {:status  404
                                 :headers {"Content-Type" "application/json"}
                                 :body    "{\"message\": \"Took a wrong turn?\"}"})}))
    {:executor sieppari/executor}))

;; http server state
(defonce http-server (atom nil))

(defn start [port]
  (log/info (str "Starting server on port " port))
  (reset! http-server (jetty/run-jetty #'server {:port port :join? false :async? true})))

(defn stop []
  (when-not (nil? @http-server)
    (log/info "Stopping server")
    (.stop @http-server)))

(comment
  (start 3000)
  (stop))
