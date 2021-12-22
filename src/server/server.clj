(ns server.server
  (:require [clojure.tools.logging :as log]
            [reitit.ring :as ring]
            [reitit.http :as http]
            [reitit.coercion.malli :as malli]
            [reitit.http.interceptors.muuntaja :as muuntaja]
            [reitit.interceptor.sieppari :as sieppari]
            [muuntaja.core :as m]
            [ring.adapter.jetty :as jetty]))

(defn respond
  ([data]
   (respond data 200))
  ([data status]
   {:status status
    :body   {:message data}}))

(defn health-check
  [_]
  (respond "Ok"))

(def routes
  [["/health" {:get {:handler health-check}}]])

(def server
  (http/ring-handler
    (http/router routes
                 {:data {:coercion malli/coercion
                         :muuntaja m/instance
                         :interceptors [(muuntaja/format-response-interceptor)]}})
    (ring/routes
      (ring/create-default-handler
        {:not-found (constantly {:status  404
                                 :headers {"Content-Type" "application/json"}
                                 :body    "{\"message\": \"Took a wrong turn?\"}"})}))
    {:executor sieppari/executor}))

;; http server state
(defonce http-server (atom nil))

(defn start []
  (log/info "Starting server")
  (reset! http-server (jetty/run-jetty #'server {:port 3000 :join? false :async? true})))

(defn stop []
  (when-not (nil? @http-server)
    (log/info "Stopping server")
    (.stop @http-server)))

(comment
  (start)
  (stop))
