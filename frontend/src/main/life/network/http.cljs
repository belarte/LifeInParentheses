(ns life.network.http
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

(defn call
  ([endpoint callback]
   (call endpoint nil callback))
  ([endpoint params callback]
   (go (let [response (<! (http/get (str "http://localhost/api" endpoint)
                                    {:with-credentials? false
                                     :query-params params}))]
         (callback response)))))

(comment
  (call "/calculate" {"expression" "1|2"} #(prn "Status: " %))
  (call "/health" #(prn "Status: " %)))
