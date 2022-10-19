(ns life.network.http
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

(defn call [endpoint callback]
  (go (let [response (<! (http/get (str "http://localhost/api" endpoint)
                                   {:with-credentials? false
                                    :query-params nil}))]
        (callback response))))

(comment
  (call "/health" #(prn "Status: " %)))
