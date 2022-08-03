(ns framework
  (:require [babashka.curl :as curl]
            [babashka.process :refer [process destroy-tree]]
            [babashka.wait :refer [wait-for-port]]
            [clojure.string :as s]
            [cheshire.core :as json]))

(defn to-json [error]
  (-> error :body (json/parse-string true)))

(defn pretty-error [{:keys [name endpoint params result]}]
  {:name     name
   :endpoint endpoint
   :params   params
   :error    (to-json result)})

(defn start-calculator [file port]
  (let [cmd  ["java" "-jar" file "--port" port]
        proc (process cmd {:out :inherit
                           :shutdown destroy-tree})]
    (println (str "Starting calculator: `" (s/join " " cmd) "'"))
    (wait-for-port "localhost" port {:timeout 30000 :pause 500})
    proc))

(defn create-endpoint [host port]
  (fn [{:keys [endpoint params test-fn]}]
    (let [url (str host ":" port endpoint)
          res (curl/get url {:query-params params :throw false})]
      (if (test-fn res) {} res))))

(comment
  (let [call-endpoint (create-endpoint "localhost" "3000")]
    (call-endpoint {:endpoint "/calculate"
                    :params {"expression" "3&7"}
                    :test-fn #(= 200 (:status %))}))
  (->
    (curl/get "localhost:3000/calculate" {:query-params {"expression" "(64|63)&191"} :throw false})
    :body
    (json/parse-string true)))
