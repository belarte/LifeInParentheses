(ns tests
  (:require [babashka.process :refer [process destroy-tree]]
            [babashka.curl :as curl]
            [babashka.wait :refer [wait-for-port]]
            [clojure.string :as s]
            [cheshire.core :as json]))

(defn- humanized [error]
  (-> error :body (json/parse-string true) :humanized))

(defn- pretty-error [{:keys [name endpoint params result]}]
  {:name     name
   :endpoint endpoint
   :params   params
   :error    (humanized result)})

(def test-cases
  [{:name     "Health check"
    :endpoint "/health"
    :test-fn  #(= 200 (:status %))}

   {:name     "Calculate endpoint is responsive"
    :endpoint "/calculate"
    :params   {"expression" "0"}
    :test-fn  #(= 200 (:status %))}

   {:name     "Calculate endpoint requires an expression"
    :endpoint "/calculate"
    :test-fn  #(and
                 (= 400 (:status %))
                 (= ["missing required key"]
                    ((humanized %) :expression)))}])

(defn- start-calculator [file port wait]
  (let [cmd  ["java" "-jar" file "--port" port]
        proc (process cmd {:out :inherit
                           :shutdown destroy-tree})]
    (println (str "Starting calculator: `" (s/join " " cmd) "'"))
    (wait)
    proc))

(defn- create-endpoint [host port]
  (fn [{:keys [endpoint params test-fn]}]
    (let [url (str host ":" port endpoint)
          res (curl/get url {:query-params params :throw false})]
      (if (test-fn res) {} res))))

(defn- run-tests [call-endpoint]
  (let [res  (->> test-cases
                  (map #(assoc % :result (call-endpoint %)))
                  (filter #(seq (:result %)))
                  (map pretty-error))]
    [(count res) (if (empty? res) "Success!" res)]))

(defn run [file options]
  (println (str "Processing file " file " with options " options))
  (let [port  (:port options)
        host  (:host options)
        start (not (:no-startup options))
        wait  #(wait-for-port host port {:timeout 30000 :pause 500})
        call-endpoint (create-endpoint host port)]
    (when start (start-calculator file port wait))
    (run-tests call-endpoint)))

(comment
  (let [call-endpoint (create-endpoint "localhost" "3000")]
    (call-endpoint {:endpoint "/calculate"
                    :params {"expression" "3&7"}
                    :test-fn #(= 200 (:status %))}))
  (->
    (curl/get "localhost:3000/calculate" {:query-params {} :throw false})
    :body
    (json/parse-string true)
    :humanized))
