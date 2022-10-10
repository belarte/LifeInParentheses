(ns tests
  (:require [framework :refer [to-json pretty-error create-endpoint start-calculator]]
            [clojure.string :as str]))

(def test-cases
  [{:name     "Health check"
    :endpoint "/health"
    :test-fn  #(= 200 (:status %))}

   {:name     "Unknown endpoint returns 404"
    :endpoint "/does-not-exit"
    :test-fn  #(= 404 (:status %))}

   {:name     "Calculate is responsive"
    :endpoint "/calculate"
    :params   {"expression" "0"}
    :test-fn  #(= 200 (:status %))}

   {:name     "Calculate returns the correct answer"
    :endpoint "/calculate"
    :params   {"expression" "(63|64)&191"}
    :test-fn  #(= 63 (-> (to-json %) :message :result))}

   {:name     "Calculate requires an expression"
    :endpoint "/calculate"
    :test-fn  #(and
                 (= 400 (:status %))
                 (= ["missing required key"]
                    (-> (to-json %) :humanized :expression)))}

   {:name     "Calculate requires a valid expression"
    :endpoint "/calculate"
    :params   {"expression" "(63|"}
    :test-fn  #(and
                 (= 400 (:status %))
                 (= "malformed expression: '(63|'"
                    (-> (to-json %) :message str/lower-case)))}])

(defn- run-tests [call-endpoint]
  (let [res  (->> test-cases
                  (map #(assoc % :result (call-endpoint %)))
                  (filter #(seq (:result %)))
                  (map pretty-error))]
    [(count res) (if (empty? res) "Success!" (str/join "\n" res))]))

(defn run [file options]
  (let [port  (:port options)
        host  (:host options)
        start (not (:no-startup options))
        call-endpoint (create-endpoint host port)]
    (when start (start-calculator file port))
    (run-tests call-endpoint)))
