(ns tests
  (:require [framework :refer [humanized pretty-error create-endpoint start-calculator]]))

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
        call-endpoint (create-endpoint host port)]
    (when start (start-calculator file port))
    (run-tests call-endpoint)))
