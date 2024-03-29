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
    :params   {"expression" "(63 | 64) & 191"}
    :test-fn  #(= 63 (-> (to-json %) :message :result))}

   {:name     "Calculate does not returns steps or dimensions by default"
    :endpoint "/calculate"
    :params   {"expression" "(63 | 64) & 191"}
    :test-fn  #(let [response (-> (to-json %) :message)]
                 (and
                   (not (contains? response :steps))
                   (not (contains? response :width))
                   (not (contains? response :height))))}

   {:name     "Calculate returns steps and dimensions when asked for"
    :endpoint "/calculate"
    :params   {"expression" "63 & (127 & 255)"
               "steps" true}
    :test-fn  #(let [response (-> (to-json %) :message)]
                 (and
                   (contains? response :steps)
                   (= 117 (count (response :steps)))
                   (= 424 (response :width))
                   (= 35 (response :height))))}

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

(defn- skip-tests [tests]
  (filter #(or
             (not (contains? % :skip))
             (= false (% :skip)))
          tests))

(defn- summary [total ran failed]
  (->> [(str "Tests: " total)
        (str "Skipped: " (- total ran))
        (str "Failed: " failed)]
       (str/join " - ")))

(defn- output [to-run res]
  (str
    \newline
    (if (empty? res) "Success!" (str/join "\n" res))
    \newline \newline
    (summary (count test-cases) (count to-run) (count res))))

(defn- run-tests [call-endpoint]
  (let [to-run (skip-tests test-cases)
        res  (->> to-run
                  (map #(assoc % :result (call-endpoint %)))
                  (filter #(seq (:result %)))
                  (map pretty-error))]
    [(count res) (output to-run res)]))

(defn run [file options]
  (let [port  (:port options)
        host  (:host options)
        start (not (:no-startup options))
        call-endpoint (create-endpoint host port)]
    (when start (start-calculator file port))
    (run-tests call-endpoint)))
