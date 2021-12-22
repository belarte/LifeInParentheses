(ns app.main
  (:require [clojure.tools.logging :as log]
            [clojure.repl :as repl])
  (:gen-class))

(defn shutdown!
  [& _]
  (log/info "Received SIGINT, Shutting down")
  (shutdown-agents)
  (System/exit 0))

(defn -main
  [& _]
  (repl/set-break-handler! shutdown!)
  (println (+ 1 2))
  (Thread/sleep 5000)
  (println "Done"))
