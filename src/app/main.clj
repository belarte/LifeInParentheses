(ns app.main
  (:require [clojure.tools.logging :as log]
            [clojure.repl :as repl]
            [server.server :as server])
  (:gen-class))

(defn shutdown!
  [& _]
  (log/info "Received SIGINT, Shutting down")
  (server/stop)
  (shutdown-agents)
  (System/exit 0))

(defn -main
  [& _]
  (repl/set-break-handler! shutdown!)
  (server/start))
