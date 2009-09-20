(ns trends.main
  (:use 
   [compojure]
   [trends.dispatch]))

(defn main []
  (let [server (run-server {:port 8080} "/*" (servlet webservice))]
    (start server)))

(main)


