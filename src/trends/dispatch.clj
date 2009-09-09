(ns trends.dispatch
  (:use 
   [trends.general]
   [trends.trend]
   [trends.model]
   [trends.login]
   [trends.submit]
   [trends.home]
   [clojure.contrib.test-is]
   [compojure]))

(defroutes webservice
  (GET "/trend/:id" (with-user cookies trend-controller params))
  (GET #"/home(/*)" (with-user cookies home-controller))
  (GET #"/submit(/*)" (logged-in-only cookies submit-controller))
  (POST #"/submit(/*)" (logged-in-only cookies submit-controller params))
  (GET #"/login(/*)" (login-view))
  (POST #"/login(/*)" (login-controller params))
  (ANY #"/logout(/*)" (logout-controller))
  (ANY "/404.html" (error-page-view))
  (ANY #"(/*)" (with-user cookies home-controller))
  (ANY "/*" (redirect-to "/404.html")))

(def server (run-server {:port 8080} "/*" (servlet webservice)))
(start server)
