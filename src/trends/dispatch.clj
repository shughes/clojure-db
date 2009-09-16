(ns trends.dispatch
  (:require
   [trends.controllers.users :as users]
   [trends.controllers.trends :as trend]
   [trends.controllers.login :as login]
   [trends.views.layout :as layout]
   [trends.security :as security])
  (:use 
   [trends.general]
   [clojure.contrib.sql.internal]
   [clojure.contrib.sql]
   [clojure.contrib.test-is]
   [clojure.contrib.json.read]
   [compojure]))


(defn- logout []
  [302 {:headers {"Location" "/login"
		  "Set-Cookie" "userdata=nil; expires=Sat, 01-Jan-2000 00:00:00 GMT"}}])

(defn- my-context [ctx ret]
  (let [pattern (re-pattern (str "^" ctx "(/.*)?"))]
    (fn [request]
      (if-let [val (re-matches (re-pattern pattern) (request :uri))]
	{:status 200
	 :headers {"Content-type" "text/html"}
	 :body ret}))))

(defn- my-route [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "what up playa"})

(defroutes webservice
  (route-context "/users" (users/users-context))
  (route-context "/trend" (trend/trend-context))
  (route-context "/login" (login/login-context))
  (ANY "/:name.css" (serve-file (str (params :name) ".css")))
  (ANY "/logout" (logout))
  (route-context 
   "/" 
   (list (ANY #"(/*)" (security/with-user trend/show-list request))))
  (ANY "/*" [404 (layout/error-page-view)]))