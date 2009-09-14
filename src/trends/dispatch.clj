(ns trends.dispatch
  (:require
   [trends.controllers.users :as users]
   [trends.controllers.trends :as trend]
   [trends.controllers.login :as login]
   [trends.security :as security]
   [trends.views.layout :as general])
  (:use 
   [clojure.contrib.sql]
   [clojure.contrib.test-is]
   [clojure.contrib.json.read]
   [compojure]))

(declare *context*)

(defn- route-context
  [ctx route-seq]
  (let [handler (apply routes route-seq)
	pattern (re-pattern (str "^" ctx "(/.*)?"))]
    (fn [request]
      (if-let [[_ uri] (re-matches pattern (:uri request))]
	(binding [*context* ctx]
	  (handler (assoc request :uri uri)))))))

(defn url [path]
  (str *context* path))

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

(defroutes webservice
  (route-context "/users" (users/users-context))
  (route-context "/trend" (trend/trend-context))
  (route-context "/login" (login/login-context))
  (ANY "/logout" (logout))
  (ANY #"(/*)" (security/with-user trend/show-list request))
  (ANY "/:name.css" (serve-file (str (params :name) ".css")))
  (ANY "/*" [404 (general/error-page-view)]))