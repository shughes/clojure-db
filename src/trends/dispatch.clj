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


(defn- my-context [ctx ret]
  (let [pattern (re-pattern (str "^" ctx "(/.*)?"))]
    (fn [request]
      (if-let [val (re-matches (re-pattern pattern) (request :uri))]
	{:status 200
	 :headers {"Content-type" "text/html"}
	 :body ret}))))

(defroutes webservice
  ;; JQuery Desktop
  (ANY "/jquery-desktop" (serve-file "/jquery-desktop/desktop.html"))
  (ANY "/jquery-desktop/*" (serve-file (request :uri)))
  (ANY "/assets/*" (serve-file (str "/jquery-desktop" (request :uri))))

  (ANY "/logout" (login/logout))
  (route-context "/users" (users/users-context))
  (route-context "/trend" (trend/trend-context))
  (route-context "/login" (login/login-context))
  (ANY "/:name.css" (serve-file (str (params :name) ".css")))
  (ANY "/:name.otf" (serve-file (str (params :name) ".otf")))
  (route-context "/" (list (ANY #"(/*)" (security/with-user trend/show-list request))))
  (ANY "/*" [404 (layout/error-page-view)]))

;; add session capability to be stored as cookies.
(decorate webservice
	  (with-session :cookie))


