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
  (GET "/test" (db-test 
		 (str (security/logged-in? request))))

  (fn [request] (if (= (request :uri) "/test")
    {:session {:password (security/md5 "stok6394")}, :status 200, :body "waht up everybody"}))

  (GET "/test"
    {:session {:password (security/md5 "stok6394")}
     :body "Die"})
  (GET "/test2"
    (do (def tmp session)
	"die"))
  (GET "/clear"  [(clear-session) (redirect-to "/test2")])

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

(defn- with-header [handler header value]
  (fn [request]
    (let [response (handler request)]
      (assoc-in response [:headers header] value))))

(defn hello-world [request]
  (let [body "word"]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body body}))

(decorate hello-world
	  (with-header "Test" "test value"))

;(def server2 (run-server {:port 8081} "/*" (servlet hello-world)))
;(start server2)

