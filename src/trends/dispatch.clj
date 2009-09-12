(ns trends.dispatch
  (:use 
   [trends.vote]
   [trends.general]
   [trends.model]
   [trends.login :as login]
   [trends.trend :as trend]
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

(defn logout []
  [302 {:headers {"Location" "/login"
		  "Set-Cookie" "userdata=nil"}}])

(defroutes webservice
  (ANY "/logout" (logout))
  (route-context "/trend" (trend/trend-context))
  (route-context "/login" (login/login-context))
  (ANY "/404.html" (error-page-view))
  (ANY "/:name.css" (serve-file (str (params :name) ".css")))
  (ANY #"(/*)" (with-user trend/show-list cookies))
  (ANY "/*" (redirect-to "/404.html")))

(def server (run-server {:port 8080} "/*" (servlet webservice)))
(start server)
