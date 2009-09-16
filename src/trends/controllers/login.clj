(ns trends.controllers.login
  (:use 
   [clojure.contrib.json.write]
   [trends.general]
   [trends.views.layout]
   [trends.security]
   [trends.models.user]
   [compojure]))

(defn- login-view [user request]
  (page user
    [:form {:method "post"}
     "User name: "
     [:input {:name "username", :type "text"}]
     [:br]
     "Password: "
     [:input {:name "password", :type "password"}]
     [:br]
     [:input {:type "submit" :value "Login"}]]))

(defn- login-post [request]
  (let [params (request :params)
	user (first (get-users {:where (str "username='" (params :username) "'")}))
	userdata (setup-session (params :username) (params :password))]
    (if (not= nil userdata)
      [302 {:headers {"Location" "/" 
		      "Set-Cookie" ((userdata :headers) "Set-Cookie")}}]
      (redirect-to "/login"))))

(defn login-context []
  (list
   (GET #"(/*)" (with-user login-view request))
   (POST #"(/*)" (login-post request))
   (ANY "*" (redirect-to "/404.html"))))