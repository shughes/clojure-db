(ns trends.controllers.login
  (:use 
   [clojure.contrib.json.write]
   [trends.general]
   [trends.views.layout]
   [trends.security]
   [trends.models.user]
   [compojure]))

(defn logout []
  [(clear-session)
   (redirect-to "/login")])

(defn- login-view [user request]
  (page user
	(html [:h1 "Login"]
	      [:form {:method "post"}
	       [:ul.list
		[:li.wide [:label {:for "username"} "User name:"]]
		[:li [:input {:id "username", :name "username", :type "text"}]]]
	       [:ul.list
		[:li.wide [:label {:for "password"} "Password:"]]
		[:li [:input {:id "password", :name "password", :type "password"}]]]
	       [:ul.list
		[:li [:input {:type "submit" :value "Login"}]]]])))

(defn login-post [request]
  (let [params (request :params)
	user (first (get-users {:where (str "username='" (params :username) "'")}))]
    (if (not= nil user)
      [(session-assoc :username (user :username))
       (session-assoc :password (user :password))
       (redirect-to "/")]
      (redirect-to "/login"))))

(defn login-context []
  (list
   (GET #"(/*)" (with-user login-view request))
   (POST #"(/*)" (login-post request))
   (ANY "*" (redirect-to "/404.html"))))