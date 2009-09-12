(ns trends.login
  (:use 
   [clojure.contrib.json.write]
   [trends.general]
   [trends.security]
   [trends.model]
   [compojure]))

(defn- login-view []
  (page 
    [:form {:method "post"}
     "User name: "
     [:input {:name "username", :type "text"}]
     [:br]
     "Password: "
     [:input {:name "password", :type "password"}]
     [:br]
     [:input {:type "submit" :value "Login"}]]))

(defn- login-post [params]
  (let [user (get-user (params :username))
	userdata (setup-session (params :username) (params :password))]
    (if (not= nil userdata)
      [302 {:headers {"Location" "/" 
		      "Set-Cookie" ((userdata :headers) "Set-Cookie")}}]
      (redirect-to "/login"))))

(defn login-context []
  (list 
   (GET #"(/*)" (login-view))
   (POST #"(/*)" (login-post params))
   (ANY "*" (redirect-to "/404.html"))))
	