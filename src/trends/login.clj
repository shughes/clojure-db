(ns trends.login
  (:use 
   [clojure.contrib.json.write]
   [trends.general]
   [trends.model]
   [compojure]))

(defn login-view []
  (html
   [:body
    [:form {:method "post"}
     "User name: "
     [:input {:name "username", :type "text"}]
     [:br]
     "Password: "
     [:input {:name "password", :type "password"}]
     [:br]
     [:input {:type "submit" :value "Login"}]]]))

(defn login-controller [params]
  (let [user ((@data :users) (params :username))]
    (if (= (user :password) (md5 (params :password)))
      (let [userdata (set-cookie "userdata" (json-str {:username (params :username) 
						       :password (md5 (params :password))}))]
	[302 {:headers {"Location" "/articles" 
			"Set-Cookie" ((userdata :headers) "Set-Cookie")}}])
      (redirect-to "/login"))))

(defn logout-controller []
  [302 {:headers {"Location" "/login"
		  "Set-Cookie" "userdata=nil"}}])