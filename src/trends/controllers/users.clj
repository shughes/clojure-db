(ns trends.controllers.users
  (:require
   [clojure.contrib.str-utils2 :as s2])
  (:use 
   [trends.controllers.login]
   [trends.models.user]
   [trends.models.comment]
   [trends.views.layout]
   [trends.security]
   [trends.general]
   [compojure]))

(defn- reg-form [title submit & users]
  (let [u (first users)
	user (if (not= nil u)
	       u
	       {:name "", :username "", :password ""})]
    (html [:h1 title]
	  [:form {:method "post"}
	   [:ul.list
	    [:li.wide [:label {:for "name"} "Name: "]]
	    [:li [:input {:type "text" :name "name" :id "name" :value (user :name)}]]]
	   [:ul.list
	    [:li.wide [:label {:for "username"} "Username: "]]
	    [:li [:input {:type "text" :name "username" :id "username" :value (user :username)}]]]
	   [:ul.list
	    [:li.wide [:label {:for "password"} "Password: "]]
	    [:li [:input {:type "password" :name "password" :id "password"}]]]
	   [:ul.list
	    [:li [:input {:type "submit" :value submit}]]]])))

(defn- edit [user request]
  (page user (reg-form "Edit" "edit" user)))

(defn- edit-post [user request]
  (let [params (request :params)
	new-user {:id (user :id)
		  :name (s2/trim (params :name))
		  :username (s2/trim (params :username))
		  :password (if (= (s2/trim (params :password)) "")
			      (user :password)
			      (md5 (params :password)))}]
    (if (= nil (get-user {:where (str "username='" (new-user :username) "' "
				      "and id!=" (user :id))}))
      (do
	(set-user new-user)
	(redirect-to (str "/users/edit/" (user :id))))
      "throw that user already exists")))

(defn- register [user request]
  (page user (reg-form "Register" "register")))

(defn- register-post [user request]
  (let [params (request :params)
	users (get-users {:where (str "username='" (params :username) "'")})]
    (if (= nil (first users))
      (let [new-user (struct -user (params :name) (params :username) (md5 (params :password)))]
	(add-user new-user)
	[(session-assoc :username (params :username))
	 (session-assoc :password (md5 (params :password)))
	 (redirect-to "/")])
      (redirect-to "/users/register"))))

(defn- get-user-karma-lst []
  (loop [users (get-users), result []]
    (if (= nil (first users)) result
	(let [user (first users)
	      karma (get-user-karma user)]
	  (recur (rest users) (conj result {:id (user :id), :karma karma}))))))

(defn- adjust-user-karma []
  (doseq [node (adjust-user-karma)]
    (let [user (get-user (node :id))]
      (set-user (assoc user :karma (node :karma))))))

(defn- get-leaders []
  (loop [leaders (get-users {:orderby "karma desc", :limit "100"})
	 result (html "")]
    (if (= nil (first leaders)) 
      result
      (let [leader (first leaders)]
	(recur (rest leaders) 
	       (str result (html [:li (leader :username) " " (leader :karma)])))))))

(defn- display-leaders [user request]
  (page 
   user
   (html [:h1 "Leaders"] [:ol (get-leaders)])))

(defn users-context []
  (list 
   (GET "/leaders" (with-user display-leaders request))
   (GET "/register" (with-user register request))
   (POST "/register" (with-user register-post request))
   (POST "/edit/:id" (logged-in-only edit-post request))
   (GET "/edit/:id" (logged-in-only edit request))))