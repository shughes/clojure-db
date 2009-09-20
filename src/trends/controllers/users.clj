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
	   [:label {:for "name"} "Name: "]
	   [:input {:type "text" :name "name" :id "name" :value (user :name)}]
	   [:br]
	   [:label {:for "username"} "Username: "]
	   [:input {:type "text" :name "username" :id "username" :value (user :username)}]
	   [:br]
	   [:label {:for "password"} "Password: "]
	   [:input {:type "password" :name "password" :id "password"}]
	   [:br]
	   [:input {:type "submit" :value submit}]])))

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

(defn- get-user-karma [user]
  (loop [comments (get-comments {:where (str "userid=" (user :id))})
	 k 0]
    (let [comment (first comments)]
      (if (= nil comment) 
	k
	(recur (rest comments) (+ k (comment :karma)))))))

(defn- adjust-user-karma []
  (loop [users (get-users), result []]
    (if (= nil (first users)) result
	(let [user (first users)
	      karma (get-user-karma user)]
	  (recur (rest users) (conj result {:id (user :id), :karma karma}))))))

(db-test (adjust-user-karma))

(defn- display-leaders [user request]
  )

(defn users-context []
  (list 
   (GET "/leaders" (with-user display-leaders request))
   (GET "/register" (with-user register request))
   (POST "/register" (with-user register-post request))
   (POST "/edit/:id" (logged-in-only edit-post request))
   (GET "/edit/:id" (logged-in-only edit request))))