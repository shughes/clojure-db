(ns trends.models.user
  (:use 
   [trends.general]))

(derive java.lang.Integer ::int)
(derive clojure.lang.PersistentArrayMap ::map)

(defstruct -user :name :username :password)

(defn get-users [args]
  (db-find "users" args))

(defmulti get-user class)

(defmethod get-user ::int [id]
  (first (get-users {:where (str "id=" id)})))

(defmethod get-user ::map [args]
  (first (get-users args)))

(defn set-user [user]
  (db-update :users ["id=?" (user :id)] user))

(defn add-user [user]
  (db-insert :users 
	     [:name :username :password]
	     [(user :name) (user :username) (user :password)]))