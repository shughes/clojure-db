(ns trends.models.user
  (:use 
   [trends.models.comment]
   [trends.general]))

(derive java.lang.Integer ::int)
(derive clojure.lang.PersistentArrayMap ::map)

(defstruct -user :name :username :password)

(defn get-users [& args]
  (if (= nil args) 
    (db-find "users")
    (db-find "users" (first args))))

(defn get-user-karma [user]
  (loop [comments (get-comments {:where (str "userid=" (user :id))})
	 k 0]
    (if (= nil (first comments)) 
      k
      (let [comment (first comments)
	    karma (get-karma (comment :id))]
	(recur (rest comments) (+ k karma))))))

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