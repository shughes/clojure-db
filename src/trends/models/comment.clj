(ns trends.models.comment
  (:use
   [trends.general]
   [trends.models.user]
   [clojure.contrib.sql]))

(derive clojure.lang.PersistentArrayMap ::map)
(derive java.lang.Integer ::int)


(defn get-comments [& args]
  (if (= nil args) (db-find "comments")
      (trends.general/db-find "comments" (first args))))

(defmulti get-comment class)

(defmethod get-comment ::map [args]
  (first (get-comments args)))

(defmethod get-comment ::int [id]
  (get-comment {:where (str "id=" id)}))

(defn- get-comment-history* [args]
  (trends.general/db-find "comment_history" args))

(defn get-comment-history 
  "With 2 args, returns comment history or nil if it doesn't exist"
  ([args]
     (get-comment-history* args))
  ([userid commentid]
     (let [comment (first (get-comments {:where (str "id = " commentid)}))
	   user (first (get-users {:where (str "id = " userid)}))
	   comment_history (first (get-comment-history* {:where (str "user_id = " userid 
								     " and comment_id = " commentid)}))]
       (if (or (= nil comment) (= nil user) (= nil comment_history))
	 nil
	 {:id (comment_history :id)
	  :karma (comment_history :karma)
	  :comment comment
	  :user user}))))

(defn get-karma
  "Adds up total karma points for a comment"
  [id]
  (loop [lst (get-comment-history {:where (str "comment_id=" id)})
	 result 0]
    (let [history (first lst)]
      (if (= nil history) 
	result
	(recur (rest lst) (+ result (history :karma)))))))

(defn- add-comment-history [userid commentid karma]
  (let [cols [:comment_id :user_id :karma]
	vals [commentid userid karma]]
    (db-insert :comment_history cols vals)))

(defn set-comment-history 
  "Sets comment history. If comment history doesn't exist, it will be added."
  [userid commentid karma]
  (let [h (get-comment-history userid commentid)]
    (if (= nil h)
      (let [user (first (get-users {:where (str "id=" userid)}))
	    comment (first (get-comments {:where (str "id=" commentid)}))]
	(if (and (not= nil user) (not= nil comment))
	  (add-comment-history userid commentid karma)
	  "throw something"))
      (db-update :comment_history ["id=?" (h :id)] {:karma karma}))))

(defstruct -comment :time_posted :is_trend :subject :comment :weight :userid :parentid)

(defn set-comment [comment]
  (db-update :comments ["id=?" (comment :id)] comment))

(defn add-comment 
  "Takes a map of field => vals as parameter. Any fields not in the map
   are set to their default value."
  [comment]
  (let [time-posted (if (= nil (comment :time_posted))
		     (timestamp) (comment :time_posted))
	is-trend (if (= nil (comment :is_trend))
		   true (comment :is_trend))
	subject (if (= nil (comment :subject))
		       "No Subject" (comment :subject))
	comment2 (if (= nil (comment :comment))
		  "No Comment" (comment :comment))
	weight (if (= nil (comment :weight))
		 0.0 (comment :weight))
	userid (if (= nil (comment :userid))
		 -1 (comment :userid))
	parentid (if (= nil (comment :parentid))
		   -1 (comment :parentid))]
    (db-insert :comments [:is_trend :subject :comment ::weight :time_posted :userid :parentid]
	       [is-trend subject comment2 weight time-posted userid parentid])))
