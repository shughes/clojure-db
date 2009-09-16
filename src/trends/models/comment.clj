(ns trends.models.comment
  (:use
   [trends.general]
   [trends.models.user]
   [clojure.contrib.sql]))

(defn get-comments [args]
  (trends.general/db-find "comments" args))

(defn- get-comment-history* [args]
  (trends.general/db-find "comment_history" args))

(defn get-comment-history [userid commentid]
  (let [comment (first (get-comments {:where (str "id = " commentid)}))
	user (first (get-users {:where (str "id = " userid)}))
	comment_history (first (get-comment-history* {:where (str "user_id = " userid 
								  " and comment_id = " commentid)}))]
    (if (or (= nil comment) (= nil user) (= nil comment_history))
      nil
      {:id (comment_history :id)
       :karma (comment_history :karma)
       :comment comment
       :user user})))

(defn- add-comment-history [userid commentid karma]
  (let [cols [:comment_id :user_id :karma]
	vals [commentid userid karma]]
    (db-insert :comment_history cols vals)))

(defn set-comment-history [userid commentid karma]
  (let [h (get-comment-history userid commentid)]
    (if (= nil h)
      (let [user (first (get-users {:where (str "id=" userid)}))
	    comment (first (get-comments {:where (str "id=" commentid)}))]
	(if (and (not= nil user) (not= nil comment))
	  (add-comment-history userid commentid karma)
	  "can't do it"))
      (db-update :comment_history ["id=?" (h :id)] {:karma karma}))))

	