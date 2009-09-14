(ns trends.models.user
  (:use 
   [clojure.contrib.json.write]
   [clojure.contrib.json.read]
   [clojure.contrib.sql]
   [compojure]))

(def users (ref {1 {:id 1
		    :username "shughes"
		    :password "2360f41705bad0c72affa41a59b9218a"
		    :name "Samuel Hughes"
		    :karma 0
		    :weight 1.0}}))

(def history (ref {1 {:id 1
		      :voted {2 1}}}))

(def users-username (ref {"shughes" 1}))

(defn- new-history [userid]
  {:id userid
   :voted {}})

(defn get-users []
  @users)

(defmulti get-user class)

(defmethod get-user String [username]
  (@users (@users-username username)))

(defmethod get-user Integer [id]
  (@users id))

(defn set-user-karma [userid karma]
  (let [user (get-user userid)]
    (dosync
     (let [user (assoc user :karma karma)]
       (ref-set users (assoc @users userid user))))))

(defn get-user-history [userid]
  (let [h (@history userid)]
    (if (= nil h)
      (new-history userid)
      h)))

(defn get-user-vote-history [userid trendid]
  (((get-user-history userid) :voted) trendid))

(defn set-user-vote-history [userid trendid karma]
  (let [h (get-user-history userid)
	v (h :voted)]
    (dosync
     (ref-set history (assoc @history userid (assoc h :voted (assoc v trendid karma)))))))

(defn set-user-history [userid h]
  (dosync
   (ref-set history (assoc @history userid h))))
