(ns trends.model
  (:use 
   [clojure.contrib.json.write]
   [clojure.contrib.json.read]
   [compojure]))

(def users (ref {1 {:id 1
		    :username "shughes"
		    :password "2360f41705bad0c72affa41a59b9218a"
		    :karma 0}
		 "shughes" 1}))

(def data (ref {:articles [{:description "the description", :body "the body"}
			   {:description "article two", :body "this is my bodddy"}]
		:trends {1 {:subject "Are you into Sam Hughes?", 
			    :id 1
			    :karma 20
			    :trend "the trend 1"
			    :userid 1}
			 2 {:subject "What's latest on Sam Hughes"
			    :id 2 
			    :karma 35 
			    :trend "the trend 2"
			    :userid 1}
			 3 {:subject "The latest Obama scanre"
			    :id 3 
			    :karma 20
			    :trend "what's the deal with this Obama character?"
			    :userid 1}}
		}))

(defn get-users []
  @users)

(defn get-trends []
  (@data :trends))

(defn add-trend 
  ([userid subject trend]
     (let [trends (@data :trends)
	   size (inc (count trends))]
       (add-trend {:trend trend, :subject subject, :karma 0, :id size, :userid userid})))
  ([trend]
     (dosync
      (let [trends (assoc (@data :trends) (trend :id) trend)]
	(ref-set data (assoc @data :trends trends))))))

(defmulti get-user class)

(defmethod get-user String [username]
  (@users (@users username)))

(defmethod get-user Integer [id]
  (@users id))

(defn get-trend [trendid]
  ((@data :trends) trendid))

(defn get-trend-karma [trendid]
  (((@data :trends) trendid) :karma))

(defn set-trend-karma [trendid karma]
  (dosync
   (let [trend ((@data :trends) trendid)
	 new-trend (assoc trend :karma karma)
	 new-trends (assoc (@data :trends) trendid new-trend)]
     (ref-set data (assoc @data :trends new-trends)))))

(defn set-karma [userid karma]
  (let [user (get-user userid)]
    (dosync
     (let [user (assoc user :karma karma)]
       (ref-set users (assoc @users userid user))))))

(defn get-cookie-username [cookies]
  ((read-json (cookies :userdata)) "username"))
