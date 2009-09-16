(ns trends.models.trend
  (:import 
   [java.util Date])
  (:use 
   [trends.general]
   [clojure.contrib.json.write]
   [clojure.contrib.json.read]
   [trends.security]
   [clojure.contrib.sql]
   [compojure]))

(defn- create-tables []
  (with-connection db
    (create-table :comment_history
		  [:id          :integer "primary key"]
		  [:comment_id  :integer]
		  [:user_id     :integer]
		  [:karma       :integer])
    (create-table :users 
		  [:id       :integer "primary key"]
		  [:name     "varchar(200)"] 
		  [:username :text]
		  [:password :text])
    (create-table :comments
		  [:id           :integer "primary key"]
		  [:is_trend     :boolean "default true"]
		  [:subject      :text]
		  [:comment      :text]
		  [:karma	 :integer "default 0"]
		  [:weight       :double  "default 0.0"]
		  [:time_posted	 :long]
		  [:userid       :integer]
		  [:parentid     :integer "default -1"])))

(defn- add-rows []
  (with-connection db
    (insert-values :comment_history
		   [:user_id :comment_id :karma]
		   [1 1 -1])
    (insert-values :users
		   [:name :username :password]
		   ["Samuel Hughes" "shughes" "2360f41705bad0c72affa41a59b9218a"])
    (insert-values :comments
		   [:subject :comment :time_posted :userid]
		   ["trend about samuel" "do you think sam is greater than gandhi?" (timestamp) 1])))


(defn- sample-query []
  (with-connection db
    (with-query-results results ["select * from users"]
      (doseq [record results]
	(println "name = " (record :name))
	(println "username = " (record :username))))))

(def trends (ref {1 {:subject "Are you into Sam Hughes?", 
		     :id 1
		     :karma 20
		     :trend "the trend 1"
		     :weight 1.0
		     :userid 1}
		  2 {:subject "What's latest on Sam Hughes"
		     :id 2 
		     :karma 35 
		     :trend "the trend 2"
		     :weight 1.0
		     :userid 1}
		  3 {:subject "The latest Obama scanre"
		     :id 3 
		     :karma 20
		     :trend "what's the deal with this Obama character?"
		     :weight 1.0
		     :userid 1}}))

(defn get-trends []
  @trends)

(defn add-trend 
  ([userid subject trend]
     (let [size (inc (count @trends))]
       (add-trend {:trend trend, :subject subject, :karma 0, :id size, :userid userid})))
  ([trend]
     (dosync
      (ref-set trends (assoc @trends (trend :id) trend)))))

(defn get-trend [trendid]
  (@trends trendid))

(defn get-trend-karma [trendid]
  ((@trends trendid) :karma))

(defn set-trend-karma [trendid karma]
  (dosync
   (let [trend (@trends trendid)
	 new-trend (assoc trend :karma karma)]
     (ref-set trends (assoc @trends trendid new-trend)))))


