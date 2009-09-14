(ns trends.models.trend
  (:use 
   [clojure.contrib.json.write]
   [clojure.contrib.json.read]
   [clojure.contrib.sql]
   [compojure]))

(def db {:classname   "org.sqlite.JDBC"
	 :subprotocol "sqlite"
	 :subname     "trends.db"})

(defn init-db []
  (with-connection db
    (create-table :users 
		  [:name     :text] 
		  [:username :text]
		  [:password :text]
		  [:karma    :int])
    (create-table :trends
		  [:subject :text]
		  [:karma   :int]
		  [:trend   :text]
		  [:userid  :int])))

(defn contrib-sql []
  (with-connection db
    (create-table :users 
		  [:name :text] 
		  [:username :text]
		  [:password :text]
		  [:karma :int])
    (insert-values :people
		   [:name          :occupation]
		   ["Gandhi"       "politics"]
		   ["Turing"       "computers"]
		   ["Wittgenstein" "smartypants"])
    (with-query-results results ["select * from people"]
      (doseq [record results]
	(println "name =" (:name record))
	(println "job  =" (:occupation record))))))

(def trends (ref {1 {:subject "Are you into Sam Hughes?", 
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


