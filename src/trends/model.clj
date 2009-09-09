(ns trends.model
  (:use 
   [clojure.contrib.json.read]
   [compojure]))

(def data (ref {
		:articles [{:description "the description", :body "the body"}
			   {:description "article two", :body "this is my bodddy"}]
		:users {"shughes" {:username "shughes" :password "2360f41705bad0c72affa41a59b9218a"}}
		:trends {1 {:subject "Are you into Sam Hughes?", :id 1, :points 20, :trend "the trend 1"}
			 2 {:subject "What's latest on Sam Hughes", :id 2, :points 35, :trend "the trend 2"}}
		}))

(defn get-trends []
  (@data :trends))

(defn add-trend 
  ([subject trend]
     (let [trends (@data :trends)
	   size (inc (count trends))]
       (add-trend {:trend trend, :subject subject, :points 0, :id size})))
  ([trend]
     (dosync
      (let [trends (assoc (@data :trends) (trend :id) trend)]
	(ref-set data (assoc @data :trends trends))))))

(defn get-user [userid]
  ((@data :users) userid))

(defn get-cookie-username [cookies]
  ((read-json (cookies :userdata)) "username"))