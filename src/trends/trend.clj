(ns trends.trend
  (:use [compojure]
	[trends.general]
	[trends.security]
	[trends.model]))

(defn- display-trend [user params]
  (let [trends (get-trends)]
    (if (not= nil (trends (new Integer (params :id))))
      (page ((trends (new Integer (params :id))) :trend) user)
      (redirect-to "/404.html"))))

(defn- vote [user params]
  (let [trendid (new Integer (params :id))]
    (if (= (params :dir) "up")
      (set-trend-karma trendid (inc (get-trend-karma trendid)))
      (set-trend-karma trendid (- (get-trend-karma trendid) 1)))
    (redirect-to (str "/trend/id/" trendid))))

(defn- submit [user]
  (page [:form {:method "post"}
	 "title " [:input {:name "subject", :type "text"}]
	 [:br]
	 "trend " [:textarea {:name "trend"}]
	 [:br]
	 [:input {:type "submit", :value "submit"}]] user))

(defn- submit-post [user params]
  (add-trend (user :id) (params :subject) (params :trend))
  (redirect-to "/home"))

(defn- show-trends [trends]
  (if (= (first trends) nil) 
    ""
    (let [trend ((first trends) 1)]
      (concat 
       (vector (link-to (str "/trend/vote/" (trend :id) "/up") "up") 
	       "/"
	       (link-to (str "/trend/vote/" (trend :id) "/down") "down")
	       " "
	       [:b (link-to (str "/trend/id/" (trend :id)) (trend :subject)) " "] 
		(trend :karma) [:br])
       (show-trends (rest trends))))))

(defn show-list [user]
  (page (vec (concat (list :div) (show-trends (get-trends)))) user))
  
(defn trend-context []
  (list (GET "/vote/:id/:dir" (logged-in-only vote cookies params))
	(GET "/submit" (logged-in-only submit cookies))
	(POST "/submit" (logged-in-only submit-post cookies params))
	(GET "/list" (with-user show-list cookies))
	(GET "/id/:id" (with-user display-trend cookies params))
	(ANY "*" (redirect-to "/404.html"))))


