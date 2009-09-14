(ns trends.controllers.trends
  (:use [compojure]
	[trends.views.layout]
	[trends.security]
	[trends.models.user]
	[trends.models.trend]))

(defn- display-trend [user request]
  (let [trends (get-trends)
	params (request :params)]
    (if (not= nil (trends (new Integer (params :id))))
      (page user ((trends (new Integer (params :id))) :trend))
      (redirect-to "/404.html"))))

(defn- log-user-vote [userid trendid dir]
  (let [h (get-user-history userid)
        key (if (= dir 1)
	      :voted-up
	      :voted-down)
	v (h key)]
    (let [new-v (assoc v trendid true)
	  new-h (assoc h key new-v)]
      (set-user-history userid new-h))))
    
(defn- user-has-voted [userid trendid dir]
  (let [h (get-user-history userid)]
    (let [result (if (= dir 1)
		   (h :voted-up)
		   (h :voted-down))]
      (if (= nil result)
	false
	(result trendid)))))

(defn- points-to-apply [userid trendid dir]
  (let [h (get-user-history userid)
	t ((h :voted) trendid)]
    (cond 
      (or (= 0 t) (= nil t)) (if (= 1 dir) 1 -1)
      (= 1 t) (if (= 1 dir) 0 -2)
      (= -1 t) (if (= 1 dir) 2 0)
      :else 0)))

(defn- vote [user request]
  (let [params (request :params)
	trendid (new Integer (params :id))
	dir (if (= (params :dir) "up") 1 0)
	points (points-to-apply (user :id) trendid dir)]
    (if (not= 0 points)
      (let [old1 (get-user-vote-history (user :id) trendid)
	    old-points (if (= nil old1) 0 old1)
	    old-karma (get-trend-karma trendid)]
	(set-user-vote-history (user :id) trendid (+ old-points points))
	(set-trend-karma trendid (+ old-karma points))))
    (redirect-to (str "/trend/id/" trendid))))

(defn- submit [user request]
  (page user [:form {:method "post"}
	 "title " [:input {:name "subject", :type "text"}]
	 [:br]
	 "trend " [:textarea {:name "trend"}]
	 [:br]
	 [:input {:type "submit", :value "submit"}]]))

(defn- submit-post [user request]
  (let [params (request :params)]
	(add-trend (user :id) (params :subject) (params :trend))
	(redirect-to "/trend/list")))

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

(defn show-list [user request]
  (page user (vec (concat (list :div) (show-trends (get-trends))))))
  
(defn trend-context []
  (list (GET "/vote/:id/:dir" (logged-in-only vote request))
	(GET "/submit" (logged-in-only submit request))
	(POST "/submit" (logged-in-only submit-post request))
	(GET "/list" (with-user show-list request))
	(GET "/id/:id" (with-user display-trend request))
	(ANY "*" (redirect-to "/404.html"))))
