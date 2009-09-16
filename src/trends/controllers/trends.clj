(ns trends.controllers.trends
  (:use [compojure]
	[trends.views.layout]
	[trends.security]
	[trends.models.user]
	[trends.models.trend]
	[trends.models.comment]))

(defn- display-trend [user request]
  (let [params (request :params)
	trend (first (get-comments {:where (str "id = " (params :id))}))]
    (if (not= nil trend)
      (page user (trend :comment))
      (redirect-to "/404.html"))))

(defn- points-to-apply [userid trendid dir]
  (let [h (get-comment-history userid trendid)]
    (if (not= nil h)
      (let [karma (h :karma)]
	(cond 
	  (or (= 0 karma) (= nil karma)) (if (= 1 dir) 1 -1)
	  (= 1 karma) (if (= 1 dir) 0 -1)
	  (= -1 karma) (if (= 1 dir) 1 0)
	  :else 0))
      0)))

(defn- vote [user request]
  (let [params (request :params)
	trendid (new Integer (params :id))
	dir (if (= (params :dir) "up") 1 0)
	points (points-to-apply (user :id) trendid dir)
	h (get-comment-history (user :id) trendid)]
    (if (and (not= 0 points) (not= nil h))
      (let [karma (h :karma)]
	(set-comment-history (user :id) trendid points)))
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
    (let [trend (first trends)]
      (concat 
       (vector (link-to (str "/trend/vote/" (trend :id) "/up") "up") 
	       "/"
	       (link-to (str "/trend/vote/" (trend :id) "/down") "down")
	       " "
	       [:b (link-to (str "/trend/id/" (trend :id)) (trend :subject)) " "] 
		(trend :karma) [:br])
       (show-trends (rest trends))))))

(defn show-list [user request]
  (let [trends (get-comments {:where "is_trend = 'true'", :orderby "weight", :limit 10})]
    (page user (vec (concat (list :div) (show-trends trends))))))

(defn trend-context []
  (list (GET "/vote/:id/:dir" (logged-in-only vote request))
	(GET "/submit" (logged-in-only submit request))
	(POST "/submit" (logged-in-only submit-post request))
	(GET "/list" (with-user show-list request))
	(GET "/id/:id" (with-user display-trend request))
	(ANY "*" (redirect-to "/404.html"))))

