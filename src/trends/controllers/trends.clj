(ns trends.controllers.trends
  (:import [java.util Date Calendar])
  (:use [compojure]
	[clojure.contrib.math]
	[trends.general]
	[trends.views.layout]
	[trends.security]
	[trends.models.user]
	[trends.models.comment]))

(defn- get-info-line [comment]
  (html
   (get-karma (comment :id)) " points by "
   ((get-user (comment :userid)) :username) " "
   (get-display-time (comment :time_posted))))

(defn- get-points-line [comment]
  (html (link-to (str "/trend/vote/" (comment :id) "/up") "up") 
	"/"
	(link-to (str "/trend/vote/" (comment :id) "/down") "down")))

(defn- display-comments [user parentid]
    (loop [comments (get-comments {:where (str "parentid=" parentid)
				   :orderby "weight DESC"})
	   view [:ul]]
      (cond 
	(and (= nil (first comments)) (= view [:ul])) ""
	(= nil (first comments)) view
	:else (let [comment (first comments)]
	  (recur (rest comments) 
		 (conj view 
		       [:li (get-points-line comment) " "
			(get-info-line comment)
			[:br]
			(comment :comment)
			[:br]
			(link-to (str "/trend/id/" (comment :id)) "reply")
			(display-comments user (comment :id))]))))))

(defn- display-trend [user request]
  (let [params (request :params)
	trend (first (get-comments {:where (str "id = " (params :id))}))]
    (if (not= nil trend)
      (page 
       user 
       (html (get-points-line trend) " | "
	     (if (= "true" (trend :is_trend)) 
	       (trend :subject)
	       (link-to (str "/trend/id/" (trend :parentid)) "parent"))
	     [:br]
	     (get-info-line trend)
	     [:br]
	     (trend :comment)
	     [:br]
	     [:h1 "Comment"]
	     [:form {:method "post"}
	      [:textarea {:name "comment"}]
	      [:input {:type "hidden", :name "id", :value (trend :id)}]
	      [:br]
	      [:input {:type "submit", :value "add comment"}]]
	     [:br]
	     (display-comments user (params :id)))))))

(defn- submit-comment [user request]
  (let [params (request :params)
	parent (get-comment (new Integer (params :id)))
	comment {:is_trend false,
		 :subject (str "RE:" (parent :subject))
		 :comment (params :comment)
		 :parentid (params :id)
		 :userid (user :id)}]
    (add-comment comment)
    (redirect-to (str "/trend/id/" (params :id)))))

(defn- get-new-rank [comment]
  (if (= nil comment)
    nil
    (let [karma (get-karma (comment :id))
	  age (get-hours (comment :time_posted) (now))
	  base (- karma 1)
	  num (if (> base 0) (expt base 0.8) base)
	  den (expt (+ age 2) 1.8)
	  rank (/ num den)]
      rank)))

(defn- updated-ranks []
  (loop [lst (get-comments)
	 result '()]
    (let [comment (first lst)]
      (if (= nil comment) result
	  (recur (rest lst) (conj result {:id (comment :id), :rank (get-new-rank comment)}))))))

(defn- adjust-ranks []
  (doseq [rank (updated-ranks)]
    (let [comment (get-comment (rank :id))
	  new-comment (assoc comment :weight (rank :rank))]
      (set-comment new-comment))))

(defn test-karma []
  (db-test (adjust-ranks)))


(defn- points-to-apply 
  "Returns points to apply to comment. If user has voted on comment before,
   they can only vote in the opposite direction. Otherwise, 0 points is 
   returned."
  [userid trendid dir]
  (let [h (get-comment-history trendid userid)]
    (if (not= nil h)
      (let [karma (h :karma)]
	(cond 
	  (or (= 0 karma) (= nil karma)) (if (= 1 dir) 1 -1)
	  (= 1 karma) (if (= 1 dir) 0 -1)
	  (= -1 karma) (if (= 1 dir) 1 0)
	  :else 0))
      (do
	(set-comment-history userid trendid 0)
	(if (= dir 1) 1 -1)))))

(defn- vote [user request]
  (let [params (request :params)
	trendid (new Integer (params :id))
	dir (if (= (params :dir) "up") 1 0)
	points (points-to-apply (user :id) trendid dir)]
    (if (not= 0 points)
      (set-comment-history (user :id) trendid points))
    (redirect-to (str "/trend/id/" trendid))))

(defn- submit [user request]
  (page user 
	[:div
	 [:h1 "Submit"]
	 [:form {:method "post"}
	  [:ul.list
	   [:li.left [:label {:for "subject"} "title:"]]
	   [:li [:input {:name "subject", :id "subject", :type "text"}]]]
	  [:ul.list
	   [:li.left [:label {:for "trend"} "trend:"]]
	   [:li [:textarea {:name "trend", :id "trend"}]]]
	  [:ul.list
	   [:li [:input {:type "submit", :value "submit"}]]]]]))

(defn- submit-post [user request]
  (let [params (request :params)]
    (add-comment (struct -comment nil true (params :subject) (params :trend) 0.0 (user :id) -1))
    (redirect-to "/trend/list")))
    
(defn- get-karma-total 
  "Adds up total karma points for a comment"
  [lst]
  (let [history (first lst)]
    (cond (= nil history) 0
	  :else (+ (history :karma) (get-karma-total (rest lst))))))



(defn- show-trends [trends]
  (if (= (first trends) nil) 
    ""
    (let [trend (first trends)
	  history (get-comment-history {:where (str "comment_id = " (trend :id))})]
      (str (html			
	    [:ul.list
	     [:li.left (get-points-line trend)]
	     [:li
	      [:b (link-to (str "/trend/id/" (trend :id)) (trend :subject))]
	      [:br]
	      (get-info-line trend)]]
	    [:br])
	   (show-trends (rest trends))))))

(defn show-list [user request]
  (let [trends (get-comments {:where "is_trend = 'true'", :orderby "weight desc", :limit 10})]
    (page user (show-trends trends))))

(defn- get-comment-list [comments]
  (if (= (first comments) nil) ""
      (let [comment (first comments)]
	(str (html [:ul.list 
		    [:li.left (get-points-line comment)]
		    [:li (get-info-line comment) " " 
		     (link-to (str "/trend/id/" (comment :id)) "link")
		     " | "
		     (link-to (str "/trend/id/" (comment :parentid)) "parent")
		     [:br]
		     (comment :comment)]]
		   (get-comment-list (rest comments)))))))

(defn- show-comments [user request]
  (let [comments (get-comments {:where "parentid != -1", :orderby "weight desc"})]
    (page user
	  (html [:h1 "Comments"]
		(get-comment-list comments)))))

(defn trend-context []
  (list
   (GET "/comments" (with-user show-comments request))
   (GET "/vote/:id/:dir" (logged-in-only vote request))
   (GET "/submit" (logged-in-only submit request))
   (POST "/submit" (logged-in-only submit-post request))
   (GET "/list" (with-user show-list request))
   (GET "/id/:id" (with-user display-trend request))
   (POST "/id/:id" (logged-in-only submit-comment request))))