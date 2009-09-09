(ns trends.home
  (:use [compojure]
	[trends.model]))

(defn home-show-trends [trends]
  (if (= (first trends) nil) ""
      (let [trend ((first trends) 1)]
	(str "<b>" (trend :subject) "</b> " (trend :points) "<br />\n"
	     (home-show-trends (rest trends))))))

(defn home-controller [user]
  (html 
   [:html
    [:body (home-show-trends (get-trends))]]))