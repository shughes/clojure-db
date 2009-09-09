(ns trends.trend
  (:use [compojure]
	[trends.model]))

(defn trend-controller [user params]
  (let [trends (get-trends)]
    (if (not= nil (trends (new Integer (params :id))))
      ((trends (new Integer (params :id))) :trend)
      (redirect-to "/404.html"))))