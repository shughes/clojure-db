(ns trends.vote
  (:use [compojure]
	[trends.model]
	[trends.general]))

(defn vote-controller [user params]
  (let [trend ((get-trends) (new Integer (params :id)))
	other-user (get-user (new Integer (trend :userid)))
	karma (other-user :karma)]
    (set-karma (other-user :id) (inc karma))
    (str ((get-user (other-user :id)) :karma))))

