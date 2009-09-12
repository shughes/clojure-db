(ns trends.submit
  (:use [trends.model]
	[trends.general]
	[compojure]))

(defn submit-controller 
  ([user params] 
     (add-trend (user :id) (params :subject) (params :trend))
     (redirect-to "/home"))
  ([user] 
     (page [:form {:method "post"}
	      "title " [:input {:name "subject", :type "text"}]
	      [:br]
	      "trend " [:textarea {:name "trend"}]
	      [:br]
	      [:input {:type "submit", :value "submit"}]] user)))

(defn submit-context []
  (list (GET #"(/*)" (logged-in-only submit-controller cookies))
	(POST #"(/*)" (logged-in-only submit-controller cookies params))))