(ns trends.submit
  (:use [trends.model]
	[compojure]))

(defn submit-controller 
  ([user params] 
     (add-trend (params :subject) (params :trend))
     (redirect-to "/home"))
  ([user] 
     (html
      [:body [:form {:method "post"}
	      "title " [:input {:name "subject", :type "text"}]
	      [:br]
	      "trend " [:textarea {:name "trend"}]
	      [:br]
	      [:input {:type "submit", :value "submit"}]]])))