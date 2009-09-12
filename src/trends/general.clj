(ns trends.general
  (:use 
   [compojure]
   [trends.model]
   [clojure.contrib.json.write]
   [clojure.contrib.json.read])
  (:import
   [java.security NoSuchAlgorithmException MessageDigest]))

(defn- show-login-menu [user]
  (if (= (count user) 1)
    (if (not= (first user) nil)
      [:li (link-to "/logout" "logout")]
      [:li (link-to "/login" "login")])
    [:li (link-to "/login" "login")]))

(defn page [content & user]
  (html
   [:html [:head [:link {:rel "stylesheet" :href "/style.css"}]]
    [:body 
     [:ul {:class "menu"} 
      [:li (link-to "/trend/submit" "submit")]
      [:li (link-to "/trend/list" "home")]
      (show-login-menu user)]
      [:div {:id "content"} content]]]))

(defn error-page-view []
  "Page Not Found")



