(ns trends.views.layout
  (:use 
   [compojure]
   [clojure.contrib.json.write]
   [clojure.contrib.json.read])
  (:import
   [java.security NoSuchAlgorithmException MessageDigest]))

(defn- logged-in-menus [user]
  (if (not= user nil)
    (html
     [:li (link-to (str "/users/edit/" (user :id)) (user :username))]
     [:li (link-to "/logout" "logout")])
    (html
     [:li (link-to "/login" "login")]
     [:li (link-to "/users/register" "register")])))

(defn page [user content]
  (html
   [:html [:head [:link {:rel "stylesheet" :href "/style.css"}]]
    [:body 
     [:ul {:class "menu"} 
      [:li (link-to "/trend/list" "home")]
      [:li (link-to "/users/leaders" "leaders")]
      [:li (link-to "/trend/submit" "submit")]
      (logged-in-menus user)]
      [:div {:id "content"} content]]]))

(defn error-page-view []
  "Page Not Found")

(defn get-cookie-username [cookies]
  ((read-json (cookies :userdata)) "username"))