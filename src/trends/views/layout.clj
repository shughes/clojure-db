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
     [:li "|"]
     [:li (link-to "/logout" "logout")])
    (html
     [:li (link-to "/login" "login")]
     [:li "|"]
     [:li (link-to "/users/register" "register")])))

(defn page [user content]
  (html
   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\""
   " \"http://www.w3.org/TR/html4/strict.dtd\">"
   [:html [:head [:link {:rel "stylesheet" :href "/style.css"}]]
    [:body 
     [:ul.menu
      [:li (link-to "/trend/list" "home")]
      [:li "|"]
      [:li (link-to "/trend/comments" "comments")]
      [:li "|"]
      [:li (link-to "/users/leaders" "leaders")]
      [:li "|"]
      [:li (link-to "/trend/submit" "submit")]
      [:li "|"]
      (logged-in-menus user)]
      [:div {:id "content"} content]]]))

(defn error-page-view []
  "Page Not Found")

(defn get-cookie-username [cookies]
  ((read-json (cookies :userdata)) "username"))