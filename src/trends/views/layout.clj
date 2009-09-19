(ns trends.views.layout
  (:use 
   [compojure]
   [clojure.contrib.json.write]
   [clojure.contrib.json.read])
  (:import
   [java.security NoSuchAlgorithmException MessageDigest]))

(defn- show-username [user]
  (if (not= user nil)
    [:li (link-to (str "/users/edit/" (user :id)) (user :username))]))

(defn- show-login-menu [user]
  (if (not= user nil)
    [:li (link-to "/logout" "logout")]
    [:li (link-to "/login" "login")]))

(defn- show-register-menu [user]
  (if (= user nil)
    [:li (link-to "/users/register" "register")]))

(defn page [user content]
  (html
   [:html [:head [:link {:rel "stylesheet" :href "/style.css"}]]
    [:body 
     [:ul {:class "menu"} 
      [:li (link-to "/trend/list" "home")]
      [:li (link-to "/trend/submit" "submit")]
      (show-username user)
      (show-login-menu user)
      (show-register-menu user)]
      [:div {:id "content"} content]]]))

(defn error-page-view []
  "Page Not Found")

(defn get-cookie-username [cookies]
  ((read-json (cookies :userdata)) "username"))