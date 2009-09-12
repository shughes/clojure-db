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

(defn ensure-logged-in [cookies]
  (if (and (not= (cookies :userdata) nil) (not= (cookies :userdata) "nil"))
    (let [user (read-json (cookies :userdata))
	  real-user (get-user (user "username"))]
      (if (= (user "password") (real-user :password))
	true
	false))
    false))

(defn- call-f [f cookies params]
  (if (not= nil params)
    (f (get-user (get-cookie-username cookies)) (first params))
    (f (get-user (get-cookie-username cookies)))))

(defn logged-in-only [f cookies & params]
  (if (ensure-logged-in cookies)
    (call-f f cookies params)
    (redirect-to "/login")))

(defn with-user 
  "Pass function f along with cookies and/or params. Cookies will be 
   converted to user structure, which will be passed to function f."
  [f cookies & params]
  (if (ensure-logged-in cookies)
    (call-f f cookies params)
    (if (not= nil params)
      (f nil (first params))
      (f nil))))

(defn md5 [str]
  (let [alg (doto (MessageDigest/getInstance "MD5")
	      (.reset)
	      (.update (.getBytes str)))]
    (try
     (.toString (new BigInteger 1 (.digest alg)) 16)
     (catch NoSuchAlgorithmException e
       (throw (new RuntimeException e))))))

(defn setup-session [username password]
  (let [user (get-user username)]
    (if (= nil user) 
      nil
      (if (= (user :password) (md5 password))
	(let [userdata (set-cookie "userdata" (json-str {:username username 
							 :password (md5 password)}))]
	  userdata)
	nil))))

