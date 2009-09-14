(ns trends.security
  (:use 
   [compojure]
   [trends.models.user]
   [trends.views.layout]
   [clojure.contrib.json.write]
   [clojure.contrib.json.read])
  (:import
   [java.security NoSuchAlgorithmException MessageDigest]))

(defn ensure-logged-in 
  "Returns true if there's a logged in user session. Otherwise, returns false."
  [cookies]
  (if (and (not= (cookies :userdata) nil) (not= (cookies :userdata) "nil")
	   (not= (cookies :userdata) ""))
    (let [user (read-json (cookies :userdata))
	  real-user (get-user (user "username"))]
      (if (= (user "password") (real-user :password))
	true
	false))
    false))

(defn logged-in-only 
  "Like with-user, but user session must exist for function f to be called.
   Otherwise, will be redirect to login.

   function f takes user and request as arguments."
  [f request]
  (if (ensure-logged-in (request :cookies))
    (f (get-user (get-cookie-username (request :cookies))) request)
    (redirect-to "/login")))

(defn with-user 
  "Will pass user into function f if the user cookie exists. Otherwise
   show page without a user session.
   
   Pass function f along with request. Cookies will be converted to user 
   structure, which will be passed to function f, along with the request."
  [f request]
  (if (ensure-logged-in (request :cookies))
    (f (get-user (get-cookie-username (request :cookies))) request)
    (f nil request)))

(defn md5 
  "Return md5 hash string"
  [str]
  (let [alg (doto (MessageDigest/getInstance "MD5")
	      (.reset)
	      (.update (.getBytes str)))]
    (try
     (.toString (new BigInteger 1 (.digest alg)) 16)
     (catch NoSuchAlgorithmException e
       (throw (new RuntimeException e))))))

(defn setup-session 
  "If user with md5 hashed password exists, sets user cookie."
  [username password]
  (let [user (get-user username)]
    (if (= nil user) 
      nil
      (if (= (user :password) (md5 password))
	(set-cookie "userdata" (json-str {:username username, :password (md5 password)}))
	nil))))