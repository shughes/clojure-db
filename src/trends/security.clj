(ns trends.security
  (:use 
   [clojure.contrib.sql]
   [compojure]
   [trends.views.layout]
   [trends.models.user]
   [clojure.contrib.json.write]
   [clojure.contrib.json.read])
  (:import
   [java.security NoSuchAlgorithmException MessageDigest]))

(defn logged-in? [request]
  (let [session (request :session)]
    (if (empty? session) 
      false
      (let [username (session :username)
	    password (session :password)
	    real-user (first (get-users {:where (str "username='" username "'")}))]
	(cond (= nil real-user) 
	      false
	      (= password (real-user :password))
	      true
	      :else false)))))

(defn logged-in-only 
  "Like with-user, but user session must exist for function f to be called.
   Otherwise, will be redirect to login.

   function f takes user and request as arguments."
  [f request]
  (if (logged-in? request)
    (f (first (get-users {:where (str "username='" 
				      ((request :session) :username) "'")})) request)
      (redirect-to "/login")))

(defn with-user 
  "Will pass user into function f if the user cookie exists. Otherwise
   show page without a user session.
   
   Pass function f along with request. Cookies will be converted to user 
   structure, which will be passed to function f, along with the request."
  [f request]
  (if (logged-in? request)
    (f (first (get-users {:where (str "username='" ((request :session) :username) "'")})) request)
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
  (let [user (first (get-users {:where (str "username='" username "'")}))]
    (if (= nil user) 
      nil
      (if (= (user :password) (md5 password))
	{:username username, :password (md5 password)}
	nil))))


