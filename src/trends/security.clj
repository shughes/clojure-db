(ns trends.security
  (:use 
   [compojure]
   [trends.model]
   [clojure.contrib.json.write]
   [clojure.contrib.json.read])
  (:import
   [java.security NoSuchAlgorithmException MessageDigest]))

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

(defn logged-in-only 
  "Like with-user, but user session must exist for function f to be called.
   Otherwise, will be redirect to login.

   function f takes user as first argument, and optionally takes params as
   second argument."
  [f cookies & params]
  (if (ensure-logged-in cookies)
    (call-f f cookies params)
    (redirect-to "/login")))

(defn with-user 
  "Will pass user into function f if the user cookie exists. Otherwise
   show page without a user session.
   
   Pass function f along with cookies and/or params. Cookies will be 
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
	(let [userdata (set-cookie "userdata" 
				   (json-str {:username username, :password (md5 password)}))]
	  userdata)
	nil))))