(ns trends.general
  (:use 
   [compojure]
   [trends.model]
   [clojure.contrib.json.write]
   [clojure.contrib.json.read])
  (:import
   [java.security NoSuchAlgorithmException MessageDigest]))

(defn error-page-view []
  "Page Not Found")

(defn ensure-logged-in [cookies]
  (if (and (not= (cookies :userdata) nil) (not= (cookies :userdata) "nil"))
    (let [user (read-json (cookies :userdata))
	  real-user ((@data :users) (user "username"))]
      (if (= (user "password") (real-user :password))
	true
	false))
    false))

(defn logged-in-only [cookies f & params]
  (if (ensure-logged-in cookies)
    (if (not= nil params)
      (f (get-user (get-cookie-username cookies)) (first params))
      (f (get-user (get-cookie-username cookies))))
    (redirect-to "/login")))

(defn with-user [cookies f & params]
  (if (ensure-logged-in cookies)
    (if (not= nil params)
      (f (get-user (get-cookie-username cookies)) (first params))
      (f (get-user (get-cookie-username cookies))))
    (f nil)))

(defn md5 [str]
  (let [alg (doto (MessageDigest/getInstance "MD5")
	      (.reset)
	      (.update (.getBytes str)))]
    (try
     (.toString (new BigInteger 1 (.digest alg)) 16)
     (catch NoSuchAlgorithmException e
       (throw (new RuntimeException e))))))