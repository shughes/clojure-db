(ns clojure.messaging.users
  (:import
   [java.security NoSuchAlgorithmException MessageDigest]))

(def db {"shughes" "2360f41705bad0c72affa41a59b9218a"})

(defn- md5 [str]
  (let [alg (doto (MessageDigest/getInstance "MD5")
	      (.reset)
	      (.update (.getBytes str)))]
    (try
     (.toString (new BigInteger 1 (.digest alg)) 16)
     (catch NoSuchAlgorithmException e
       (throw (new RuntimeException e))))))

(defn verify [username password]
  (if (= (db username) password)
    true
    false))