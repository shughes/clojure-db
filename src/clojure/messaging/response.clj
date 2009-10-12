(ns clojure.messaging.response
  (:import 
   [java.io File BufferedReader InputStreamReader])
  (:require 
   [clojure.messaging.users :as users])
  (:use
   [clojure.contrib.json.read :as jread]
   [clojure.contrib.json.write :as jwrite]))

(defn- stream-to-string [stream]
  (let [rdr (new BufferedReader (new InputStreamReader stream))]
    (loop [reader rdr, sb ""]
      (let [line (. reader readLine)]
	(if (= line nil) sb
	    (str sb line "\n"))))))

(defn- sfirst [val]
  (if (not= "" val) 
    (subs val 0 1)
    nil))

(defn- srest [val]
  (if (not= "" val)
    (subs val 1)
    nil))

(defn- strip-uri [uri]
  (cond
    (= (sfirst uri) nil) ""
    (= (sfirst uri) "/") (strip-uri (srest uri))
    :else (str (sfirst uri) (strip-uri (srest uri)))))

(declare do-action)
    
(defn process-post [req resp]
  (. resp (setHeader "Content-Type" "application/json"))
  (let [out (. resp (getOutputStream))
	in (. req getInputStream)]
    (. out (println 
	    (do-action 
	     {:action (strip-uri (. req getRequestURI))} 
	     (jread/read-json (.trim (stream-to-string in))))))))

(defn process-get [req resp]
  (. resp (setHeader "Content-Type" "application/json"))
  (let [out (. resp (getOutputStream))
	qstr (. req getQueryString)]
    (. out (println (jwrite/json-str {:key qstr})))))

(defmulti do-action :action)

(defmethod do-action "login" [arg data]
  (users/verify (data "username") (data "password")))

