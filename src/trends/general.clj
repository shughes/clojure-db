(ns trends.general
  (:import
   [java.util Date])
  (:use
   [compojure]
   [clojure.memcached]
   [trends.views.layout]
   [clojure.contrib.math]
   [clojure.contrib.sql]))


(def page-routes {1 "/trend/list"
		  2 "/trend/id"
		  3 "/trend/vote"
		  4 "/trend/comments"
		  5 "/trend/submit"
		  6 "/login"
		  7 "/logout"
		  8 "/users/leaders"
		  9 "/users/edit"})

(declare *context*)
(declare db)

(def sockets (setup-sockets ["localhost:11211"]))

(defn route-context
  [ctx route-seq]
  (let [handler (apply routes route-seq)
	pattern (re-pattern (str "^" ctx "(/.*)?"))]
    (fn [request]
      (with-connection db
	(if-let [[_ uri] (re-matches pattern (:uri request))]
	  (binding [*context* ctx]
	    (handler (assoc request :uri uri))))))))

(defn url [path]
  (str *context* path))

(defn timestamp []
  (. (new Date) getTime))

(def db {:classname   "org.sqlite.JDBC"
	 :subprotocol "sqlite"
	 :subname     "trends.db"})

(defmacro db-test [& body]
  `(with-connection db
     ~@body))

(defn db-find 
  "Syntax: {:where \"x = y\", :orderby \"z\", :limit a}"
  [table & args]
  (let [arg (if (= nil args) {}
		(first args))
	query 
	(str "select * from " table
	     (if (arg :where)
	       (str " where " (arg :where)))
	     (if (arg :orderby)
	       (str " order by " (arg :orderby)))
	     (if (arg :limit)
	       (str " limit " (arg :limit))))]
    (with-query-results results [query]
      (doseq [record results])
      results)))

(def #^{:private true} bindings (ref (list nil)))

(defn db-bind 
  "f is a function that will be called whenever the database is updated.
   f takes two arguments, table name and action. db-bind's main purpose is 
   for cases where pages are being cached. Those pages should add a 
   function using db-bind, which lets them know when to update their cache."
  [f]
  (dosync
   (ref-set bindings (conj @bindings f))))

(defn call-bind [table action]
  (loop [lst @bindings]
    (if (= (first lst) nil)
      nil
      (do
	((first lst) table action)
	(recur (rest lst))))))

(defmacro db-insert [table cols & vals]
  `(do 
     (call-bind ~table :insert)
     (insert-values ~table ~cols ~@vals)))

(defn db-update 
  "query syntax: [\"field=?\" field]
   vals syntax: {:key val}"
  [table query vals]
  (transaction (update-values table query vals))
  (call-bind table :update))

(defn get-minutes [start end]
  (let [diff (- (double end) (double start))
	seconds (/ diff 1000)
	minutes (/ seconds 60)]
    minutes))

(defn get-hours [start end]
  (/ (get-minutes start end) 60))

(defn now []
  (.getTime (new Date)))

(defn get-display-time [time]
  (let [hours (int (get-hours time (now)))
	minutes (int (get-minutes time (now)))
	ending (cond (> 60 minutes) "minutes"
		     (> 24 hours) "hours"
		     (= 1 hours) "hour"
		     (> 48 hours) "day"
		     (<= 48 hours) "days")]
    (cond (< minutes 60) (str minutes " " ending " ago")
	  (< hours 24) (str (int hours) " " ending " ago")
	  (>= hours 24) (str (int (/ hours 24)) " " ending " ago"))))

(defn round-to [n x]
  (let [pre (expt 10 x)]
    (double (/ (round (* n pre)) pre))))