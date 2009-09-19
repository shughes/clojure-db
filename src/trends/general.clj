(ns trends.general
  (:import
   [java.util Date])
  (:use
   [compojure]
   [trends.views.layout]
   [clojure.contrib.sql]))

(declare *context*)
(declare db)

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

(defmacro db-insert [table cols & vals]
  `(insert-values ~table ~cols ~@vals))

(defn db-update 
  "query syntax: [\"field=?\" field]
   vals syntax: {:key val}"
  [table query vals]
  (transaction (update-values table query vals)))
