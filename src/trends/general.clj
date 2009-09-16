(ns trends.general
  (:import
   [java.util Date])
  (:use
   [clojure.contrib.sql]))

(defn timestamp []
  (. (new Date) getTime))

(def db {:classname   "org.sqlite.JDBC"
	 :subprotocol "sqlite"
	 :subname     "trends.db"})

(defn db-find 
  "Syntax: {:where \"x = y\", :orderby \"z\", :limit a}"
  [table args]
  (with-connection db
    (let [query 
	  (str "select * from " table
	       (if (args :where)
		 (str " where " (args :where)))
	       (if (args :orderby)
		 (str " order by " (args :orderby)))
	       (if (args :limit)
		 (str " limit " (args :limit))))]
      (with-query-results results [query]
	(doseq [record results])
	results))))

(defn db-insert [table cols vals]
  (with-connection db
    (insert-values table 
		   cols
		   vals)))


(defn db-update 
  "query syntax: [\"field=?\" field]
   vals syntax: {:key val}"
  [table query vals]
  (with-connection db
    (transaction (update-values table query vals))))
