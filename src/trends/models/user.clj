(ns trends.models.user
  (:use 
   [trends.general]))

(defn get-users [args]
  (db-find "users" args))
