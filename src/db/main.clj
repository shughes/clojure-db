(ns db.main 
  (:require [db.config :as c]
	    [db.btree :as b]
	    [db.file :as f])
  (:import [java.io File]))

;(defstruct page :id :in :out :flags :parent :data :children)
(defn- main []
  (b/insert "animal")
  (b/insert "dog")
  (b/insert "cat")
  (b/insert "castle")
  (b/insert "family")
  (println (f/get-root (c/config :in)))
  
;  (let [bytes (vec (f/file-to-bytes (new File (c/config :file))))]
;    (dotimes [i (count bytes)]
      ;(print (str (bytes i) " "))))
  (f/close c/config))

(main)