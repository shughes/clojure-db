(ns db.main 
  (:use [clojure.contrib.test-is])
  (:require [db.btree :as b]
	    [db.file :as f]
	    [db.context :as c]
	    [db.model :as m])
  (:import [java.io File]
	   [java.nio ByteBuffer]))

(defn- insert []
  (b/insert "animal")
  (b/insert "cat")
  (b/insert "you")
  (b/insert "dog")
  (b/insert "mouse")
  (b/insert "rat")
  (b/insert "facebook")
  (b/insert "excel")
  (b/insert "entertain"))

(defn- setup-tests []
  (c/set-dbm "test-model.db")
  (dosync
   (ref-set f/test-dbm (f/open "test-file.db"))))

(defn- shutdown-tests []
  (f/close (c/get-dbm))
  (f/close @f/test-dbm))

(defn- main []
  (setup-tests)
  (run-tests 'db.model)
  (run-tests 'db.file)
  (shutdown-tests))

(main)