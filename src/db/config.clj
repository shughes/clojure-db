(ns db.config
  (:require [db.file :as f]))

(def open (f/open "/Users/shughes/Desktop/test.db"))

(def config {:file "/Users/shughes/Desktop/test.db"
	     :in (open :in)
	     :out (open :out)
	     :n 5})
