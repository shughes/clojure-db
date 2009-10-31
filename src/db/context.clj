(ns db.context
  (:require [db.file :as f])
  (:use [clojure.contrib.def :only (defvar-)]
	[clojure.contrib.types]))

(defvar- dbm (ref nil))

(defn set-dbm [file]
  (dosync 
   (ref-set dbm (f/open file))))

(defn get-dbm []
  @dbm)

