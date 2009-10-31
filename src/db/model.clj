(ns db.model
  (:use [clojure.contrib.def :only (defvar-)]
	[clojure.contrib.types]
	[clojure.contrib.test-is])
  (:require [db.file :as f]
	    [db.context :as c]))

(defstruct node :id :parent :children :values)

(defvar- n (ref nil))

(defvar- root (ref nil))

(defn get-node [key]
  (let [page (f/get-page (c/get-dbm) key)
	parent (if (= 0 (page :parent)) nil (page :parent))]
    (struct node key parent (page :children) (page :data))))

(defn- create-page [node]
  (struct f/page 
	  (node :id)
	  (if (= nil (first (node :children)))
	    (byte 8) (byte 0))
	  (if (= nil (node :parent))
	    0 (node :parent))
	  (node :values)
	  (if (= 0 (count (node :children))) nil (node :children))))

(defn- create-node [page]
  (let [parent (if (= 0 (page :parent)) nil (page :parent))]
    (struct node (page :id) parent (page :children) (page :data))))

(defn insert-node [node]
  (let [page (create-page node)
	new-page (f/insert-page (c/get-dbm) page)]
    (create-node new-page)))

(defn 
  #^{:test (fn []
	     (let [n (struct node 3 nil nil ["animal" "bait" "cat"])]
	       (is (= (assoc n :id 1) (set-node 1 n)))))}
  set-node [key node]
  (let [node-2 (if (= (node :parent) nil)
		 (assoc node :parent 0)
		 node)
	page (create-page (assoc node-2 :id key))
	new-page (f/set-page (c/get-dbm) page)]
    (create-node new-page)))

(defn get-root []
  (if (= nil @root)
    (dosync
     (let [r (f/get-root (c/get-dbm))]
       (ref-set root (if (= 0 r) nil r)))))
  @root)

(defn set-root [r]
  (dosync
   (f/set-root (c/get-dbm) r)
   (ref-set root r)))

(defn get-n []
  (if (= nil @n)
    (dosync (ref-set n (f/get-n (c/get-dbm)))))
  @n)

(defn file-model [key]
  (match [key]
    [:get-n] get-n
    [:get-root] get-root
    [:set-root] set-root
    [:insert-node] insert-node
    [:get-node] get-node
    [:set-node] set-node))
