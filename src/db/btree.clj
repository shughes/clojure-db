(ns db.btree
  (:use [clojure.contrib.def :only (defvar-)])
  (:require [db.file :as f]
	    [db.config :as c]))

;; Data ;; 

(defstruct node :id :parent :children :values)

(defn- get-node [key]
  (let [page (f/get-page key)
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
	new-page (f/insert-page page)]
    (create-node new-page)))

(defn- set-node [key node]
  (let [page (create-page (assoc node :id key))
	new-page (f/set-page page)]
    (create-node new-page)))

(defvar- root (ref nil))

(defn- get-root []
  (if (= nil @root)
    (dosync
     (let [r (f/get-root)]
       (ref-set root (if (= 0 r) nil r)))))
  @root)

(defn- set-root [r]
  (dosync
   (f/set-root r)
   (ref-set root r)))

(defvar- n (ref nil))

(defn- get-n []
  (if (= nil @n)
    (dosync (ref-set n (f/get-n))))
  @n)

;; End Data ;;

(defn- get-index [v vs]
  (loop [val v, vals vs, index 0]
    (cond (= (first vals) nil) index
	  (< (compare val (first vals)) 0) index
	  (= (compare val (first vals)) 0) nil
	  :else (recur val (rest vals) (+ 1 index)))))

(defn- find-insert-node [key value]
  (let [node (get-node key)
	children (node :children)
	index (get-index value (node :values))]
    (cond 
      (= nil index) nil
      (= nil (first children)) (let [new-node {:id (node :id)
					       :parent (node :parent)
					       :children nil}
				     new-values (conj (node :values) value)
				     new-node2 (assoc new-node :values (vec (sort new-values)))]
				 (set-node key new-node2)
				 new-node2)
      :else (find-insert-node ((node :children) index) value))))

(defn- new-children [children old-id id1 id2]
  (cond (= (first children) nil) nil
	(= (first children) old-id) 
	(vec (concat [id1 id2] (new-children (rest children) old-id id1 id2)))
	:else (vec (concat [(first children)] (new-children (rest children) old-id id1 id2)))))

(defn- has-children? [node]
  (if (= (first (node :children)) nil)
    false
    true))

(defn- adjust-children-parent [parent children]
  (if (= nil (first children))
    nil
    (let [child (get-node (first children))]
      (set-node (child :id) (assoc child :parent parent))
      (adjust-children-parent parent (rest children)))))

(defn- adjust-tree [node]
  (let [promoted (int (/ (get-n) 2))
	parent (if (not= nil (node :parent))
		 (do (get-node (node :parent)))
		 (insert-node {:parent nil
			       :children []
			       :values []}))
	;; node1 unofficially replaces node.
	node1 {:id (node :id)		
	       :parent (parent :id)
	       :children (if (has-children? node)
			   (vec ((split-at (inc promoted) (node :children)) 1))
			   nil)
	       :values (vec ((split-at (inc promoted) (node :values)) 1))}
	node2 (insert-node {:parent (parent :id)
			    :children (if (has-children? node)
					(vec ((split-at (inc promoted) (node :children)) 0))
					nil)
			    :values (vec ((split-at promoted (node :values)) 0))})]
    ;; make sure node2's children point to node2 as their parent.
    (if (has-children? node2) 
      (adjust-children-parent (node2 :id) (node2 :children)))
    (if (= (parent :parent) nil) (set-root (parent :id)))
    (let [parent-children (if (has-children? parent)
			    (new-children (parent :children) (node :id) (node2 :id) (node1 :id))
			    [(node2 :id) (node1 :id)])
	  parent-values (vec (sort (conj (parent :values) ((node :values) promoted))))
	  new-parent {:id (parent :id)
		      :parent (parent :parent)
		      :values parent-values
		      :children parent-children}]
      ;; node1 officially replaces node
      (set-node (node1 :id) node1)
      (set-node (parent :id) new-parent)
      (if (= (count parent-values) n)
	(adjust-tree new-parent)))))
	
(defn- filled? [node]
  (if (= (get-n) (count (node :values)))
    true
    false))

(defn insert [value]
  (if (= nil (get-root))
    (let [r (insert-node {:parent nil
			  :values [value]
			  :children nil})]
      (set-root (r :id))
      true)
    (let [node (find-insert-node (get-root) value)]
      (if (= nil node) 
	false
	(do (if (filled? node) 
	      (adjust-tree node))
	    true)))))


(defn- search-index [v vs]
  (loop [val v, vals vs, index 0]
    (cond (= (first vals) nil) index
	  (< (compare val (first vals)) 0) index
	  (= (compare val (first vals)) 0) true
	  :else (recur val (rest vals) (+ 1 index)))))

(defn- search-node [key value]
  (let [node (get-node key)
	children (node :children)
	index (search-index value (node :values))]
    (cond 
      (= true index) true
      (= nil (first children)) false
      :else (search-node ((node :children) index) value))))

(defn search [value]
  (search-node (get-root) value))