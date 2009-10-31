(ns db.btree
  (:use [clojure.contrib.def :only (defvar-)])
  (:require [db.model :as m]))

(defn- get-index [v vs]
  (loop [val v, vals vs, index 0]
    (cond (= (first vals) nil) index
	  (< (compare val (first vals)) 0) index
	  (= (compare val (first vals)) 0) nil
	  :else (recur val (rest vals) (+ 1 index)))))

(defn- find-insert-node [key value]
  (let [node (m/get-node key)
	children (node :children)
	index (get-index value (node :values))]
    (cond 
      (= nil index) nil
      (= nil (first children)) (let [new-node {:id (node :id)
					       :parent (node :parent)
					       :children nil}
				     new-values (conj (node :values) value)
				     new-node2 (assoc new-node :values (vec (sort new-values)))]
				 (m/set-node key new-node2)
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
    (let [child (m/get-node (first children))]
      (m/set-node (child :id) (assoc child :parent parent))
      (adjust-children-parent parent (rest children)))))

(defn- adjust-tree [node]
  (let [promoted (int (/ (m/get-n) 2))
	parent (if (not= nil (node :parent))
		 (do (m/get-node (node :parent)))
		 (m/insert-node {:parent nil
				 :children []
				 :values []}))
	;; node1 unofficially replaces node.
	node1 {:id (node :id)		
	       :parent (parent :id)
	       :children (if (has-children? node)
			   (vec ((split-at (inc promoted) (node :children)) 1))
			   nil)
	       :values (vec ((split-at (inc promoted) (node :values)) 1))}
	node2 (m/insert-node {:parent (parent :id)
			      :children (if (has-children? node)
					  (vec ((split-at (inc promoted) (node :children)) 0))
					  nil)
			      :values (vec ((split-at promoted (node :values)) 0))})]
    ;; make sure node2's children point to node2 as their parent.
    (if (has-children? node2) 
      (adjust-children-parent (node2 :id) (node2 :children)))
    (if (= (parent :parent) nil) (m/set-root (parent :id)))
    (let [parent-children (if (has-children? parent)
			    (new-children (parent :children) (node :id) (node2 :id) (node1 :id))
			    [(node2 :id) (node1 :id)])
	  parent-values (vec (sort (conj (parent :values) ((node :values) promoted))))
	  new-parent {:id (parent :id)
		      :parent (parent :parent)
		      :values parent-values
		      :children parent-children}]
      ;; node1 officially replaces node
      (m/set-node (node1 :id) node1)
      (m/set-node (parent :id) new-parent)
      (if (= (count parent-values) (m/get-n))
	(adjust-tree new-parent)))))
	
(defn- filled? [node]
  (if (= (m/get-n) (count (node :values)))
    true
    false))

(defn insert [value]
  (if (= nil (m/get-root))
    (let [r (m/insert-node {:parent nil
			    :values [value]
			    :children nil})]
      (m/set-root (r :id))
      true)
    (let [node (find-insert-node (m/get-root) value)]
      (if (= nil node) 
	false
	(do 
	  (if (filled? node) 
	    (adjust-tree node))
	  true)))))

(defn- search-index [v vs]
  (loop [val v, vals vs, index 0]
    (cond (= (first vals) nil) index
	  (< (compare val (first vals)) 0) index
	  (= (compare val (first vals)) 0) true
	  :else (recur val (rest vals) (+ 1 index)))))

(defn- search-node [key value]
  (let [node (m/get-node key)
	children (node :children)
	index (search-index value (node :values))]
    (cond 
      (= index true) key
      (= nil (first children)) nil
      :else (search-node ((node :children) index) value))))

(defn search [value]
  (search-node (m/get-root) value))
