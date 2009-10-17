(ns db.btree
  (:use [clojure.contrib.def :only (defvar-)]))

;; Data ;; 

(def tree (ref (with-meta {} {:next-id 0})))

(defstruct node :id :parent :children :values)

(defn- get-node [key]
  (if (= nil key) nil
      (@tree key)))

(defn- insert-node [node]
  (dosync 
   (let [next-id ((meta @tree) :next-id)
	 next-node (assoc node :id next-id)]
     (ref-set tree (with-meta (assoc @tree next-id next-node) {:next-id (inc next-id)}))
     next-node)))

(defn- set-node [key node]
  (dosync 
   (ref-set tree (assoc @tree key node))))

;; End Data ;;

(def n 5)

(def root (ref nil))

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
	(= (first children) old-id) (vec (concat [id1 id2] (new-children (rest children) old-id id1 id2)))
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
  (let [promoted (int (/ n 2))
	parent (if (not= nil (node :parent))
		 (get-node (node :parent))
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
    (if (= (parent :parent) nil) (dosync (ref-set root (parent :id))))
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
  (if (= n (count (node :values)))
    true
    false))

(defn insert [value]
  (if (= nil @root)
    (let [r (insert-node {:parent nil
			  :values [value]
			  :children []})]
      (dosync (ref-set root (r :id)))
      true)
    (let [node (find-insert-node @root value)]
      (if (= nil node) false
	  (do (if (filled? node) 
		(adjust-tree node))
	      true)))))


;; testing ;; 

(defn print-list [lst]
  (if (= (first lst) nil) (do (println) nil)
      (do (print (first lst) " ")
	  (print-list (rest lst)))))

(defn draw-tree [key tree print?]
  (if (= nil key) "finished"
      (let [node (tree key)]
	(if print?
	  (do
	    (print (str "id: " key " parent: " (node :parent) " values: "))
	    (print-list (node :values))))
	(let [children (node :children)
	      new-node {:id (node :id)
			:parent (node :parent)
			:children (vec (rest children))
			:values (node :values)}]
	  (if (= (first children) nil) 
	    (draw-tree (node :parent) tree false)
	    (draw-tree (first children) (assoc tree key new-node) true))))))


(defn run-test []
  (def n 5)
  (dosync (ref-set root nil)
	  (ref-set tree (with-meta {} {:next-id 0})))
  (insert "nothing")
  (insert "doggy")
  (insert "castle")
  (insert "mouse")
  (insert "fancy")
  (insert "feast")
  (insert "house")
  (insert "karaoke")
  (draw-tree @root @tree true))