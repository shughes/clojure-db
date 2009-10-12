(ns btree.main
  (:use [clojure.contrib.def :only (defvar-)]))

;; Data ;; 
(def test-data (ref ["paris" "hughes" "kick" "call" "cat" 
		     "fishing" "hugs" "jennings" "kisses" "lesbian" 
		     "pencel" "sarah" "party" "patty" "sally" 
		     "samuel" "silverman" "swim"]))

(def test-tree (ref [nil
		     {:id 1, :parent nil, :children [2 6], :values [0]}
		     {:id 2, :parent 1, :children [3 4 5], :values [1 2]}
		     {:id 3, :parent 2, :children nil, :values [3 4 5]}
		     {:id 4, :parent 2, :children nil, :values [6 7]}
		     {:id 5, :parent 2, :children nil, :values [8 9]}
		     {:id 6, :parent 1, :children [7 8 9], :values [10 11]}
		     {:id 7, :parent 6, :children nil, :values [12 13]}
		     {:id 8, :parent 6, :children nil, :values [14 15]}
		     {:id 9, :parent 6, :children nil, :values [16 17]}]))

(defstruct node :parent :children :values)

(defn- get-index [v vs]
  (loop [val v, vals vs, index 0]
    (cond (= (first vals) nil) index
	  (< (compare val (@data (first vals))) 0) index
	  :else (recur val (rest vals) (+ 1 index)))))

(defn- data-compare [i]
  (@data i))

(defn- get-node [key]
  (if (= nil key) nil
      (@tree key)))

(defn- insert-data [value]
  (dosync 
   (ref-set data (conj @data value))
   (- (count @data) 1)))

(defn insert-node [node]
  (dosync 
   (let [id (count @tree)]
     (ref-set tree (conj @tree (assoc node :id id)))
     (get-node id))))
	  

(defn- set-node [key node]
  (dosync 
   (ref-set tree (assoc @tree key node))))

;; End Data ;;

(defvar- n 5)

(defvar- root 1)

(defn- find-insert-node [key value]
  (let [node (get-node key)
	children (node :children)]
    (cond 
      (= nil (first children)) (let [new-node {:id (node :id)
					       :parent (node :parent)
					       :children nil}
				     data-key (insert-data value)
				     new-values (conj (node :values) data-key)]
				 (set-node key (assoc new-node 
						 :values (vec (sort-by data-compare new-values))))
				 (get-node key))
      :else (let [index (get-index value (node :values))]
	      (find-insert-node ((node :children) index) value)))))

(defn- new-children [children old-id id1 id2]
  (cond (= (first children) nil) nil
	(= (first children) old-id) (vec (concat [id1 id2] (new-children (rest children) old-id id1 id2)))
	:else (vec (concat [(first children)] (new-children (rest children) old-id id1 id2)))))

(defn- adjust-tree [node]
  (let [promoted (int (/ n 2))
	parent (if (not= nil (node :parent))
		 (get-node (node :parent))
		 (insert-node {:parent nil
			       :children []
			       :values []}))
	node1 (insert-node {:parent (parent :id)
			    :children (if (= (first (node :children)) nil)
					nil
					(vec ((split-at (inc promoted) (node :children)) 1)))
			    :values (vec ((split-at (inc promoted) (node :values)) 1))})
	node2 (insert-node {:parent (parent :id)
			    :children (if (= (first (node :children)) nil)
					nil
					(vec ((split-at (inc promoted) (node :children)) 0)))
			    :values (vec ((split-at promoted (node :values)) 0))})]
    (if (not= parent nil) 
      (let [parent-children (if (> (count (parent :children)) 0)
			      (new-children (parent :children) (node :id) (node2 :id) (node1 :id))
			      [(node2 :id) (node1 :id)])
	    parent-values (vec (sort-by data-compare
					(conj (parent :values) ((node :values) promoted))))
	    new-parent {:id (parent :id)
			:parent (parent :parent)
			:values parent-values
			:children parent-children}]
	(set-node (parent :id) new-parent)
	(if (= (count parent-values) n)
	  (adjust-tree new-parent))))))
	

(defn insert [value]
  (let [node (find-insert-node 1 value)]
    (if (= n (count (node :values)))
      (adjust-tree node))))

(def data (ref []))
(def tree (ref {}))
(defn run-test []
  (dosync (ref-set data @test-data))
  (dosync (ref-set tree @test-tree))
  (insert "nothing")
  (insert "mom")
  (insert "mad")
  (draw-tree 1 @tree true))

(defn print-list [lst]
  (if (= (first lst) nil) (do (println) nil)
      (do (print (@data (first lst)) " ")
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