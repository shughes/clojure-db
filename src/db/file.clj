(ns db.file
  (:require [db.config :as c])
  (:use [clojure.contrib.str-utils]
	[clojure.contrib.math]
	[clojure.contrib.test-is])
  (:import [java.nio.channels FileChannel]
	   [java.nio ByteBuffer]
	   [java.util BitSet]
	   [java.io File FileInputStream FileOutputStream DataOutputStream BufferedInputStream]))

(def *page-size* 1024)
(def *leaf-flag* (byte 8))
(def *flags-index* 0)
(def *parent-index* 1)
(def *free-byte-index* 5)
(def *last-child-index* 7)
(def *root-index* 11)
(def *ptrs-index* 15)

(defstruct page :id :in :out :flags :parent :data :children)

(defn- is-leaf? [b]
  (if (> (bit-and *leaf-flag* b) 0) true false))

(defn- sfirst [val]
  (if (= 0 (count val)) nil
      (subs val 0 1)))

(defn- srest [str-val]
  (if (not= "" str-val)
    (subs str-val 1)
    nil))

(defn- dec-to-hex [i]
  (let [val (if (< i 0)
	      (+ 256 i)
	      i)]
    (loop [dec val, result ""]
      (let [ret (int (/ dec 16))
	    r1 (mod dec 16)
	    rem (cond (= r1 10) "a"
		      (= r1 11) "b"
		      (= r1 12) "c"
		      (= r1 13) "d"
		      (= r1 14) "e"
		      (= r1 15) "f"
		      :else r1)]
	(if (= 0 ret)
	  (str rem result)
	  (recur ret (str rem result)))))))

(defn- byte-list 
  "Converts a base 10 number into a list of bytes."
  [n]
  (loop [val n, result '()]
    (if (= val 0) result
	(let [rem (mod val 256)
	      q (int (/ val 256))]
	  (recur q (cons (byte rem) result))))))

(defn- rev-str [val]
  (if (= nil (sfirst val))
    nil
    (concat (rev-str (srest val)) (list (sfirst val)))))

(defn- hex-to-dec [h]
  (let [rlst (rev-str h)]
    (loop [i 0
	   r rlst
	   result 0]
      (if (= (first r) nil)
	result
	(let [val (cond (= (first r) "a") 10
			(= (first r) "b") 11
			(= (first r) "c") 12
			(= (first r) "d") 13
			(= (first r) "e") 14
			(= (first r) "f") 15
			:else (first r))]
	  (recur (inc i) (rest r) 
		 (+ (* (new Integer val) (expt 16 i)) result)))))))

(defn- split-hex 
  "Takes hex string and splits it into pairs. So x85a3 becomes (\"85\", \"a3\")."
  [hex]
  (let [even (if (= 0 (mod (count hex) 2))
	       hex
	       (str "0" hex))]
    (loop [h even
	   val ""
	   result '()]
      (cond 
	(= nil h) result
	(= (count val) 2) (recur (srest h) (sfirst h) (concat result (list val)))
	:else (recur (srest h) (str val (sfirst h)) result)))))

(defn set-page-size 
  "Byte at index 16 and 17 give the page size."
  [ch b]
  (if (or (< b 512) (> b 65535))
    (throw (new Exception "Page size must be between 512 and 65,536"))
    (let [barr (byte-list b)
	  bb (. ByteBuffer (allocate 2))]
      (. bb (put 0 (first barr)))
      (. bb (put 1 (first (rest barr))))
      (.rewind bb)
      (. ch (position 16))
      (loop []
	(if (.hasRemaining bb)
	  (do 
	    (. ch (write bb))
	    (recur)))))))

(defn get-page-size [ch]
  (. ch (position 16))
  (let [bb (. ByteBuffer (allocate 2))]
    (. ch (read bb))
    (let [h1 (dec-to-hex (. bb (get 0)))
	  h2 (dec-to-hex (. bb (get 1)))
	  h3 (if (= (count h1) 1) (str "0" h1) h1)
	  h4 (if (= (count h2) 1) (str "0" h2) h2)
	  hex (str h3 h4)]
      (hex-to-dec hex))))

(defn- set-header-string [ch str]
  (. ch (position 0))
  (let [bb (. ByteBuffer (allocate 16))]
    (. bb (put (.getBytes str)))
    (.rewind bb)
    (loop []
      (if (.hasRemaining bb)
	(do
	  (. ch (write bb))
	  (recur))))))

(defn- get-header-string [ch]
  (. ch (position 0))
  (let [bb (. ByteBuffer (allocate 16))]
    (. ch (read bb))
    (.rewind bb)
    (let [result (loop [arr []]
		   (if (.hasRemaining bb)
		     (recur (conj arr (.get bb)))
		     arr))]
      (new String (into-array Byte/TYPE result)))))

(defn- write-bytes [ch-out bb]
  (loop []
    (if (.hasRemaining bb)
      (do
	(. ch-out (write bb))
	(recur)))))

(defn- n-byte-array [n num]
  (let [bytes (if (not= nil num) (byte-list num) (list (byte 0)))
	max (- n (count bytes))
	byte-vec (loop [i 0, arr bytes]
		   (if (= i max)
		     arr
		     (recur (inc i) (concat (list (byte 0)) arr))))]
    (into-array Byte/TYPE byte-vec)))

(defn- fill-string-data [bb page]
  (loop [i 0, pos *page-size*, result []]
    (if (not= i (count (page :data)))
      (let [data ((page :data) i)
	    size (+ (count data) 1
		    (if (is-leaf? (page :flags)) 0 4))
	    child (if (not (is-leaf? (page :flags))) ((page :children) i) nil)]
	(. bb (position (- pos size)))
	(if (not (is-leaf? (page :flags)))
	  (. bb (put (n-byte-array 4 child))))
	(. bb (put (byte (count data))))
	(. bb (put (.getBytes data)))
	(recur (inc i) (- pos size) (conj result (- pos size))))
      result)))

(defn set-page
  [page]
  (println "set-page " (str page))
  (let [bb (. ByteBuffer (allocate *page-size*))]
    (. bb (put (page :flags)))
    (. bb (put (n-byte-array 4 (page :parent))))
    (. bb (position *last-child-index*))
    (. bb (put (if (is-leaf? (page :flags)) 
		 (n-byte-array 4 0)
		 (n-byte-array 4 ((page :children) (- (count (page :children)) 1))))))
    (let [ptrs (fill-string-data bb page)]
      (. bb (position *ptrs-index*))
      (dotimes [i (count ptrs)]
	(. bb (put (n-byte-array 2 (ptrs i)))))
      ;; next available byte goes into byte offset 5.
      (. bb (position 5))
      (. bb (put (n-byte-array 2 (+ *ptrs-index* (* 2 (count ptrs)))))))
    ;; write to file
    (. bb (position 0))
    (. (page :out) (position (- (+ 100 (* (page :id) 
					  *page-size*))
				*page-size*)))
    (write-bytes (page :out) bb)
    page))

(defn- bytes-to-int
  [lst]
  (loop [i 0, rev (reverse lst), result 0]
    (if (= nil (first rev))
      result
      (let [val (if (< (first rev) 0)
		  (+ 256 (first rev))
		  (first rev))]
	(recur (inc i) (rest rev) (+ result (* (expt 256 i) val)))))))

(defn- get-free-byte [bb]
  (. bb (position 5))
  (let [bytes (list (.get bb) (.get bb))]
    (bytes-to-int bytes)))

(defn- get-page-index [id]
  (- (+ 100 (* id *page-size*)) *page-size*))

(defn- get-flag [bb]
  (. bb (position 0))
  (.get bb))

(defn- get-content [bb flags]
  (let [free (get-free-byte bb)
	p (loop [i *ptrs-index*, result '()]
	    (if (= i *ptrs-index*)
	      (. bb (position *ptrs-index*)))
	    (if (= i free)
	      result
	      (let [offset (bytes-to-int [(.get bb) (.get bb)])]
		(recur (+ 2 i) (concat result (list offset))))))]
    (loop [ptrs p, result '()]
      (if (= (first ptrs) nil)
	result
	(do 
	  (. bb (position (first ptrs)))
	  (let [child (if (is-leaf? flags)
			nil
			(bytes-to-int [(.get bb) (.get bb)
				       (.get bb) (.get bb)]))
		size (.get bb)
		lst (loop [i 0, c '()]
		      (if (= i (- size 1))
			(concat c (list (.get bb)))
			(recur (inc i) (concat c (list (.get bb))))))
		content (list (new String (into-array Byte/TYPE lst)) child)]
	    (recur (rest ptrs) (concat result (list content)))))))))

(defn- get-parent-id [bb]
  (. bb (position 1))
  (bytes-to-int (list (.get bb) (.get bb) (.get bb) (.get bb))))

(defn- get-last-child [bb]
  (. bb (position *last-child-index*))
  (bytes-to-int (list (.get bb) (.get bb) (.get bb) (.get bb)))) 

(defn get-page [in id]
  (. in (position (get-page-index id)))
  (let [bb (. ByteBuffer (allocateDirect *page-size*))]
    (. in (read bb))
    (let [content (get-content bb (get-flag bb))
	  parent (get-parent-id bb)
	  flags (get-flag bb)
	  last-child (get-last-child bb)
	  par-ch (loop [lst content, data '(), children '()]
		   (if (= (first lst) nil) 
		     (list data (concat children (list last-child)))
		     (recur (rest lst) (concat data (list (first (first lst))))
			    (concat children (list (first (rest (first lst))))))))]
      {:flags flags, 
       :id id, 
       :parent parent, 
       :data (vec (first par-ch)),
       :children (if (is-leaf? flags) nil (vec (first (rest par-ch))))})))

(defn- get-last-id [in]
  (. in (position 18))
  (let [bb (. ByteBuffer (allocateDirect 4))]
    (. in (read bb))
    (.rewind bb)
    (let [last (bytes-to-int [(.get bb) (.get bb)
			      (.get bb) (.get bb)])]
      last)))

(defn- set-last-id [out id]
  (let [bb (. ByteBuffer (allocateDirect 4))]
    (. bb (put (n-byte-array 4 id)))
    (. out (position 18))
    (.rewind bb)
    (write-bytes out bb)))

(defn insert-page [page]
  (let [last (get-last-id (page :in))]
    (set-page (assoc page :id (inc last)))
    (set-last-id (page :out) (inc last))
    (assoc page :id (inc last))))

(defn open [file]
  (let [in (new FileInputStream file)
	out (new FileOutputStream file)]
    {:in (.getChannel in), :out (.getChannel out)}))

(defn close [ptr]
  (.close (ptr :in))
  (.close (ptr :out)))

(defn file-to-bytes [f]
  (let [buf (. ByteBuffer (allocateDirect (.length f)))
	fin (new FileInputStream f)
	fc (.getChannel fin)]
    (. fc (read buf))
    (. buf (position 0))
    (.close fc)
    (loop [result '()]
      (if (.hasRemaining buf)
	(recur (concat result (list (.get buf))))
	result))))

(defn get-root [in]
  (let [bb (. ByteBuffer (allocate 4))]
    (. in (position *root-index*))
    (. in (read bb))
    (.rewind bb)
    (bytes-to-int (list (.get bb) (.get bb) (.get bb) (.get bb)))))

(defn set-root [out id]
  (. out (position *root-index*))
  (let [bb (. ByteBuffer (allocate 4))]
    (. bb (put (n-byte-array 4 id)))
    (.rewind bb)
    (write-bytes out bb)))