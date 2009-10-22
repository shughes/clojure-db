(ns db.file
  (:use [clojure.contrib.str-utils]
	[clojure.contrib.def :only (defvar-)]
	[clojure.contrib.math]
	[clojure.contrib.test-is])
  (:import [java.nio.channels FileChannel]
	   [java.nio ByteBuffer]
	   [java.util BitSet]
	   [java.io File FileInputStream FileOutputStream DataOutputStream BufferedInputStream]))

(defvar- *page-size* 1024)

(defvar- *default-n* 5)

(defvar- *header-size* 100)

(defvar- *leaf-flag* (byte 8))

(defvar- *flags-index* 0)

(defvar- *parent-index* 1)

(defvar- *free-byte-index* 5)

(defvar- *last-child-index* 7)

(defvar- *ptrs-index* 15)

(defvar- *header-string* 0)

(defvar- *header-page-size* 16)

(defvar- *header-root-id* 18)

(defvar- *header-last-id* 22)

(defvar- *header-n* 26)

(defvar- -file (ref {})) ;; :out and :in

(defstruct page :id :flags :parent :data :children)

(defn- is-leaf? [b]
  (if (> (bit-and *leaf-flag* b) 0) true false))

(defn- sfirst [val]
  (if (= 0 (count val)) nil
      (subs val 0 1)))

(defn- srest [str-val]
  (if (not= "" str-val)
    (subs str-val 1)
    nil))

(defn- bytes-to-int
  [lst]
  (loop [i 0, rev (reverse lst), result 0]
    (if (= nil (first rev))
      result
      (let [val (if (< (first rev) 0)
		  (+ 256 (first rev))
		  (first rev))]
	(recur (inc i) (rest rev) (+ result (* (expt 256 i) val)))))))

(defn write-bytes [bb pos]
  (.rewind bb)
  (let [ch-out (@-file :out)]
    (loop []
      (if (.hasRemaining bb)
	(do
	  (. ch-out (write bb pos))
	  (recur))))))

(defn- byte-list 
  "Converts a base 10 number into a list of bytes."
  [n]
  (loop [val n, result '()]
    (if (= val 0) result
	(let [rem (mod val 256)
	      q (int (/ val 256))]
	  (recur q (cons (byte rem) result))))))

(defn set-page-size [b]
  (let [ch (@-file :out)]
    (if (or (< b 512) (> b 65535))
      (throw (new Exception "Page size must be between 512 and 65,536"))
      (let [barr (byte-list b)
	    bb (. ByteBuffer (allocate 2))]
	(. bb (put 0 (first barr)))
	(. bb (put 1 (first (rest barr))))
	(.rewind bb)
	(. ch (position *header-page-size*))
	(write-bytes bb *header-page-size*)))))

(defn get-page-size []
  (let [ch (@-file :in)]
    (. ch (position *header-page-size*))
    (let [bb (. ByteBuffer (allocate 2))]
      (. ch (read bb))
      (.rewind bb)
      (bytes-to-int (list (.get bb) (.get bb))))))

(defn set-n [n]
  (let [bb (. ByteBuffer (allocate 1))]
    (. bb (put (byte n)))
    (write-bytes bb *header-n*)))

(defn get-n []
  (let [in (@-file :in)
	bb (. ByteBuffer (allocate 1))]
    (. in (position *header-n*))
    (. in (read bb))
    (.rewind bb)
    (bytes-to-int (list (.get bb)))))
    

(defn- set-header-string [str]
  (let [bb (. ByteBuffer (allocate *header-page-size*))]
    (. bb (put (.getBytes str)))
    (write-bytes bb *header-string*)))

(defn- get-header-string []
  (let [ch (@-file :in)]
    (. ch (position *header-string*))
    (let [bb (. ByteBuffer (allocate 16))]
      (. ch (read bb))
      (.rewind bb)
      (let [result (loop [arr []]
		     (if (.hasRemaining bb)
		       (recur (conj arr (.get bb)))
		       arr))]
	(new String (into-array Byte/TYPE result))))))

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

(defn set-page [page]
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
    (. (@-file :out) (position (- (+ *header-size* (* (page :id) 
					  *page-size*))
				*page-size*)))
    (write-bytes bb (- (+ *header-size* (* (page :id) 
					   *page-size*)) *page-size*))
    page))

(defn- get-free-byte [bb]
  (. bb (position 5))
  (let [bytes (list (.get bb) (.get bb))]
    (bytes-to-int bytes)))

(defn- get-page-index [id]
  (- (+ *header-size* (* id *page-size*)) *page-size*))

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

(defn get-page [id]
  (let [in (@-file :in)
	bb (. ByteBuffer (allocateDirect *page-size*))]
    (. in (position (get-page-index id)))
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

(defn get-last-id []
  (let [in (@-file :in)]
    (. in (position *header-last-id*))
    (let [bb (. ByteBuffer (allocateDirect 4))]
      (. in (read bb))
      (.rewind bb)
      (let [last (bytes-to-int [(.get bb) (.get bb)
				(.get bb) (.get bb)])]
	last))))

(defn- set-last-id [id]
  (let [out (@-file :out)
	bb (. ByteBuffer (allocateDirect 4))]
    (. bb (put (n-byte-array 4 id)))
    (.rewind bb)
    (write-bytes bb *header-last-id*)))

(defn insert-page [page]
  (let [last (get-last-id)]
    (set-page (assoc page :id (inc last)))
    (set-last-id (inc last))
    (assoc page :id (inc last))))

(defn- dead [fi cout]
  (if (= 0 (.length fi))
    (let [bb (. ByteBuffer (allocate *header-size*))]
      (dotimes [i *header-size*]
	(. bb (put (byte 0))))
      (.rewind bb)
      (. cout (position 0))
      (loop []
	(if (.hasRemaining bb)
	  (do
	    (. cout (write bb))
	    (recur)))))))

(defn open [f]
  (let [fi (new File f)
	new (if (not (.exists fi)) true false)
	out (new FileOutputStream f true)
	in (new FileInputStream f)
	cout (.getChannel out)
	cin (.getChannel in)]
    (dosync
     (ref-set -file {:file fi
		     :out cout
		     :in cin})
     (if new
       (do
	 (set-page-size *page-size*)
	 (set-n *default-n*))))))

(defn close []
  (.close (@-file :in))
  (.close (@-file :out)))

(defn file-to-bytes []
  (let [buf (. ByteBuffer (allocateDirect (.length (@-file :file))))
	fc (@-file :in)]
    (. fc (position 0))
    (. fc (read buf))
    (. buf (position 0))
    (.close fc)
    (let [count (loop [result '(), i 0]
		  (if (.hasRemaining buf)
		    (let [b (.get buf)]
		      (print b " ")
		      (recur (concat result (list b)) (inc i)))
		    i))]
      )))

(defn get-root []
  (let [in (@-file :in)
	bb (. ByteBuffer (allocate 4))]
    (. in (position *header-root-id*))
    (. in (read bb))
    (.rewind bb)
    (bytes-to-int (list (.get bb) (.get bb) (.get bb) (.get bb)))))

(defn set-root [id]
  (let [out (@-file :out)
	bb (. ByteBuffer (allocate 4))]
      (. bb (put (n-byte-array 4 id)))
      (write-bytes bb *header-root-id*)))