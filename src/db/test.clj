(ns btree.test
  (:import [java.nio.channels FileChannel]
	   [java.nio ByteBuffer]
	   [java.io File FileInputStream FileOutputStream DataOutputStream BufferedInputStream]))

(def file (new File "/Users/shughes/Desktop/test.db"))

(defn file-io []
  (def f (new FileOutputStream file))
  (def ch (.getChannel f))
  (. ch (position 0))
  (def out (. ByteBuffer (allocate 100)))
  (. out (put (.getBytes "Samuel Hughes")))
  (. out rewind)
  (loop []
    (if (.hasRemaining out)
      (do
	(. ch (write out))
	(recur))))
  (.close ch))


(def f2 (new FileInputStream file))
(def ch2 (.getChannel f2))
(def bb (. ByteBuffer (allocateDirect (.length file))))

(. ch2 (read bb))

(defn header [buf]
  (. buf (position 0))
  (let [arr (make-array (. Byte TYPE) 16)]
    (dotimes [i 16]
      (aset arr i (byte (.get buf))))
    (new String arr)))

(defn page-size [buf]
  (. buf (position 16))
  (let [a (.get buf)
	b (.get buf)]
    b))

(defn get-bytes [buf]
  (. buf (position 0))
  (loop [result {} i 0]
    (if (.hasRemaining buf)
      (recur (assoc result i (byte (.get buf))) (inc i))
      result)))

(defn byte-seq [rdr]
  (let [result (. rdr read)]
    (if (= result -1)
      (do (.close rdr) nil)
      (lazy-seq (cons result (byte-seq rdr))))))

(def bf (new BufferedInputStream (new FileInputStream file)))
(def bytes (vec (byte-seq bf)))
(bytes 51)

(.close ch2)