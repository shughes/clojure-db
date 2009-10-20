(ns db.test
  (:import [java.nio.channels FileChannel]
	   [java.nio ByteBuffer]
	   [java.io File FileInputStream FileOutputStream DataOutputStream BufferedInputStream]))


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
  (loop [result '()]
    (if (.hasRemaining buf)
      (recur (concat result (list (.get buf))))
      result)))

(def file (new File "/Users/shughes/Desktop/test.db"))

(def f2 (new FileInputStream file))
(def ch2 (.getChannel f2))
(def bb (. ByteBuffer (allocateDirect (.length file))))
(. ch2 (read bb))
(.rewind bb)
(def bytes (vec (get-bytes bb)))
bytes
