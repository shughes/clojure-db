(ns db.main 
  (:require [db.btree :as b]
	    [db.file :as f])
  (:import [java.io File]
	   [java.nio ByteBuffer]))

(defn- insert []
  (b/insert "animal")
  (b/insert "cat")
  (b/insert "you")
  (b/insert "dog")
  (b/insert "mouse")
  (b/insert "rat")
  (b/insert "facebook")
  (b/insert "excel")
  (b/insert "entertain")
  )

;(defstruct page :id :flags :parent :data :children)
(defn- main []
  (f/open "/Users/shughes/Desktop/test.db")
  ;(println (f/get-last-id))
  (insert)
  (println (f/get-n))
  (println (b/search "butt"))
  ;(println (f/get-root))
  ;(f/set-root 1)
  ;(println (f/get-root))
  ;(f/set-page (struct f/page 1 (byte 8) 0 ["samuel"]))
  ;(f/set-page (struct f/page 2 (byte 8) 0 ["dog"]))
  ;(f/set-page (struct f/page 3 (byte 8) 0 ["fish"]))
  ;(f/file-to-bytes)
  (f/close))

(main)