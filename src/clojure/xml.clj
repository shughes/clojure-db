(add-classpath "file:///usr/local/clojure/extra/clojure-contrib/clojure-contrib.jar")

(ns clojure.xml
  (:use [clojure.contrib.str-utils])
  (:require [clojure.contrib.zip-filter.xml :as contrib-xml]))


(defstruct element-node :tag :attrs :content)

(defstruct result-node :value :xml)

(declare xml-str)

(defn- sfirst [str]
  (if (not= "" str) 
    (subs str 0 1)
    nil))

(defn- srest [str-val]
  (if (not= "" str-val)
    (subs str-val 1)
    nil))

(defn- get-element [str-val]
  (loop [xml-str str-val, element ""]
    (cond (= xml-str "") (struct result-node element xml-str)
	  (= (sfirst xml-str) ">") (struct result-node (str element ">") (srest xml-str))
	  :else (recur (srest xml-str) (str element (sfirst xml-str))))))

(defn- get-text [str-val]
  (loop [xml-str str-val, text ""]
    (cond (= xml-str "") (struct result-node (. text trim) xml-str)
	  (= (sfirst xml-str) "<") (struct result-node (. text trim) xml-str)
	  :else (recur (srest xml-str) (str text (sfirst xml-str))))))

(defn- begin-element? [txt]
  (let [a (str (first txt)), b (str (first (rest txt)))]
    (and (= a "<") (not= b "/"))))

(defn- end-element? [txt]
  (let [a (str (first txt)), b (str (first (rest txt)))]
    (and (= a "<") (= b "/"))))

(defn- text? [txt]
  (not= (str (first txt)) "<"))

(defn- element-val [txt]
  (cond (= txt nil) nil
	(= txt "") ""
	(= (sfirst txt) " ") ""
	(= (sfirst txt) "<") (element-val (srest txt))
	(= (sfirst txt) ">") (element-val (srest txt))
	(= (sfirst txt) "/") (element-val (srest txt))
	:else (str (sfirst txt) (element-val (srest txt)))))

(defn- get-node [xml-str]
  (if (text? xml-str)
    (get-text xml-str)
    (get-element xml-str)))

(defn- next-value []
  (if (= (deref xml-str) "")
    nil
    (let [node (get-node (deref xml-str))
	  value (node :value)]
      (dosync (ref-set xml-str (node :xml)))
      value)))

(defn- in-parse []
  (let [value (next-value)]
    (cond
      (= nil value) nil
      (= "" value) (in-parse)
      (end-element? value) nil
      (begin-element? value) (vec (concat 
				   [{value (in-parse)}]
				   (in-parse)))
      :else (vec (concat [value] (in-parse))))))

(defn- new-node [value children]
  (if (text? value) 
    value
    (struct element-node value nil children)))

; real one
(defn- in-parse []
  (let [value (next-value)]
    (cond
      (= value nil) nil
      (= value "") (in-parse)
      (end-element? value) nil
      (begin-element? value) (vec (concat [(new-node value (in-parse))]
					  (in-parse)))
      :else (vector (new-node value nil) (in-parse)))))

(defn parse [file]
  (def xml-str (ref (slurp file)))
  (in-parse))


; Proof for P(n):
; Pend = nil
; Pbegin = [{n: Pnext}] :: Pnext
; Pn = [{n: nil}] :: Pnext
