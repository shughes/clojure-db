(ns clojure.prime
  (:import [java.lang Math])
  (:use [clojure.contrib.math]))

(defn expt? 
  "this is docs for the expt? that i am testing"
  [n a b]
  (let [ex (expt a b)]
    (cond
      (= a 1) false
      (= n ex) true
      (> ex n) false
      (> b n) false
      :else (expt? n a (inc b)))))

(defn factor? [n]
  (loop [a 1, b 2]
    (let [result (expt? n a b)]
      (cond
	(> a n) false
	(> (expt a 2) n) false
	(= result true) true
	(= result false) (recur (inc a) b)))))

