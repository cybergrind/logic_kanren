(ns test
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))


(def t1
  (fn [lst out]
    (conde
     [(fresh [hd tl]
        (== (lcons hd tl) lst)
        (== hd out))])))

(run* [q] (t1 [1 2 3] q))


(def t2
  (fn [someval out]
    (== someval out)))

(run* [q] (t2 '(1 2) q))

(defne t3
  [f s o]
  ([() _ s])
  ([[a . d] _ [a . r]] (t3 d s r)))

(run* [q] (t3 [1 2] [3 4] q))


(defne t12
  [lst out]
  ([[a . b] b]))

(run* [q] (t12 [1 2 3] q))


(run 1 [q] (fresh (x y z) (== x z) (== 3 y)))

(run 10 [q]
  (letfn
      [(loop []
         (conde
          [(== true q)]
          [(== false q)]
          [(loop)]))]
    (loop)))



(defn anyo [g]
  (conde
   [g]
   [(anyo g)]))

(run 1 [q]
  (anyo (== true q)))

(run 10 [q]
  (conde
   [(== 2 q)]
   [(anyo (== true q))]
   [(anyo (== false q))]
   [(== 1 q)]))


(def alwayso
  (anyo (== false false)))

(run 5 [q]
  (== true q)
  alwayso
  (== true q))
