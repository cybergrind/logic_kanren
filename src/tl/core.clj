(ns test
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))


; use lcons from core-logic
(def t1
  (fn [lst out]
    (conde
     [(fresh [hd tl]
        (== (lcons hd tl) lst)
        (== hd out))])))

(run* [q] (t1 [1 2 3] q))

; simple value
(def t2
  (fn [someval out]
    (== someval out)))

(run* [q] (t2 '(1 2) q))


; appendo from core.logic
(defne t3
  [f s o]
  ([() _ s])
  ([[a . d] _ [a . r]] (t3 d s r)))

(run* [q] (t3 [1 2] [3 4] q))

; use pattern matching
(defne t12
  [lst out]
  ([[a . b] b]))

(run* [q] (t12 [1 2 3] q))

; simple statement
(run 1 [q] (fresh (x y z) (== x z) (== 3 y)))

; use letfn 'let loop ()' in pdf
(run 10 [q]
  (letfn
      [(loop []
         (conde
          [(== true q)]
          [(== false q)]
          [(loop)]))]
    (loop)))


; anyo from page 14
(defn anyo [g]
  (conde
   [g]
   [(anyo g)]))

(run 1 [q]
  (anyo (== true q)))

; anyo usage
(run 10 [q]
  (conde
   [(== 2 q)]
   [(anyo (== true q))]
   [(anyo (== false q))]
   [(== 1 q)]))


; alwayso
(def alwayso
  (anyo (== false false)))

(run 5 [q]
  (== true q)
  alwayso
  (== true q))

; appendo v2, pdf page 19

(println "\nappend2 function")
(defn appendo2
  [lst1 lst2 out]
  (conde
   [(== () lst1) (== lst2 out)]
   [(fresh [head tail]
      (== (lcons head tail) lst1)
      (fresh [acc]
        (appendo2 tail lst2 acc)
        (== (lcons head acc) out)))]))

(def r (run* [q] (appendo2 [1 2 3] [4 5] q)))
(println r)

; compute other side, pdf page 19

(println "\nreverse search")
(def r
 (run 5 [q]
  (fresh [list1 list2]
    (appendo2 list1 list2 [1 2 3 4 5])
    (== [list1 list2] q))))
(clojure.pprint/pprint r)


