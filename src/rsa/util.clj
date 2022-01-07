(ns rsa.util
  (:import java.util.Random java.math.BigInteger))

(defn pow-mod [b exp m result]
  (if (>= exp 1)
    (recur (mod (* b b) m)
           (quot exp 2)
           m
           (if (= (mod exp 2) 1)
             (mod (* result b) m)
             result))
    result))

(defn rand-big-int
  "Returns an uneven integer with the specified bit size."
  [bits]
  (+ (* 2 (BigInteger. (- bits 1) (Random.))) 1))