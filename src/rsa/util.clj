(ns rsa.util
  (:import java.util.Random java.math.BigInteger))

(defn pow-mod
  "Calculates `b^exp mod m` using the right-to-left binary method. [Wikipedia](https://en.wikipedia.org/wiki/Modular_exponentiation#Right-to-left_binary_method)"
  [b exp m result]
  (if (>= exp 1)
    (recur (mod (* b b) m)
           (quot exp 2)
           m
           (if (= (mod exp 2) 1)
             (mod (* result b) m)
             result))
    result))

(defn random-big-int
  "Creates a random number in the range of `1 to 2^(n - 1) - 1`."
  [bits]
  (inc (BigInteger. (dec bits) (Random.))))

(defn get-prime-candidate
  "Creates a random uneven number which is exactly **bits** bit long.
   
   The Formula is: *n + m*, 
   where *n* is random, odd and in the range of `1 to 2^(n - 1) - 1`, 
   and *m* is `2^(n - 1)`"
  [bits]
  (+ (+ (* 2 (BigInteger. (- bits 2) (Random.))) 1)
     (BigInteger. 1 (byte-array
                     (concat
                      [(pow-mod 2 (dec bits) 255 1)]
                      (apply vector (repeat (quot (dec bits) 8) 0)))))))

(defn gcd [a b]
  (if (= b 0) a
      (recur b (mod a b))))

(defn extended-gcd [old_r r old_s s]
  (if (= r 0) old_s
      (recur r (- old_r (* (quot old_r r) r)) s (- old_s (* (quot old_r r) s)))))

(defn divide-by-two [[a b]]
  (vector (quot a 2) (quot b 2)))

(defn get-minimal-trailing-zeros [a b]
  (count (take-while (fn [[a b]] (and (= (mod a 2) 0) (= (mod b 2) 0)))
                     (iterate divide-by-two [a b]))))