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