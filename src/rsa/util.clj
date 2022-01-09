(ns rsa.util
  (:import java.util.Random java.math.BigInteger))

(defn pow-mod
  "Calculates `b^exp mod m` using the right-to-left binary method. [Wikipedia](https://en.wikipedia.org/wiki/Modular_exponentiation#Right-to-left_binary_method)"
  ([b exp m] (pow-mod b exp m 1))
  ([b exp m result]
  (if (< exp 1)
    result
    (recur (mod (* b b) m)
           (quot exp 2)
           m
           (if (even? exp) result (mod (* result b) m))))))

(defn pow-mod-2 
  "Same as the above, but without recur."
  [b exp m]
  (first (reduce (fn [[result base] exp]
                   [(if (even? exp)
                      result
                      (mod (* result base) m))
                    (mod (* base base) m)])
                 [1 b]
                 (take-while (comp not zero?)
                             (iterate (fn [exp] (quot exp 2)) exp)))))

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

(defn gcd
  "Finds the greatest common divisor of two numbers a and b."
  [a b]
  (if (= b 0) a
      (recur b (mod a b))))

(defn extended-gcd
  "A reduced version of the extended gcd returning only one of the coefficients of Bézout's identity,
   which is will be the inverse of a number *a in Zn*."
  [old_r r old_s s]
  (if (= r 0) old_s
      (recur
       r
       (- old_r (* (quot old_r r) r))
       s
       (- old_s (* (quot old_r r) s)))))

(defn divide-by-two
  "Helper for get-minimal-trailing-zeros"
  [[a b]]
  (vector (quot a 2) (quot b 2)))

(defn get-minimal-trailing-zeros
  "Gets the smaller number of trailing zeroes of integers a and b."
  [a b]
  (count (take-while (fn [[a b]] (and (= (mod a 2) 0) (= (mod b 2) 0)))
                     (iterate divide-by-two [a b]))))