(ns rsa.prime-generator
  (:require [rsa.util :as util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sieve of Eratosthenes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn soe-filter-primes
  "Filters out non-primes by gradualy adding numbers to `primes` and 
   using those numbers filter `unfiltered`"
  [primes unfiltered]
  (if (zero? (count unfiltered)) primes
      (let [current (first unfiltered)]
        (recur (conj primes current)
               (filter (fn [x] (not (= (mod x current) 0))) unfiltered)))))

(defn soe-primes-below-n
  "Creates a Sieve of Eratosthenes containing all *primes < n*"
  [n]
  (soe-filter-primes [2] (range 3 n 2)))

(defn soe-test-num
  "Test the number against all the primes in the sieve."
  [n sieve]
  (nil? (some (fn [x] (and (= (mod n x) 0) (not= n x))) sieve)))

(def SIEVE
  "A Sieve of Eratosthenes containing the primes below 10000."
  (soe-primes-below-n 10000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miller-Rabin Primality Test ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mr-create-components
  "Helper to create the components `d` and `s` needed for the Miller-Rabin test."
  [n]
  (loop [d (- n 1) s 0]
    (if (= (mod d 2) 1) [d s]
        (recur (quot d 2) (inc s)))))

(defn mr-primality-test
  "Performs the Miller-Rabin primality test. [Wikipedia](https://en.wikipedia.org/wiki/Miller–Rabin_primality_test)"
  [n]
  (let [[d s] (mr-create-components n)
        a (util/random-big-int (.bitLength n))]
    (if (= 1 (util/pow-mod a d n 1)) true
        (not (every? false? (map
                             (fn [r]
                               (=
                                (util/pow-mod a (* (int (Math/pow 2 r)) d) n 1)
                                (dec n)))
                             (range 0 s)))))))
        ;; (loop [r 0]
        ;;   (cond
        ;;     (>= r s) false
        ;;     (= (util/pow-mod a (* (int (Math/pow 2 r)) d) n 1) (- n 1)) true
        ;;     :else (recur (inc r))))


;;;;;;;;;;;;;;;;;;;;;
;; Generate Primes ;;
;;;;;;;;;;;;;;;;;;;;;

(defn prime?
  "Returns true if n the number passed the first 10000 primes and `repeats` 
   repitions of the the Miller-Rabin test."
  [n repeats sieve]
  (if (= n 2) true
      (when (soe-test-num n sieve)
        (every? identity (repeatedly repeats #(mr-primality-test n))))))

(defn gen-prob-prime
  "Generates a **n** bit long number that is prime with 
   a high probabilty."
  ([bits]
   (first (filter #(prime? % 23 SIEVE) (repeatedly #(util/get-prime-candidate bits))))))