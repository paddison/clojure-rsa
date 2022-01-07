(ns rsa.prime-generator
  (:require [rsa.util :as util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sieve of Eratosthenes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn soe-filter-primes [primes unfiltered]
  (if (zero? (count unfiltered)) primes
      (let [current (first unfiltered)]
        (recur (conj primes current)
               (filter (fn [x] (not (= (mod x current) 0))) unfiltered)))))

(defn soe-primes-below-n [n]
  (soe-filter-primes [2] (range 3 n 2)))

(defn soe-test-num
  "Test the number against all the primes in the sieve"
  [n sieve]
  (nil? (some (fn [x] (and (= (mod n x) 0) (not= n x))) sieve)))

(def sieve 
  "A Sieve of Eratosthenes containing the primes below 10000"
  (soe-primes-below-n 10000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miller-Rabin Primality Test ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mr-create-components [n]
  (loop [d (- n 1) s 0]
    (if (= (mod d 2) 1) [d s]
        (recur (quot d 2) (inc s)))))

(defn mr-primality-test
  "Performs the rabin-miller primality test"
  [n]
  (let [[d s] (mr-create-components n)
               a (util/random-big-int (.bitLength n))]
    #break(if (= 1 (util/pow-mod a d n 1)) true
        (loop [r 0]
          (cond
            (>= r s) false
            (= (util/pow-mod a (* (int (Math/pow 2 r)) d) n 1) (- n 1)) true
            :else (recur (inc r)))))))

;;;;;;;;;;;;;;;;;;;;;
;; Generate Primes ;;
;;;;;;;;;;;;;;;;;;;;;

(defn prime? [n repeats sieve]
  (when (soe-test-num n sieve)
    (every? identity (repeatedly repeats #(mr-primality-test n)))))

(defn gen-prob-prime-helper
  "Generates a n bit long number that is probably prime (1 - 2^-23)"
  [bits sieve]
  (loop [n (util/get-prime-candidate bits)]
    (if (prime? n 23 sieve) n
        (recur (+ n 2)))))

(defn gen-prob-prime [bits] 
  (when (> bits 1)(gen-prob-prime-helper bits sieve)))

(gen-prob-prime 3)
(.toString 2 (util/rand-big-int 2))