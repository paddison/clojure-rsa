(ns rsa.rsa
  (:require [rsa.prime-generator :as pg]
            [rsa.util :as util])
  (:import java.math.BigInteger))


(defn generate-pq 
  "Generates two large primes `p` and `q`, which will be exactly `bits` bit long."
  [bits]
  (let [p (pg/gen-prob-prime bits)]
    [p
     (loop [q (pg/gen-prob-prime bits)]
       (if (not= q p) q
           (recur (pg/gen-prob-prime bits))))
     ]))

(defn get-n-phi
  "Returns n-phi."
  [p q] (* (dec p) (dec q)))

(defn generate-e 
  "Generates e, which is part of the public key."
  [n-phi]
  (loop [e (util/random-big-int (.bitLength n-phi))]
    (if (= (util/gcd n-phi e) 1) e
        (recur (util/random-big-int (.bitLength n-phi))))))

(defn generate-d 
  "Generates d, the inverse of e in Zn-phi, which is used for decryption."
  [e n-phi]
  (mod (util/extended-gcd n-phi e 0 1) n-phi))

(defn generate-key-pair 
  "Generates a public key and the respective secret key."
  [bits] 
  (let [[p q] (generate-pq bits) e (generate-e (get-n-phi p q))]
    {:public-key {:n (* p q) :e e}
     :secret-key {:p p :q q :e e}}))

(defn str-to-num 
  "Converts a string to its numerical values. Has to be ASCII-Chars."
  [msg]
  (BigInteger. (byte-array (map (comp byte int) msg))))

(defn num-to-str 
  "Converts a number to an ASCII String."
  [num]
  (apply str (map char (.toByteArray (BigInteger. (str num))))))

(defn encrypt-string 
  "Encrypts a string with the provided public key."
  [msg {e :e n :n}]
  (util/pow-mod (str-to-num msg) e n 1))

(defn decrypt-string 
  "Decrypts a string with the provided secret key."
  [c {p :p q :q e :e}]  
  (let [n (* p q) n-phi (get-n-phi p q)]
    (num-to-str (util/pow-mod c (generate-d e n-phi) n 1))))

;; (str-to-num "你好")