(ns rsa.rsa
  (:require [rsa.prime-generator :as pg :only [gen-prob-prime]]
            [rsa.util :as util])
  (:import java.math.BigInteger))


(defn generate-pq [bits]
  (let [p (pg/gen-prob-prime bits)]
    [p
     (loop [q (pg/gen-prob-prime bits)]
       (if (not= q p) q
           (recur (pg/gen-prob-prime bits))))
     ]))

(defn get-n-phi [p q] (* (dec p) (dec q)))

(defn generate-e [n-phi]
  (loop [e (util/random-big-int (.bitLength n-phi))]
    (if (= (util/gcd n-phi e) 1) e
        (recur (util/random-big-int (.bitLength n-phi))))))

(defn generate-d [e n-phi]
  (mod (util/extended-gcd n-phi e 0 1) n-phi))

(defn generate-key-pair [bits] 
  (let [[p q] (generate-pq bits) e (generate-e (get-n-phi p q))]
    {:public-key {:n (* p q)
                  :e e}
     :secret-key {:p p
                  :q q
                  :e e}}))

(defn str-to-num [msg]
  (BigInteger. (byte-array (map (comp byte int) msg))))

(defn num-to-str [num]
  (apply str (map char (.toByteArray (BigInteger. (str num))))))

(defn encrypt-string [msg pk]
  (util/pow-mod (str-to-num msg) (:e pk) (:n pk) 1))

(defn decrypt-string [c sk]  
  (let [p (:p sk) q (:q sk) e (:e sk) n (* p q) n-phi (get-n-phi p q)]
    (num-to-str (util/pow-mod c (generate-d e n-phi) n 1))))