(ns rsa.core
  (:gen-class)
  (:require [rsa.rsa :as rsa]))

(def dir "/Users/patrickbaumann/Documents/hdm/funcProg/project/rsa/my-key.ckey")

(defn verify [bits]
  (and (= (mod (int bits) 128) 0) (>= bits 128)))

(defn write-keys-file [[key-name bits]]
  (let [bits (Integer/parseInt bits)]
    (if (verify bits)
      (spit (str key-name ".ckey") (rsa/generate-key-pair bits))
      (println "Invalid bit length, must be multiple of 128 and at least 128."))))

(defn read-keys-file [key-file]
  (read-string (slurp key-file)))

(defn encrypt-message [[key-file msg]]
  (let [pk (:public-key (read-keys-file key-file))]
    (println (rsa/encrypt-string msg pk))))

(defn decrypt-cipher [[key-file c]]
  (let [sk (:secret-key (read-keys-file key-file))]
    (println (rsa/decrypt-string (BigInteger. c) sk))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (cond
    (= (first args) "generate") (-> (rest args)
                                    (write-keys-file))
    (= (first args) "encrypt") (-> (rest args)
                                   (encrypt-message))
    (= (first args) "decrypt") (-> (rest args)
                                   (decrypt-cipher))
    (= (first args) "help") (println 
                             "Usage:\ngenerate filename bitsize\nencrypt keyfile message\ndecrypt keyfile cipher")
    :else (println "invalid commands, use help to display information.")))
