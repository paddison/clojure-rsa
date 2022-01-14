(ns rsa.core
  (:gen-class)
  (:require [rsa.rsa :as rsa]))

(defn verify 
  "Verfies if user input is valid bitsize."
  [bits]
  (when (not (and (= (mod (int bits) 128) 0) (>= bits 128)))
    (throw 
     (ex-info "Invalid bit size, must be multiple of 128 and at least 128." {:bits bits})))
  )

(defn write-keys-file 
  "Writes the generated public and secret keys to a file"
  [[key-name bits]]
  (try
    (let [bits (Integer/parseInt bits)]
      (verify bits)
      (println "Generating key pair...")
      (spit (str key-name ".ckey") (rsa/generate-key-pair bits))
      (println "Done, name of the file is:" (str key-name ".ckey")))
    (catch Exception _ (println "Unable to parse value for key length. It has to be numeric, at least 128 and a multiple of 128.")))
  )

(defn read-keys-file 
  "Reads in the specified file."
  [key-file]
  (read-string (slurp key-file)))

(defn encrypt-message 
  "Encrypts a message with the specified keys."
  [[key-file msg]]
  (let [pk (:public-key (read-keys-file key-file))]
    (println 
     (re-find #"\d+" (str (rsa/encrypt-string msg pk)))))) ; trim of the trailing N

(defn decrypt-cipher 
  "Decrypts a cipher with the specified keys."
  [[key-file c]]
  (let [sk (:secret-key (read-keys-file key-file))]
    (println (rsa/decrypt-string (BigInteger. c) sk))))

(defn -main
  "Main function."
  [& args]
  (case (first args)
    "generate" (-> (rest args)
                   (write-keys-file))
    "encrypt" (-> (rest args)
                  (encrypt-message))
    "decrypt" (-> (rest args)
                  (decrypt-cipher))
    "help" (println "Usage:\ngenerate filename bitsize\nencrypt keyfile message\ndecrypt keyfile cipher")
    (println "invalid commands, use help to display information.")))