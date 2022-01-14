# rsa

A simple implementation of an rsa key-generator with encryption and decryption.

## Usage

Make sure to have leiningen installed. Then execute the commands:

    `$lein run generate 'filename' key-size` to create a new keypair with name 'filename'.
    `$lein run encrypt 'keyfile' 'message` to encrypt a message 'message' with the public key from 'keyfile'.
    `$lein run decrypt 'keyfile 'cypher'` to decrypt a cypher 'cypher' with the secret key form 'keyfile'.
    `$lein run help` to display the possible commands.
