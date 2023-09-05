### blockchain and cryptos

# Example of a block
block <- list(number = 3,
              timestamp = "2023-10-01 17:24:18 CEST",
              data = "Rennes",
              parent = 2)



# some blocks
block1 <- list(number = 1,
               timestamp = "2023-10-01 17:24:00 CEST",
               data = "Rennes",
               parent = NA)

block2 <- list(number = 2,
               timestamp = "2023-10-01 17:24:15 CEST",
               data = "Paris",
               parent = 1)

block3 <- list(number = 3,
               timestamp = "2023-10-01 17:24:30 CEST",
               data = "Nice",
               parent = 2)


# the blockchain
blockchain = list(block1, block2, block3)

# get the 2nd block
blockchain[[2]]

## Validate the block
## For now, we only check that the parent field of 
## a non-genesis block references to the previous block in the chain.

validate = function(blockchain) {                   # declare a function
  if (length(blockchain) >= 2) {                    # check if there are more then 2 blocks
    for (i in 2:length(blockchain)) {               #take each block in a loop
      if (blockchain[[i]]$parent != blockchain[[i-1]]$number) { # compare the field Parent (n) with Parent (n-1)
        return(FALSE)                               # return FALSE if they are not equal 
      }
    }
  }
  return(TRUE)                                      # otherwise it is ok
}

validate(blockchain)


###Hash
## The hash algorithm used here (SHA-256) is part of SHA-2 (Secure Hash Algorithm 2), 
## a set of cryptographic hash functions designed by the United States National Security Agency (NSA). 
## it uses digests of 256 bits (or 32 hexadecimal figures). 
## It is implemented in R package digest.

# creates hash digests of arbitrary R objects
library(digest)

# hash a string
digest("My first phrase to encrypts", "sha256")

# hash blocks
block1 <- list(number = 1,
               timestamp = "2020-10-01 17:24:00 CEST",
               data = "Rennes",
               parent_hash = "0") # we create a genesis block (the first in the chain)

block1$hash = digest(block1, "sha256") # in block1 we create a field hash that is a hash of all block

block2 <- list(number = 2,
               timestamp = "2020-10-01 17:24:15 CEST",
               data = "Paris",
               parent_hash = block1$hash) # parent block is block1

block2$hash = digest(block2, "sha256")

block3 <- list(number = 3,
               timestamp = "2020-10-01 17:24:30 CEST",
               data = "Nice",
               parent_hash = block2$hash)

block3$hash = digest(block3, "sha256")

# the blockchain
blockchain = list(block1, block2, block3)


## update the validation function and give a simple example:
validate = function(blockchain) {
  for (i in 1:length(blockchain)) { #in a loop take each of teh blocks
    block = blockchain[[i]]         # assign each block to a variable block
    hash = block$hash               # reassign the initial hash of the block to variable hash
    block$hash = NULL               #  delete field hash to have an initial block
    hash_expected = digest(block, "sha256") # apply a hash function to the initial block
    if (hash != hash_expected) {    # compare the received has with the initial 
      return(FALSE)                 # return False if they are not the same, meaning the block was manipulated
    }
  }
  if (length(blockchain) >= 2) {    # otherwise check the hash with the parent block's hash
    for (i in 2:length(blockchain)) {
      if (blockchain[[i]]$parent_hash != blockchain[[i-1]]$hash) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

validate(blockchain)



# alter data of first block
blockchain[[1]]$data = "Budapest" # change the 
validate(blockchain)

# restore data
blockchain[[1]]$data = "Rennes"
validate(blockchain)


# alter data and hash of first block
blockchain[[1]]$data = "Budapest"
blockchain[[1]]$hash = NULL
blockchain[[1]]$hash = digest(blockchain[[1]], "sha256")

validate(blockchain)


## Proof-of-Work
## Hash alone is not enough to prevent tampering, since hash values can be computed fast by 
## computers. A PoW algorithm controls the difficulty of creating a new block.

## In the case of BitCoin the PoW problem involves the problem of finding a number (called nonce)
## that once added to the block is such that the corresponding block hash contains a certain amount of leading zeros called difficulty (more specifically, Hashcash).
## The average work that a miner needs to perform in order to find a valid nonce is exponential in the difficulty, 
## while one can verify the validity of the block by executing a single hash function.

proof_of_work = function(block, difficulty) {         # declare a function proof_of_work with 2 inputs
  block$nonce <- 0                                    # create a nonce field with 0
  hash = digest(block, "sha256")                      # create a hash of the block
  zero <- paste(rep("0", difficulty), collapse="")    # creates zeros with the number that corresponsd to difficulty
  while(substr(hash, 1, difficulty) != zero) {        # extract a part of the hash as long as it doesn't equal to zero
    block$nonce = block$nonce + 1
    hash = digest(block, "sha256")  
  }
  return(list(hash = hash, nonce = block$nonce)) # return the results as a list with two fields: hash and nonce
}

block <- list(number = 1,
              timestamp = "2018-10-01 17:24:00 CEST",
              data = "Rennes",
              hash = "88e96d4537bea4d9c05d12549907b32561d3bf31f45aae734cdc119f13406cb6Parent",
              parent_hash = "d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3")


proof_of_work(block, 4)


n = 4
iterations = vector("integer", n)
for (i in 1:n) {
  iterations[i] = proof_of_work(block, i)$nonce
}


iterations
plot(1:n, log2(iterations), type = "b", xlab = "difficulty", ylab = "log(iterations)")




## Build a blockchain using the PoW method:
mine <- function(previous_block, difficulty = 3, genesis = FALSE){
  
  if (genesis) {                                                             # if true - generate a genesis block
    # define genesis block
    new_block <-  list(number = 1,
                       timestamp = Sys.time(),
                       data = "I'm genesis block",
                       parent_hash = "0")  
  } else {
    # create new block
    new_block <- list(number = previous_block$number + 1,
                      timestamp = Sys.time(),                                   # get the current system time
                      data = paste0("I'm block ", previous_block$number + 1),   # generate content of the block
                      parent_hash = previous_block$hash)                        # get the parent block hash
  }
  # add nonce with PoW
  new_block$nonce <- proof_of_work(new_block, difficulty)$nonce 
  # add hash 
  new_block$hash <- digest(new_block, "sha256")
  return(new_block)
}

blockchained = function(difficulty = 3, nblocks = 3) {
  # mine genesis block
  block_genesis = mine(NULL, difficulty, TRUE)   
  # first block is the genesis block
  blockchain <- list(block_genesis)
  
  if (nblocks >= 2) {
    # add new blocks to the chain
    for (i in 2:nblocks){
      blockchain[[i]] <- mine(blockchain[[i-1]], difficulty) 
    }
    
  }
  
  return(blockchain)
}

blockchained(nblocks = 3)




## Transactions
## Typically, the data field of a block contains a certain number of transactions. 
## Each transaction has a sender, a receiver and a value of crypto currency that is transferred from sender to receiver. 
## Moreover, it contains a fee of the transaction. 
## Transactions are stored in a pool of pending transactions and each mined block will include a (proper) subset of pending transactions. 
## The miner of the block gets the fees of all blocked transactions plus a fixed mining reward (and this transaction is included in the following block).
# This is how new coins are introduced in the blockchain economy.

## Let's create a pool of fictitious pending transactions. We store them in a data frame structure using the tibble package.

library(tibble) #provides stricter checking and better formatting than the traditional data frame
ntrx = 10
sender = sample(LETTERS, ntrx) # generate some sample with 10 letters
receiver = sample(LETTERS, ntrx)
value = round(runif(n = ntrx, min = 0, max = 100), 0) # generate random integern numbers from 0 to 100
fee = round(runif(n = ntrx, min = 0, max = 1), 2) # generate some random fees. rounded to 2 dec.
(transactions = tibble(sender, receiver, value, fee)) # generate a transaction table; () are used to display the data.frame


## Let's update the mining function. 
## Each block will include the most profitable transactions (the ones with the highest fees) among the pending ones. 
## The miner of the block gets as reward the sum of the fees of transactions in the block. 
## The reward transaction is inserted in the next block. We take advantage of dplyr package.

library(dplyr)
library (digest)
mine <- function(previous_block, transactions, difficulty = 3, block_size = 3, miner = "Z", genesis = FALSE){
  # filter transactions to add
  trans_pending = arrange(transactions, -fee) # arrange the transaction by fee
  if (nrow(trans_pending) < block_size) {     # check the size of the block with the number of transactions
    trans_to_add = trans_pending              # add all the transactions if their amount is less than a ize of the block
    trans_pending = tibble()                  # 
  } else {
    trans_to_add =  filter(trans_pending, row_number() <= block_size) # otherwise select the ones with the highest fee
    trans_pending = filter(trans_pending, row_number() > block_size)  # and alter the pending transactions
  }
  
  if (genesis) {
    # define genesis block
    new_block <-  list(number = 1,
                       timestamp = Sys.time(),
                       data = trans_to_add,
                       parent_hash = "0")  
  } else {
    # create new block
    new_block <- list(number = previous_block$number + 1,
                      timestamp = Sys.time(),
                      data = trans_to_add,
                      parent_hash = previous_block$hash)
  }
  
  # add nonce with PoW
  new_block$nonce <- proof_of_work(new_block, difficulty)$nonce
  # add hash 
  new_block$hash <- digest(new_block, "sha256")
  # add reward transaction
  trans_pending = rbind(trans_pending, data.frame(sender = NA, receiver = miner, value = sum(new_block$data$fee), fee = 0.01))
  return(list(block = new_block, transactions = trans_pending))
}


blockchained = function(transactions, difficulty = 3, block_size = 3, nblocks = 3) {
  # define genesis block
  mined = mine(NULL, transactions, difficulty, block_size, miner = "Z", genesis = TRUE)
  block_genesis <- mined$block
  pending = mined$transactions
  # first block is the genesis block
  blockchain <- list(block_genesis)
  
  if (nblocks >= 2) {
    # add blocks to the chain
    for (i in 2:nblocks){
      mined <- mine(blockchain[[i-1]], pending, difficulty, block_size, miner = "Z")
      blockchain[[i]] <- mined$block
      pending = mined$transactions
    }
  }
  
  return(blockchain)
}

blockchained(transactions, nblocks = 3, block_size = 5)



## Digital signature
## Blockchain uses asymmetric cryptography to implement digital signatures of transactions. 
## Each transaction is signed by the sender with her private key and anyone can verify the authenticity of the signature 
## (and of the transaction) using the sender's public key.

## Public-key cryptography, or asymmetric cryptography, is any cryptographic system that uses pairs of keys: 
## public keys which may be disseminated widely, and private keys which are known only to the owner. 
## This accomplishes two functions: authentication, where the public key verifies that a holder of the paired private key sent
## the message, and encryption, where only the paired private key holder can decrypt the message encrypted with the public key. 
## The strength of a public key cryptography system relies on the computational effort required to find the private key from its paired public key.

## In a public key encryption system, any person can encrypt a message using the receiver's public key. 
## That encrypted message can only be decrypted with the receiver's private key. 
## In a public key signature system, a person can combine a message with a private key to create a short digital signature 
## on the message. Anyone with the corresponding public key can combine the signed message and the known public key to verify whether 
## the signature on the message was valid, i.e. made by the owner of the corresponding private key. 
## Changing the message, even replacing a single letter, will cause verification to fail/

## To speed up the process of transmission, instead of applying the sender's digital signature to the (possibly large) message, 
## the sender can rather hash the message using a cryptographic hash function and then digitally sign the generated hash value. 
## The sender would then sign the newly generated hash value with their private key and encrypt the original message with the receiver's 
## public key. The transmission would then take place securely and with confidentiality and non-repudiation still intact. 
## The receiver would then verify the signature with sender's public key and decrypt the message with their private key.

## RSA (Rivest-Shamir-Adleman) is one of the first public-key cryptosystems and is widely used for secure data transmission. 
## In RSA, the asymmetry is based on the practical difficulty of the factorization of the product of two large prime numbers.

## Next is an example of encryption and digital signature using package openssl:

library(openssl)

# encryption
# generate a private key (key) and a public key (pubkey)
key <- rsa_keygen(512)
pubkey <- key$pubkey
# message
msg <- charToRaw("My first encrypted message!") #  convert string to raw bytes
# cipher the message with public key
ciphermsg <- rsa_encrypt(msg, pubkey)
# decrypt the message with private key
rawToChar(rsa_decrypt(ciphermsg, key))


# signature
# generate a private key (key) and a public key (pubkey)
key <- rsa_keygen()
pubkey <- key$pubkey
# build a transaction
trans = list(sender = "A", receiver = "B", amount = "100")
# serialize data
data <- serialize(trans, NULL) # converst to raw vector (a "raw" sequence of bytes)
# sign (a hash of) the transaction with private key
sig <- signature_create(data, sha256, key = key)
# verify the message with public key
signature_verify(data, sig, sha256, pubkey = pubkey)


# signature and encryption
# generate a private key (key) and a public key (pubkey)
key <- rsa_keygen()
pubkey <- key$pubkey
# build a transaction
trans = list(sender = "A", receiver = "B", value = "100")
# serialize data
data <- serialize(trans, NULL)
# sign (a hash of) the transaction with private key
sig <- signature_create(data, sha256, key = key)
# cipher the transaction with public key
ciphermsg <- rsa_encrypt(data, pubkey)
# verify the message with public key
signature_verify(data, sig, sha256, pubkey = pubkey)


# decrypt and unserialize the transation with private key
unserialize(rsa_decrypt(ciphermsg, key))

## Finally, the blockchain ledger is distributed over a peer-to-peer network. The steps to run the network are as follows:
## new transactions are broadcast to all nodes;
## each node collects new transactions into a block;
## each node works on finding a difficult proof-of-work for its block;
## when a node finds a proof-of-work, it broadcasts the block to all nodes;
## nodes accept the block only if all transactions in it are valid and not already spent;
## nodes express their acceptance of the block by working on creating the next block in the chain, using the hash of the accepted block as the previous hash.

## Nodes always consider the longest chain to be the correct one and will keep working on extending it. 
## The incentive of rewards may help encourage nodes to stay honest. 
## If a greedy attacker is able to assemble more CPU power than all the honest nodes, he would have to choose between using it to 
## defraud people by stealing back his payments, or using it to generate new coins. 
## He ought to find it more profitable to play by the rules (generate new coins), such rules that favour him with more new coins 
## than everyone else combined, than to undermine the system and the validity of his own wealth.


