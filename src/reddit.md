## Problem
So I'm writing an app that of course works on a lot of data. So far all of the data was kept in a running program, after it was initially loaded from files. Before closing the app the data should be saved into the same files again.

This has problems:
 - huge memory usage, unsustainable
 - there can be exceptions on save, better to have the data be saved incrementally and not loose it all.

## Solution?
I want to add some database (is there other solution?). In a nutshell I want a simple library, best if can use existing Aeson JSON encodings or can generate all the typeclasses automatically. Complicating matters - I may need to combine in one atomic block both database and STM transactions.

### Here in more detail:

My app is a cryptocurrency node. I want to store in persistent way:
 - Wallet, collection of owned coins
 - some part of the blockchain, probably a list of blocks older than some threshold.

These cases are different:
 - Wallet is a typical database usage, collection of records. It probably should be encrypted though?
- In a db I want to keep a list of records (simple). It is one part of the whole blockchain state though. I separate between recent LivelyBlocks (we are not certain they will end up in the only surviving branch of the constantly forking blockchain) and old FixedBlocks (I plan to store these in db). FixedBlocks is the said list of records. There is also dictionary of all unspent coins (let's call UTXOPool). The point is that on receiving a new block we need to atomically update all of the values. I'm debating keeping UTXOPool in db as well, but collection of new blocks is small and doesn't need to be stored persistently so probably should be kept in runtime memory (as TVar). This creates a problem of combining atomically database transactions with STM transactions.

Then lastly I'm lazy and don't want to rewrite everything. For now my implementation is centered around using Aeson library to generate JSON representation of all the data structures. Then saving JSON's to file. I would prefer not to have to write any instances for my data types myself but rather have them genericly generated.

### Question
What are all the options? Do I need database? General advice welcomed. 

## PS
It is not the first time I'm wondering about how to tackle this problem and finally move forward. The realization I might need to get rid of some working code is overwhelming xd. I decide to publish the question to finally move forward at least by this bit. Feel free to discuss different aproaches to databases/persistent memory/huge runtime memory and also give partial answers to some of my doubts. Please criticize. 