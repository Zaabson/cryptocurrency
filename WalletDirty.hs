{-# LANGUAGE LambdaCase #-}
module Wallet where

import BlockValidation (OwnedUTXO)

-- Wallet is supposed to be a light node. It's a part of a network, but only stores blockheaders

-- messageHandler :: _ 
-- messageHandler = serverHandler ()


--data WalletAppState = WalletAppState {
--    peers : PeersSet,
 --   fixed : [BlockHeaders]
--}

-- 
-- Light node mantains (Fixed, Lively) structures but keeping only headers and veryfing only hashes.
-- Wallet mantains a set of utxos of interest - coins (or txs). Methods to insert and delete from set.
-- To operate a wallet we need to associate each block with set of coins from it 
-- and update statuses of them once the block moves to fixed (or on insert if already in fixed).
-- Two options: 
--  - Store coins in seperate Dict BlockId [Coins]
--  - together with blockheader in lively and fixed structures.




newtype Fixed b = Fixed {getFixed :: [b]}
    deriving (Show, Generic)
data Lively b = Lively { root :: BlockReference, forest :: [Tree b]}
    deriving (Show, Generic)



data BlockchainUpdated
      --            new         old
    = BlockInserted FixedBlocks FixedBlocks (Lively BlockHeader)
    | BLockInsertedLinksToRoot (Lively BlockHeader)
    | FutureBlock FutureBlocks
    | BlockAlreadyInserted
    | BlockInvalid
-- | BlockNotLinked

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- If a block can be appended to a block in Lively, 
-- then append it, prune branches and move some of the older blocks from Lively to Fixed
updateWithBlock ::  ForkMaxDiff                   -- constant specifying which forks can be safely discarded - the ones strictly shorter than maxdiff
                 -> TargetHash                    -- constant specifying mining difficulty
                --  -> UTXOPool                      -- pool of utxos from FixedBlocks
                 -> Block                         -- block to be validated and inserted into LivelyBlocks
                 -> Lively BlockHeader                  -- recent chains
                 -> Fixed BlockHeader                   -- older chain
                 -> FutureBlocks                  -- blocks that might link to blocks we haven't received yet
                 -> BlockchainUpdated                       -- blockchain updated with the block
updateWithBlock (ForkMaxDiff maxdiff) target utxoPool newblock lb@(Lively {root ,forest}) fb@(FixedBlocks fixed) (FutureBlocks future) =
    -- Does block link directly to root?
    if blockPreviousHash newblock == root then
        -- Is it already present?
        if any ((== shash256 (blockHeader newblock)) . (\case Tree b _ -> shash256 (blockHeader b)) ) forest then
            BlockAlreadyInserted
        else
            -- Is it valid?
            -- if fst $ validateBlock target utxoPool newblock then
                BLockInsertedLinksToRoot (lb {forest=newTree newblock : forest})
            -- else
                -- BlockInvalid
    else
        -- Find a node in Lively with given hash referenced by newblock
        case break (isJust . snd) $ map (id &&& linkToChain newblock) forest of

            -- New block doesn't append to any known recent block - add it to FutureBlocks.
            -- TODO: Manage FutureBlocks.
            (_, []) -> FutureBlock . FutureBlocks $ Map.alter (maybe (Just $ Set.singleton newblock) (Just . Set.insert newblock)) (blockPreviousHash newblock) future

            -- new block appends to blockchain
            (ts1, (_, Just zipper) : ts2) ->
                if blockAlreadyInserted newblock zipper then
                    BlockAlreadyInserted
                else  
                    -- calculate UTXOPool in a moment in blockchain where the new block appends 
                    -- let utxoPool'           = collectUTXOs utxoPool $ pathFromRoot zipper in
                    -- validate the block
                    -- let (valid, utxoPool'') = validateBlock target utxoPool' newblock   in 

                    if valid then
                        -- We append a new block with all the blocks waiting in the FutureBlocks that append to it.
                        -- We recursively create a tree of blocks from FutureBlocks and put it into Lively in a place given by zipper.
                        let newtree = insertFutures future utxoPool'' newblock in
                        -- we put the tree with inserted blocks back into the list
                        let newforest = map fst ts1 ++ [fromZipper $ insertAsChild newtree zipper] ++ map fst ts2 in
                        -- Said tree is hung (hanged?) in the Lively tree and a resulting tree is pruned and old blocks are moved to FixedBlocks.
                        let (newfixed, lively) = fixBlocks maxdiff $ prune maxdiff newforest
                        in BlockInserted (FixedBlocks (newfixed ++ fixed)) (Lively (maybe root blockRef (safeHead (newfixed ++ fixed))) lively) (collectUTXOs utxoPool (reverse newfixed))
                    else
                        BlockInvalid
            (_, (_, Nothing) : _) -> error "Break on (isjust . snd) - doesn't happen"

    where
        
        -- We take zipper focused on newly added block. Check whether there's block waiting in the futures to be appended here.
        -- If so - append it and append blocks from futures recursively. 
        insertFutures :: Map.Map BlockReference (Set.Set Block)    -- future blocks, keys are blockPreviousHash'es
                      -> UTXOPool                      -- utxoPool up to the root block including
                      -> Block                         -- Block to put in the root 
                      -> Tree Block                    -- Tree made of blocks from FutureBlocks Map rooted in the given block
        insertFutures futures utxoPool block = case Map.lookup (blockRef block) futures of
            Just blocks ->
                
                Tree block $ foldl' (
                    \bs b -> 
                        let (valid, utxoPool') = validateBlock target utxoPool b in
                        if valid then
                            insertFutures futures utxoPool' b : bs
                        else
                            bs
                    ) [] blocks
                
            Nothing -> Tree block []

        pathFromRootA :: Place a -> [a]-> [a]
        pathFromRootA (Root bl) acc               = bl : acc
        pathFromRootA (Brother parent _ bl _) acc = pathFromRootA parent (bl : acc)

        -- returns a list of blocks on a path starting from root and ending on given Zipper
        pathFromRoot :: Zipper a -> [a]
        pathFromRoot (Zipper _ pl) = pathFromRootA pl []


insertAsChild :: Tree a -> Zipper a -> Zipper a
insertAsChild t (Zipper ts b) = Zipper (t : ts) b
