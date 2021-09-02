module Demo where
import App
import BlockType (Genesis(Genesis), Transaction (Transaction), PublicAddress, Cent(..))
import Data.Maybe (fromJust)
import BlockCreation (Keys(Keys))
import Hashing (shash256)
import Control.Concurrent.Async (wait, Async)

config = Config {
    blockchainFilepath = "data/fixed_blocks.json",
    peersFilepath = "data/peers.json",
    targetDifficulty   = 3,
    loggingMode = ToStdin,
    port        = "49155",
    blockchainGenesis = Genesis "Złoty",
    minerWaitForTxs = False
    }

randomPublicAddress :: IO PublicAddress
randomPublicAddress = do
    (Keys pub priv) <- generateKeys
    return $ shash256 pub

run = do
    withAppDo config (\appSt log -> do
        recipient <- randomPublicAddress
        mtx <- makeTransaction appSt recipient (Cent 10)
        case mtx of
            Nothing -> log "Not enough money"
            Just tx -> broadcastTransaction appSt tx)

main = run