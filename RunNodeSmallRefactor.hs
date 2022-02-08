
-- how to open only a single handle but also have it closed automaticaly?
--                             name to log under, message to log
logger :: LoggingMode -> MVar () -> String -> IO ()
logger Silent _ _ = return ()
logger mode lock str       = void $ forkIO $ withMVar lock (\a -> do
    time <- getZonedTime
    -- TODO: fix time not displaying properly
    let timeinfo = formatTime defaultTimeLocale "%T" time
    let msg =  timeinfo ++ ": " ++ str
    case mode of 
        ToStdin     -> putStrLn msg
        ToFile path -> withFile path AppendMode (`hPutStrLn` msg)
    return a)

-- AppState, running main (mining, server), logging function
newtype RunningApp = RunningApp (AppState, Async (), String -> IO ())


runNode :: Config -> IO (Maybe RunningApp)
runNode config = do
    -- make logging function
    loggerlock <- newMVar ()
    let log = logger (loggingMode config) loggerlock
    let targetHash = difficultyToTargetHash $ targetDifficulty config

    -- TODO: optional cmd arg to load state from save, otherwise only loads

    eitherPeersAndBlocks  <- runErrorT $ do
        peers <- (ErrorT $ eitherDecodeFileStrict (peersFilepath config)) `catchError` \e -> throwError ("app: Couldn't open or parse peers file. Quits with error: \"" ++ err ++ ["."])
        fixed <- (ErrorT $ eitherDecodeFileStrict (blockchainFilepath config)) `catchError` \e -> throwError ("app: Failed to load blockchain from file. Quits with error: \"" ++ err ++ ["."])
        return (peers, fixed) 

    case eitherPeersAndBlocks of
        Left err -> log err >> return Nothing
        Right (peers, fixed) -> do
            
            log "app: Loaded fixed blocks."

            blockchainState <- initBlockchainState (blockchainGenesis config) fixed
            appState        <- initAppState blockchainState peers
    
            -- TODO: catching up to  blockchain
            -- query for blocks after our last block
            forkIO $ catchUpToBlockchain forkMaxDiff targetHash appState log

            let mine = mining targetHash appState (minerWaitForTxs config) log

            let serverAddr = Address "localhost" (port config)
            let runServer = server serverAddr log (serverHandler (handleMessage forkMaxDiff targetHash appState log) log)
    
            -- forkIO runServer
            -- forkIO mine
            main <- async $ concurrently_ mine runServer

            return (Just $ RunningApp (appState, main, log))

    where            
        
        initBlockchainState :: Genesis -> FixedBlocks -> IO BlockchainState
        initBlockchainState genesis fixed = 
            BlockchainState genesis
                        <$> newTVarIO fixed
                        <*> newTVarIO (LivelyBlocks {
                                root=case fixed of 
                                    FixedBlocks [] -> shash256 (Left $ genesis)
                                    FixedBlocks (b:bs) -> blockRef b, forest=[]})
                        <*> newTVarIO (FutureBlocks Map.empty)
                        <*> newTVarIO (collectUTXOs Map.empty fixed)

        initAppState :: BlockchainState -> PeersSet -> IO AppState
        initAppState blockchainState peers
            AppState blockchainState
                <$> newTVarIO Seq.empty
                <*> newTVarIO peers
                <*> newTVarIO []
