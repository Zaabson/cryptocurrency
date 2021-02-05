import Network.Socket
import Data.List
import Control.Monad (forever)
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handler = withSocketsDo $
    do  -- Look up the port.  Either raises an exception or returns
        -- a nonempty list.  
        addrinfos <- getAddrInfo
                     (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                     Nothing (Just port)
        let serveraddr = head addrinfos
        print serveraddr
        -- create a socket
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol

        -- bind it to the address we're listening to
        bind sock (addrAddress serveraddr)

        -- listen for the connections
        listen sock 5

        -- create a lock for synchronizing access to the handler
        lock <- newMVar ()

        -- loop forever listening for connections
        forever $ procRequest lock sock

    where
        -- | Proccess incoming connections
        procRequest :: MVar () -> Socket -> IO ThreadId
        procRequest lock mastersock =
            do  (connsock, clientaddr) <- accept mastersock  -- gets us new socket
                handle lock clientaddr "syslogtcpserver.hs: client connnected"
                forkIO $ procMessages lock connsock clientaddr

        -- | Process incoming messages
        procMessages :: MVar () -> Socket -> SockAddr -> IO ()
        procMessages lock connsock clientaddr = 
            do  connhdl <- socketToHandle connsock ReadMode
                hSetBuffering connhdl LineBuffering
                messages <- hGetContents connhdl
                mapM_ (handle lock clientaddr) (lines messages)   -- no talking with connhdl whatsoever
                hClose connhdl
                handle lock clientaddr "syslogtcpserver.hs: client disconnected" -- should this be using handler?
        
        --     :: Mvar () -> SockAddr -> String -> IO ()
        handle :: MVar () -> HandlerFunc
        handle lock clientaddr msg = withMVar lock
            (\a -> handler clientaddr msg >> return a)

simpleHandler :: HandlerFunc
simpleHandler clientaddr msg = putStrLn $ "From :" ++ show clientaddr ++ ": " ++ msg