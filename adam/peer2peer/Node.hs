module Node where

import Message
import Control.Concurrent
import Network.Socket
import Control.Concurrent.MVar
import Data.List

type MessageHandler = SockAddr -> Message -> IO ()

runNode :: String -- port
        -> MessageHandler
        -> IO ()
runNode port handler = withSocketsDo $
    do  -- Look up the port.  Either raises an exception or returns
        -- a nonempty list.  
        addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
        let serveraddr = head addrinfos

        -- create a socket
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol

        -- bind it to the address we're listening to
        bind sock (addrAddress serveraddr)

        -- listen for the connections
        listen sock 5

        -- create a lock for synchronizing access to the handler
        lock <- newMVar ()

        return ()