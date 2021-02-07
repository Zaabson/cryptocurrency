module Server where

import Network.Socket
import Control.Exception (handle, IOException, finally)
import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (forever)
import Control.Concurrent.MVar (withMVar, newMVar, MVar)
import System.IO (IOMode(ReadMode, ReadWriteMode), hSetBuffering, hClose, hGetContents, BufferMode (LineBuffering, NoBuffering))
import Data.Functor (void)
import Control.Arrow (second,Arrow (first))

-- type HostName = String
-- Either a host name e.g., "haskell.org" or a numeric host address string consisting of a dotted decimal IPv4 address or an IPv6 address e.g., "192.168.0.1".

-- type ServiceName = String
-- Either a service name e.g., "http" or a numeric port number.

data Address = Address {hostName :: HostName, serviceName :: ServiceName}

-- -- get tcp AddrInfo for given url and port
-- grabAddressInfo :: Address -> IO (Maybe AddrInfo)
-- grabAddressInfo address = 
--     -- getAddrInfo either returns non empty list or raise IOException
--     handle ((\e -> return Nothing) :: IOException -> IO (Maybe AddrInfo)) 
--         $ Just . head <$> getAddrInfo (Just $ defaultHints { addrSocketType = Stream }) 
--             (Just $ hostName address) 
--             (Just $ serviceName address)

-- get tcp AddrInfo for given url and port
grabAddressInfo :: Address -> IO AddrInfo
grabAddressInfo address = 
    -- getAddrInfo either returns non empty list or raises IOException
    head <$> getAddrInfo (Just $ defaultHints { addrSocketType = Stream }) 
        (Just $ hostName address) 
        (Just $ serviceName address)

makeSocket :: Address -> IO Socket
makeSocket address = withSocketsDo $ do
    addrinfo <- grabAddressInfo address 
    socket (addrFamily addrinfo) Stream defaultProtocol


data FooMessage = FooMessage

-- first msgLenBits signify the length of the rest of the message
msgLenBits = 10
maxMsgSize = 10 ^ msgLenBits

-- trim Int to be msgLenBits digits long
intToLenBits :: Int -> String
intToLenBits x = replicate (msgLenBits - length dropped) '0' ++ dropped  where 
    str = show x
    dropped = drop (length str - msgLenBits) str

appendLenBits :: String -> String
appendLenBits str = intToLenBits (length str) ++ str

splitMessages :: String -> [String]
splitMessages str =
    case readMessage' str of
        Nothing         -> []
        Just (msg, str) -> msg : splitMessages str 

safeTake :: Int -> [a] -> Maybe ([a], [a])
safeTake n (x : xs) | n > 0 = first (x :) <$> safeTake (n - 1) xs
safeTake 0 xs = Just ([], xs)
safeTake _ _  = Nothing

readMessage' :: String -> Maybe (String, String)
readMessage' str = do
    (num, str) <- safeTake msgLenBits str 
    safeTake (read num) str

readMessage str = do
    (num, str) <- safeTake msgLenBits str 
    fst <$> safeTake (read num) str

--  :: String -> String

-- String -> [FooMessage]

type HandlerFunc = Socket -> SockAddr -> String -> IO ()

serverAddress = Address {hostName = "localhost", serviceName = "80"}
maxConnections = 5

server :: Address
       -> HandlerFunc
       -> IO ()
server servAddr handler = withSocketsDo $ do
    
    addrinfo <- grabAddressInfo servAddr

    sock <- socket (addrFamily addrinfo) Stream defaultProtocol

    -- bind it to the address we're listening to
    bind sock (addrAddress addrinfo)

    listen sock maxConnections

    lock <- newMVar ()

    forever $ procRequest (logger lock) sock

    where
    -- | Proccess incoming connections
    procRequest :: (String -> IO ()) -> Socket -> IO ThreadId
    procRequest log mastersock =
        do  (connsock, clientaddr) <- accept mastersock  -- gets us new socket
            log "server: client connnected"
            forkIO $ procConnection log connsock clientaddr

    -- | Process incoming messages
    procConnection :: (String -> IO ()) -> Socket -> SockAddr -> IO ()
    procConnection log connsock clientaddr = 
        do  connhdl <- socketToHandle connsock ReadWriteMode 
            hSetBuffering connhdl NoBuffering
            mmsg <- hGetContents connhdl
            case readMessage mmsg of
                Nothing -> 
                    do hClose connhdl
                       log "server: No valid message read."
                Just msg -> 
                    finally 
                        (handler connsock clientaddr msg) 
                        (do hClose connhdl
                            log "server: client disconnected")         

    logger :: MVar () -> String -> IO ()
    logger lock str = void $ forkIO $ withMVar lock (\a -> print str >> return a)

-- Return type of server can be RWST Config Log AppState IO () 
-- But writing logs would be more like using a function thats prints somewhere
-- And AppState is maybe TVar keeping State so no pure-mutable state inside statemonad needed
-- Logs can be implemented with a TVar queue, threads append to a queue and a seperate thread reads from the queue and logs.