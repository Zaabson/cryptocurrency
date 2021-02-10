module Server where

import Network.Socket
import Control.Exception (handle, IOException, finally)
import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (forever, replicateM)
import Control.Concurrent.MVar (withMVar, newMVar, MVar)
import System.IO (IOMode(ReadMode, ReadWriteMode), hSetBuffering, hClose, hGetContents, BufferMode (LineBuffering, NoBuffering), hPutStr, Handle, hIsEOF, hGetChar)
import Data.Functor (void)
import Control.Arrow (second,Arrow (first))
import Text.Read (readMaybe)

-- first msgLenDigits signify the length of the rest of the message
msgLenDigits = 10
maxMsgSize = 10 ^ msgLenDigits

-- trim Int to be msgLenDigits digits long
intToLenDigits :: Int -> String
intToLenDigits x = replicate (msgLenDigits - length dropped) '0' ++ dropped  where 
    str = show x
    dropped = drop (length str - msgLenDigits) str

appendLenDigits :: String -> String
appendLenDigits str = intToLenDigits (length str) ++ str

readMessage :: Handle -> IO (Maybe String)
readMessage hdl = do
    mlen <- readn msgLenDigits hdl
    case mlen >>= readMaybe of
        Nothing  -> return Nothing
        Just len -> readn len hdl
    where 
        mGetChar :: Handle -> IO (Maybe Char)
        mGetChar hdl = do
            eof <- hIsEOF hdl
            if eof then
                return Nothing 
            else
                Just <$> hGetChar hdl
        readn :: Int -> Handle -> IO (Maybe String)
        readn n hdl = sequence <$> replicateM n (mGetChar hdl)

-- type HostName = String
-- Either a host name e.g., "haskell.org" or a numeric host address string consisting of a dotted decimal IPv4 address or an IPv6 address e.g., "192.168.0.1".

-- type ServiceName = String
-- Either a service name e.g., "http" or a numeric port number.

data Address = Address {hostName :: HostName, serviceName :: ServiceName}

-- get tcp AddrInfo for given url and port
grabAddressInfo :: Address -> IO AddrInfo
grabAddressInfo address = 
    -- getAddrInfo either returns non empty list or raises IOException
    head <$> getAddrInfo (Just $ defaultHints { addrSocketType = Stream }) 
        (Just $ hostName address) 
        (Just $ serviceName address)


type HandlerFunc = SockAddr -> String -> String

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
            mmsg <- readMessage connhdl
            case mmsg of
                Nothing -> 
                    do hClose connhdl
                       log "server: No valid message read. disconnected."
                Just msg -> 
                    finally 
                        (hPutStr connhdl $ appendLenDigits $ handler clientaddr msg)
                        (do hClose connhdl
                            log "server: client disconnected")         

    logger :: MVar () -> String -> IO ()
    logger lock str = void $ forkIO $ withMVar lock (\a -> print str >> return a)

answerPing :: HandlerFunc
answerPing _ "ping" = "pong"
answerPing _ _      = ""

-- Return type of server can be RWST Config Log AppState IO () 
-- But writing logs would be more like using a function thats prints somewhere
-- And AppState is maybe TVar keeping State so no pure-mutable state inside statemonad needed
-- Logs can be implemented with a TVar queue, threads append to a queue and a seperate thread reads from the queue and logs.
-- (or just straight up logs it)