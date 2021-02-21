
module Server where

import Control.Exception          (handle, IOException, finally)
import Control.Concurrent         (ThreadId, forkIO, threadDelay, forkFinally)
import Control.Monad              (forever, replicateM)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Concurrent.MVar    (withMVar, newMVar, MVar)
import Control.Arrow              (second,Arrow (first))
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.Int                   (Int32)
import Data.Functor               (void) 
import Text.Read                  (readMaybe)
import System.IO                  (IOMode(ReadMode, ReadWriteMode), hSetBuffering, hClose, 
                                   hGetContents, BufferMode (LineBuffering, NoBuffering), 
                                   hPutStr, Handle, hIsEOF, hGetChar, hFlush)
import System.Environment         (getArgs)
import Network.Socket
import Network.Transport.Internal (encodeEnum32, decodeNum32) -- TODO avoid this dependency
import qualified Network.Socket.ByteString as NSB
import Control.Exception.Base (bracket)

-- A message is preceded with 32 bits that read as an Int32 state the length of the message in bits.

appendLenBits :: B.ByteString -> B.ByteString
appendLenBits bs = B.append (encodeEnum32 (toEnum $ B.length bs :: Int32)) bs

msgToBytes :: String -> B.ByteString
msgToBytes = appendLenBits . UTF8.fromString

readMessage :: Socket -> IO (Maybe B.ByteString)
readMessage sock = do 
    lenBytes <- NSB.recv sock 4 
    if B.length lenBytes == 4 then do
        let len = decodeNum32 lenBytes
        msg <- NSB.recv sock len
        if B.length msg == len then
            return $ Just msg
        else
            return Nothing
    else
        return Nothing

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


type HandlerFunc = SockAddr -> B.ByteString -> B.ByteString 

type Miliseconds = Int
timeOutToRecvTCP_FIN :: Miliseconds
timeOutToRecvTCP_FIN = 1000

maxConnections = 5

-- Seems like i can change the type to MonadIO for free. I may want to use it as some transformer on IO
-- But hey then i can just add liftIO there, in the transformer. Let's revert the change for now
server :: Address
       -> HandlerFunc
       -> IO ()
server servAddr handler = withSocketsDo $ do
    lock <- newMVar ()
    bracket (open servAddr) close (loop lock)

    where

    open servAddr = do 
        addrinfo <- grabAddressInfo servAddr

        sock <- socket (addrFamily addrinfo) Stream defaultProtocol

        -- bind it to the address we're listening to
        bind sock (addrAddress addrinfo)

        listen sock maxConnections
        
        return sock

    loop lock sock = forever (procRequest (logger lock) sock)

    -- | Proccess incoming connections
    procRequest :: (String -> IO ()) -> Socket -> IO ThreadId
    procRequest log mastersock =
        do  (connsock, clientaddr) <- accept mastersock  -- gets us new socket
            log "server: client connnected"
            procConnection log connsock clientaddr 
                `forkFinally`
                const (gracefulClose connsock timeOutToRecvTCP_FIN)
    
    -- | Process incoming messages
    procConnection :: (String -> IO ()) -> Socket -> SockAddr -> IO ()
    procConnection log connsock clientaddr =
        do  mmsg <- readMessage connsock
            case mmsg of
                Nothing -> 
                    log "server: No valid message read. disconnected."
                Just msg -> 
                    finally 
                        (NSB.sendAll connsock $ appendLenBits $ handler clientaddr msg)
                        (log "server: client disconnected")         

    logger :: MVar () -> String -> IO ()
    logger lock str = void $ forkIO $ withMVar lock (\a -> print str >> return a)

answerPing :: HandlerFunc
answerPing _ bs = UTF8.fromString $ if UTF8.toString bs == "ping" then "pong" else ""

-- Return type of server can be RWST Config Log AppState IO () 
-- But writing logs would be more like using a function thats prints somewhere
-- And AppState is maybe TVar keeping State so no pure-mutable state inside statemonad needed
-- Logs can be implemented with a TVar queue, threads append to a queue and a seperate thread reads from the queue and logs.
-- (or just straight up logs it)