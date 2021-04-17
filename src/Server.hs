{-# LANGUAGE DeriveGeneric #-}
module Server where

import Control.Exception          (handle, IOException, finally)
import Control.Concurrent         (ThreadId, forkIO, threadDelay, forkFinally)
import Control.Monad              (forever, replicateM)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Concurrent.MVar    (withMVar, newMVar, MVar)
import Control.Arrow              (second,Arrow (first))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Int         (Int32, Int64)
import Data.Bits ( Bits(shiftL, shiftR), toIntegralSized ) 
import Data.Functor               (void) 
import Text.Read                  (readMaybe)
import System.IO                  (IOMode(ReadMode, ReadWriteMode), hSetBuffering, hClose, 
                                   hGetContents, BufferMode (LineBuffering, NoBuffering), 
                                   hPutStr, Handle, hIsEOF, hGetChar, hFlush)
import System.Environment         (getArgs)
import Network.Socket
-- import Network.Transport.Internal (encodeEnum32, decodeNum32) -- TODO avoid this dependency
import qualified Network.Socket.ByteString.Lazy as NSB
import Control.Exception.Base (bracket)
import Data.Word
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

int64ToByteString :: Int64 -> B.ByteString
int64ToByteString n = B.pack [aaaaaaaa, aaaaaaa, aaaaaa, aaaaa, aaaa, aaa, aa, a]
    where
        a = fromIntegral n :: Word8
        aa = fromIntegral (n `shiftR` 8) :: Word8
        aaa = fromIntegral (n `shiftR` 16) :: Word8
        aaaa = fromIntegral (n `shiftR` 24) :: Word8
        aaaaa = fromIntegral (n `shiftR` 32) :: Word8
        aaaaaa = fromIntegral (n `shiftR` 40) :: Word8
        aaaaaaa = fromIntegral (n `shiftR` 48) :: Word8
        aaaaaaaa = fromIntegral (n `shiftR` 56) :: Word8

byteStringToInt64 :: B.ByteString -> Int64
byteStringToInt64 bytes = case B.unpack bytes of
    [a7, a6, a5, a4, a3, a2, a1, a0]  -> 
        (toEnum (fromEnum a7) :: Int64) `shiftL` 56 + (toEnum (fromEnum a6) :: Int64) `shiftL` 48
        + (toEnum (fromEnum a5) :: Int64) `shiftL` 40 + (toEnum (fromEnum a4) :: Int64) `shiftL` 32
        + (toEnum (fromEnum a3) :: Int64) `shiftL` 24 + (toEnum (fromEnum a2) :: Int64) `shiftL` 16
        + (toEnum (fromEnum a1) :: Int64) `shiftL` 8 + (toEnum (fromEnum a0))


-- A message is preceded with 8 bytes that read as an Int64 state the length of the message in bits.

appendLenBits :: B.ByteString -> B.ByteString
appendLenBits bs = B.append (int64ToByteString (B.length bs)) bs

msgToBytes :: String -> B.ByteString
msgToBytes = appendLenBits . UTF8.fromString

readMessage :: Socket -> IO (Maybe B.ByteString)
readMessage sock = do 
    lenBytes <- NSB.recv sock 8
    if B.length lenBytes == 8 then do
        let len = byteStringToInt64 lenBytes
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
    deriving (Show, Generic)

instance ToJSON Address
instance FromJSON Address
    
-- get tcp AddrInfo for given url and port
grabAddressInfo :: Address -> IO AddrInfo
grabAddressInfo address = 
    -- getAddrInfo either returns non empty list or raises IOException
    head <$> getAddrInfo (Just $ defaultHints { addrSocketType = Stream }) 
        (Just $ hostName address) 
        (Just $ serviceName address)


type HandlerFunc = SockAddr -> B.ByteString -> IO B.ByteString 

type Miliseconds = Int
timeOutToRecvTCP_FIN :: Miliseconds
timeOutToRecvTCP_FIN = 1000

maxConnections = 5

-- Seems like i can change the type to MonadIO for free. I may want to
server :: Address
       -> (String -> IO ())
       -> HandlerFunc
       -> IO ()
server servAddr logger handler = withSocketsDo $ do
    bracket (open servAddr) close loop

    where

    open servAddr = do 
        addrinfo <- grabAddressInfo servAddr

        sock <- socket (addrFamily addrinfo) Stream defaultProtocol

        -- bind it to the address we're listening to
        bind sock (addrAddress addrinfo)

        listen sock maxConnections
        
        return sock

    loop sock = forever (procRequest logger sock)

    -- | Proccess incoming connections
    procRequest :: (String -> IO ()) -> Socket -> IO ThreadId
    procRequest log mastersock =
        do  (connsock, clientaddr) <- accept mastersock  -- gets us new socket
            log "server: Client connnected."
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
                        (do answer <- handler clientaddr msg
                            NSB.sendAll connsock $ appendLenBits answer)
                        (log "server: Client disconnected.")         

answerPing :: HandlerFunc
answerPing _ bs = UTF8.fromString <$> if UTF8.toString bs == "ping" then return "pong" else return ""