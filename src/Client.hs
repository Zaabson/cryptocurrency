module Client where

import           Control.Exception  (bracket, handle, IOException)
import           Control.Concurrent (threadDelay)
import           Control.Monad      (forM, void, forever)
import           Control.Monad.Cont (forM_)
import           Control.Concurrent.Async
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           System.IO          (Handle, IOMode (WriteMode, ReadWriteMode), 
                                     BufferMode (BlockBuffering, NoBuffering), hSetBuffering,
                                     hPutStr, hClose, hGetContents)
import           System.Environment (getArgs)
import           Network.Socket
import qualified Network.Socket.ByteString.Lazy as NSB


import Server                       (Address, appendLenBits, grabAddressInfo, timeOutToRecvTCP_FIN, readMessage, msgToBytes)
import Data.Either (fromLeft)

makeConnection :: Address -> IO Socket
makeConnection address = withSocketsDo $
    do  addrinfos <- grabAddressInfo address

        sock <- socket (addrFamily addrinfos) Stream defaultProtocol

        setSocketOption sock KeepAlive 1

        connect sock (addrAddress addrinfos)
        return sock

withSocket :: Address -> (Socket -> IO a) -> IO a
withSocket address = bracket (makeConnection address) (`gracefulClose` timeOutToRecvTCP_FIN)

-- send :: B.ByteString -> Address -> IO ()
-- send msg address = withSocket address (`NSB.sendAll` msg)

-- sendToAll :: B.ByteString -> [Address] -> IO ()
-- sendToAll msg addresses = do
--     forConcurrently_ addresses $ send msg

-- Send bytes, await a response (1s) and do something with the response if we get it
sendAndReceive :: B.ByteString -> Address -> (Maybe B.ByteString -> IO a) -> IO a
sendAndReceive msg address k = do
    response <- ((\e -> return (Right ())) :: IOException -> IO (Either a ())) `handle` awaitResponse
    
    k $ fromLeft Nothing response 

    where
        awaitResponse = withSocket address $ \sock -> do
            -- send "ping"
            NSB.sendAll sock (appendLenBits msg)

            -- look for answer
            answer <- async (readMessage sock)
                
            -- wait the delay 
            waiting <- async $ void $ threadDelay 1000000

            answer `waitEitherCancel` waiting

send :: Socket -> B.ByteString -> IO ()
send sock msg = NSB.sendAll sock (appendLenBits msg)

type Microseconds = Int

ping :: Microseconds -> Address -> IO Bool
ping delay address =
    (const (return False) :: IOException -> IO Bool) `handle` tryPing
    where
    tryPing = 
        withSocket address $ \sock -> do 
            -- send "ping"
            NSB.sendAll sock (msgToBytes "ping")

            -- look for answer
            answer <- async $ do
                response <- readMessage sock
                case response of
                    Nothing     -> return False
                    Just pong -> do
                        if UTF8.toString pong == "pong" then
                            return True
                        else
                            return False
            
            -- wait the delay 
            waiting <- async $ void $ threadDelay delay

            res <- answer `waitEitherCancel` waiting
            return $ case res of
                Left bl  -> bl
                Right () -> False
