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

makeConnection :: Address -> IO Socket
makeConnection address = withSocketsDo $
    do  addrinfos <- grabAddressInfo address

        sock <- socket (addrFamily addrinfos) Stream defaultProtocol

        setSocketOption sock KeepAlive 1

        connect sock (addrAddress addrinfos)
        return sock

withSocket :: Address -> (Socket -> IO a) -> IO a
withSocket address = bracket (makeConnection address) (`gracefulClose` timeOutToRecvTCP_FIN)

send :: B.ByteString -> Address -> IO ()
send msg address = withSocket address (`NSB.sendAll` msg)

sendToAll :: [Address] -> B.ByteString -> IO ()
sendToAll addresses msg = do
    forConcurrently_ addresses $ send msg

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
