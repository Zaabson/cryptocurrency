module Client where

import Network.Socket
import Server
import Control.Monad (forM, void)
import Control.Monad.Cont (forM_)
import Control.Concurrent.Async
import System.IO (Handle, IOMode (WriteMode, ReadWriteMode), BufferMode (BlockBuffering, NoBuffering), hSetBuffering, hPutStr, hClose, hGetContents)
import Control.Exception (bracket, handle, IOException)
import Control.Concurrent (threadDelay)

makeConnection :: Address -> IO Handle
makeConnection address = withSocketsDo $
    do  addrinfos <- grabAddressInfo address

        sock <- socket (addrFamily addrinfos) Stream defaultProtocol

        setSocketOption sock KeepAlive 1

        connect sock (addrAddress addrinfos)

        h <- socketToHandle sock ReadWriteMode

        hSetBuffering h NoBuffering
        
        return h

withConnection :: Address -> (Handle -> IO a) -> IO a
withConnection address = bracket (makeConnection address) hClose

send :: String -> Address -> IO ()
send msg address = withConnection address $ \hdl -> 
    hPutStr hdl (appendLenDigits msg)

sendToAll :: [Address] -> String -> IO ()
sendToAll addresses msg = do
    forConcurrently_ addresses $ send msg

type Microseconds = Int

ping :: Microseconds -> Address -> IO Bool
ping delay address =
    (const (return False) :: IOException -> IO Bool) `handle` tryPing
    where
    tryPing = 
        withConnection address $ \hdl -> do 
            -- send "ping"
            hPutStr hdl (appendLenDigits "ping")

            -- look for answer
            answer <- async $ do
                response <- readMessage hdl
                case response of
                    Nothing     -> return False
                    Just "pong" -> return True
            
            -- wait the delay 
            waiting <- async $ void $ threadDelay delay

            res <- answer `waitEitherCancel` waiting
            return $ case res of
                Left bl  -> bl
                Right () -> False


