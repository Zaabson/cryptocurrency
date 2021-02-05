module FooBar () where

import Control.Exception
import Network.Socket
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad (replicateM)
import Data.Either (either)
import System.IO

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

type Port = String
data Address = Address { url :: HostName, port :: Port }

-- get tcp AddrInfo for given url and port
grabAddressInfo :: Address -> IO [AddrInfo]
grabAddressInfo address = let hints = defaultHints { addrSocketType = Stream } in 
    if   url address == "localhost"
    then getAddrInfo (Just hints) Nothing (Just $ port address)
    else getAddrInfo (Just hints) (Just $ url address) (Just $ port address)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- creates tcp socket
connectSocket :: Address -> IO (Maybe Socket)
connectSocket address = withSocketsDo $ do
    addrinfos <- grabAddressInfo address
    case safeHead addrinfos of
        Nothing -> return Nothing
        Just pinginfo -> do sock <- socket (addrFamily pinginfo) Stream (addrProtocol pinginfo)
                            catch (connect sock (addrAddress pinginfo) >> return (Just sock))
                                  (\(SomeException _) -> return Nothing)


                        
-- make this async

-- blocks till a message is read
-- or fuckin explodes when eof
readMsg :: Handle -> IO String
readMsg hdl = do
    -- read msgLenBits bits
    lenstr <- replicateM msgLenBits $ hGetChar hdl
    let len = read lenstr :: Int
    -- read message
    replicateM len $ hGetChar hdl

-- tries to read a full message. blocks till a message is read.
-- returns Nothing if any error occurs.
readMsgMaybe :: Handle -> IO (Maybe String)
readMsgMaybe hdl = handle gotNothing (Just <$> readMsg hdl)
    where gotNothing = const $ return Nothing ::  SomeException -> IO (Maybe String)

type Microseconds = Int
-- ping an Address and wait for answer
pingDelay :: Microseconds -> Address -> IO Bool
pingDelay delay address = withSocketsDo $ do
    -- create socket and get a handle   
    msock <- connectSocket address
    case msock of 
        Nothing -> return False
        Just sock -> finally (doPinging sock) (close sock)
    where doPinging sock = do
            setSocketOption sock KeepAlive 1
            hdl <- socketToHandle sock ReadWriteMode
            -- send PING
            hPutStrLn hdl $ appendLenBits "PING"

            waitMsg <- async $ readMsgMaybe hdl

            waitDelay <- async $ threadDelay delay >> return False

            either (== Just "PING") id `fmap` waitEither waitMsg waitDelay

ping = pingDelay 5000000

pingListDelay :: Microseconds -> [Address] -> IO [Bool]
pingListDelay delay = mapM $ pingDelay delay

pingList :: [Address] -> IO [Bool]
pingList = pingListDelay 50000

keepSelected :: [a] -> [Bool] -> [a]
keepSelected [] bs = []
keepSelected (x:xs) (True:bs) = x : keepSelected xs bs
keepSelected (x:xs) (False:bs) = keepSelected xs bs

-- Node needs: 
-- running :: IO ()
-- handler :: Messsage -> IO
-- but these two share state

-- init :: IO S
-- run :: S -> IO ()
-- shorten: start :: IO S
-- handler :: S -> Message -> IO ()

-- zamienic IO, na STM i w tym module się troszczyć by modyfikowanie S powtarzane?




turnOn :: String -> IO Handle
turnOn port = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just defaultHints) Nothing (Just port)
    let serveraddr = head addrinfos
    print serveraddr
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 1
    (connsock, clientaddr) <- accept sock
    socketToHandle connsock ReadWriteMode


call :: String -> IO Handle
call port = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just defaultHints) Nothing (Just port)
    let serveraddr = head addrinfos
    print serveraddr
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    socketToHandle sock ReadWriteMode
    