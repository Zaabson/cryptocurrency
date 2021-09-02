{-# LANGUAGE LambdaCase #-}

import App (sendAndReceiveMsg)
import Message (Message(PingMessage))
import Server (Address(Address, serviceName, hostName))
import System.Environment (getArgs)

main = do 
    port : _ <- getArgs
    let addr = Address {hostName="localhost", serviceName=port}
    sendAndReceiveMsg PingMessage (\case 
        Nothing -> print "Didn't receive answer."
        Just answer -> print answer) addr