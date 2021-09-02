
import Server (Address(..), server)
import Client (ping)
import System.Environment (getArgs)
import App (serverHandler)
import Message (Answer(AnswerPing, MessageParseError), Message (PingMessage))

handler sock PingMessage = return AnswerPing
handler sock _ = return MessageParseError

main = do
    port : _ <- getArgs
    let addr = Address {hostName="localhost", serviceName=port}

    server addr print (serverHandler handler print)


