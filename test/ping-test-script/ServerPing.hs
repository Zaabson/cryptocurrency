import Server (Address(..), server, answerPing)
import System.Environment (getArgs)

main = do
    port : _ <- getArgs
    let addr = Address {hostName="localhost", serviceName=port}
    server addr print answerPing

