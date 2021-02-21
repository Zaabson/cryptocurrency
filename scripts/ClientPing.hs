import Server (Address(..))
import Client (ping)
import System.Environment (getArgs)

main = do
    port : _ <- getArgs
    let addr = Address {hostName="localhost", serviceName=port}
    ping 5000000 addr >>= print

