import Network.Socket
import Data.List
import System.IO
import Data.Bits

{- | Priorities define how important a log message is. -}

data Priority = 
            DEBUG                   -- ^ Debug messages
          | INFO                    -- ^ Information
          | NOTICE                  -- ^ Normal runtime conditions
          | WARNING                 -- ^ General Warnings
          | ERROR                   -- ^ General Errors
          | CRITICAL                -- ^ Severe situations
          | ALERT                   -- ^ Take immediate action
          | EMERGENCY               -- ^ System is unusable
                    deriving (Eq, Ord, Show, Read, Enum)

{- | Facilities are used by the system to determine where messages
are sent. -}

data Facility = 
              KERN                      -- ^ Kernel messages
              | USER                    -- ^ General userland messages
              | MAIL                    -- ^ E-Mail system
              | DAEMON                  -- ^ Daemon (server process) messages
              | AUTH                    -- ^ Authentication or security messages
              | SYSLOG                  -- ^ Internal syslog messages
              | LPR                     -- ^ Printer messages
              | NEWS                    -- ^ Usenet news
              | UUCP                    -- ^ UUCP messages
              | CRON                    -- ^ Cron messages
              | AUTHPRIV                -- ^ Private authentication messages
              | FTP                     -- ^ FTP messages
              | LOCAL0                  
              | LOCAL1
              | LOCAL2
              | LOCAL3
              | LOCAL4
              | LOCAL5
              | LOCAL6
              | LOCAL7
                deriving (Eq, Show, Read)

facToCode = [ 
        (KERN, 0),
        (USER, 1),
        (MAIL, 2),
        (DAEMON, 3),
        (AUTH, 4),
        (SYSLOG, 5),
        (LPR, 6),
        (NEWS, 7),
        (UUCP, 8),
        (CRON, 9),
        (AUTHPRIV, 10),
        (FTP, 11),
        (LOCAL0, 16),
        (LOCAL1, 17),
        (LOCAL2, 18),
        (LOCAL3, 19),
        (LOCAL4, 20),
        (LOCAL5, 21),
        (LOCAL6, 22),
        (LOCAL7, 23)
    ]

codeToFac = map (\(x, y) -> (y, x)) facToCode


{- | We can't use enum here because the numbering is discontiguous -}
codeOfFac :: Facility -> Int
codeOfFac f = case lookup f facToCode of
                Just x -> x
                _ -> error "Internal error in codeOfFac"

facOfCode :: Int -> Facility
facOfCode f = case lookup f codeToFac of
                Just x -> x
                _ -> error "Invalid code in facOfCode"

data SyslogHandle = 
    SyslogHandle {slHandle :: Handle,
                  slProgram :: String}

openlog :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> String               -- ^ Name to log under
        -> IO SyslogHandle      -- ^ Handle to use for logging
openlog hostname port progname = withSocketsDo $
    do  -- Look up the hostname and port.  Either raises an exception
        -- or returns a nonempty list.  First element in that list
        -- is supposed to be the best option.
        addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
        let serveraddr = head addrinfos

        -- establish a socket for communication
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol

        -- Mark the socket for keep-alive handling since it may be idle
        -- for long periods of time
        setSocketOption sock KeepAlive 1

        -- Connect to server
        connect sock (addrAddress serveraddr)

        h <- socketToHandle sock WriteMode

        -- We're going to set buffering to BlockBuffering and then
        -- explicitly call hFlush after each message, below, so that
        -- messages get logged immediately
        hSetBuffering h (BlockBuffering Nothing)
        
        return $ SyslogHandle h progname


syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogh fac pri msg = 
    do  hPutStrLn (slHandle syslogh) sendmsg
        hFlush (slHandle syslogh)
    where code     = makeCode fac pri
          sendmsg  = "<" ++ show code ++ ">" ++ slProgram syslogh ++ ": " ++ msg

closelog :: SyslogHandle -> IO ()
closelog syslogh = hClose (slHandle syslogh)

{- | Convert a facility and a priority into a syslog code -}
makeCode :: Facility -> Priority -> Int
makeCode fac pri =
    let faccode = codeOfFac fac
        pricode = fromEnum pri 
        in
          (faccode `shiftL` 3) .|. pricode

main = undefined