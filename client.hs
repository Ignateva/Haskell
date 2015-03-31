import Network
import System.IO
import Control.Monad
import System.Environment
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

 
data Async a = Async ThreadId (TMVar (Either SomeException a))

main = withSocketsDo $ do
       [host] <- getArgs
       handle <- connectTo host (PortNumber (12345))
       putStrLn "Connected!"      
       start ((receive handle) `catch` handler) (sendMessage handle)
       hClose handle
       return ()

start :: IO a -> IO b -> IO (Either a b)
start ioa iob =
  withAsync ioa $ \a ->
  withAsync iob $ \b ->
    waitEither a b

receive :: Handle -> IO ()
receive handle = forever $ do
       hSetBuffering handle NoBuffering
       msg <- hGetLine handle
       putStrLn msg

handler :: IOError -> IO ()
handler e =  putStrLn " \nBye Bye!!"

sendMessage :: Handle -> IO ()
sendMessage handle = forever $ do
       line <- getLine
       hPutStrLn handle line

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  t <- forkFinally action (atomically . putTMVar var)
  return (Async t var)

waitSTM :: Async a -> STM a
waitSTM a = do
  r <- waitCatchSTM a
  case r of
    Left e  -> throwSTM e
    Right a -> return a

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var


waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $
  fmap Left (waitSTM a)
    `orElse`
  fmap Right (waitSTM b)

cancel :: Async a -> IO ()
cancel (Async t var) = throwTo t ThreadKilled

withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io operation = bracket (async io) cancel operation


