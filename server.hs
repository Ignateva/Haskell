 {-# LANGUAGE RecordWildCards #-}  --Позволяет синтаксис подстановки " .. "в записи шаблона

 import Control.Applicative
 import Control.Concurrent
 import Control.Concurrent.STM
 import Control.Exception
 import Control.Monad
 import Network
 import Prelude hiding (id)
 import System.IO
 import Text.Printf
 import qualified Data.Map as Map
 
 type ClientName = String
 
 data Client = Client
   {  clientName    :: ClientName           -- имя клиента
   , clientHandle   :: Handle
   , clientKicked   :: TVar (Maybe String)  -- был ли пользователь выгнан из чата
   , clientAdmin    :: TVar (Bool)          --является ли пользователь модератором
   , clientMessage  :: TChan Message        --все другие сообщения, которые могут быть отправлены клиенту
   }

 data Session = Session
   { 
	clients :: TVar (Map.Map ClientName Client)
   }
   
 data Message = Notice String                       -- Сообщение с сервера
              | Private String ClientName String    -- Личное сообщение другому клиенту
              | Broadcast ClientName String         -- Публичное сообщение от клиента
              | Command String                      -- Команда

 data Async a = Async ThreadId (TMVar (Either SomeException a))

 main :: IO ()
 main = withSocketsDo $ do
   session <- newSession --создаем новую сессию (базу пользователей)
   s <- listenOn (PortNumber (fromIntegral port)) -- создаем сетевой сокет для прослушивания порта 12345.
   forever $ do  --входим в цикл, чтобы принимать соединения от клиентов.
       (handle, host, port) <- accept s --ждет нового соединения клиента.
       printf "Connection from %s: %s\n" host (show port)
       forkFinally (talk handle session) (\_ -> hClose handle) --создать новый поток для обработки запроса.
 
 port :: Int
 port = 12345

 -------Проверить, в сети ли пользователь с таким ником ----	
 showUser :: Session -> Client -> ClientName -> STM ()
 showUser Session{..} client1 name = do
	clientmap <- readTVar clients
	case Map.lookup name clientmap of
		Nothing -> sendMessage client1 $ Notice (name ++ " doesn't exist")
        	Just client -> sendMessage client1 $ Notice (name ++ " is in the network!")
------------------------------------------------------------

-------- Жалоба модератору----
 complaint :: Session -> Client -> String -> STM ()
 complaint Session{..} c msg = do
   clientmap <- readTVar clients
   mapM_ (\client1 -> check_a client1 msg (clientName c)) (Map.elems clientmap) 	
	
 check_a :: Client -> String -> ClientName -> STM ()
 check_a client@Client{..} msg c= do
	adm <- readTVar clientAdmin 
        when (adm == True) $ sendMessage client $ Notice ("Complaint from " ++ c ++ " : " ++ msg)
-------------------------------

--------Вывод всех пользователей-------	
 showAllUsers :: Session -> Client -> STM ()
 showAllUsers Session{..} client = do
	clientmap <- readTVar clients
        mapM_ (\client1 -> sendMessage client $ Notice (clientName client1)) (Map.elems clientmap) 
--------------------------------------------------------------------------------------------------------

------Отправка сообщений ------			
 sendMessage :: Client -> Message -> STM ()
 sendMessage Client{..} msg =
   writeTChan clientMessage msg
-------------------------------

-------Для модератора: написать причину исключения из чата-----
 reason :: Client -> String -> STM()
 reason Client{..} reason =
      writeTVar clientKicked $ Just reason
---------------------------------------------------------------

------Для модератора: исключение из чата -------					  
 kick :: Session -> Client -> ClientName -> String -> STM ()
 kick Session{..} clientik name reas = do
         clientmap <- readTVar clients 
         case Map.lookup name clientmap of
              Nothing -> sendMessage clientik $ Notice "Nonexistent user"
              Just client -> reason client reas
 -----------------------------------------------

------Отправить личное сообщение ------		
 tell :: Session -> Client -> ClientName -> Message -> STM ()
 tell Session{..} clientik@Client{..} name msg = do
         clientmap <- readTVar clients 
         case Map.lookup name clientmap of
              Nothing -> sendMessage clientik $ Notice "Nonexistent user"
              Just client -> do 
                  sendMessage client msg
                  sendMessage clientik msg
---------------------------------------

------Стать модератором: проверить пароль----
 checkPasswd :: String -> Client -> STM ()
 checkPasswd password client@Client{..} = do
         if (password == passwd)
         then do 
              writeTVar clientAdmin True
              sendMessage client $ Notice "You are a moderator"		
         else sendMessage client $ Notice "Invalid password!"
         where passwd = "123"
 -------------------------------------------

------Стать модератором --------------------				 
 checkAdmin :: Session -> Client -> ClientName -> String -> STM ()
 checkAdmin session@Session{..} client@Client{..} name reason = do
         adm <- readTVar clientAdmin 
         if (adm == True) 
              then kick session client name reason
              else sendMessage client $ Notice "You are not a moderator!"
---------------------------------------------------------------

 newSession :: IO Session
 newSession = do
   c <- newTVarIO Map.empty
   return Session { clients = c }
     
 talk :: Handle -> Session -> IO ()   -- настраивает потоки для обработки нового клиентского подключения
 talk handle session@Session{..} = do
   hSetNewlineMode handle universalNewlineMode
   hSetBuffering handle LineBuffering
   readName
  where
    readName = do
      hPutStrLn handle " "
      hPutStrLn handle "Enter your login: "
      name <- hGetLine handle
      if null name
        then readName
        else mask $ \restore -> do --замаскировать асинхронные исключения.
              login <- checkClient session name handle 
              case login of
                Nothing -> restore $ do 
                  hPrintf handle "Choose another login: %s is used.\n " name
                  readName
                Just client ->
                   restore (run session client)
                       `finally` removeClient session name
 
 checkClient :: Session -> ClientName -> Handle -> IO (Maybe Client)
 checkClient session@Session{..} name handle = atomically $ do
   clientmap <- readTVar clients
   if Map.member name clientmap
     then return Nothing
     else do client <- newClient name handle
             writeTVar clients $ Map.insert name client clientmap
             broadcast session  $ Notice (name ++ " has connected")
             return (Just client)
 
 newClient :: ClientName -> Handle -> STM Client
 newClient name handle = do
   adm <- newTVar False 
   m <- newTChan 
   k <- newTVar Nothing 
   return Client { clientAdmin       = adm
                 , clientName        = name 
                 , clientHandle      = handle 
                 , clientMessage     = m
                 , clientKicked      = k
                 }

 run :: Session -> Client -> IO ()
 run serv@Session{..} client@Client{..} = do
    hPrintf clientHandle "\nWelcome in chat, %s! Command '/help' will help you! \n" clientName
    race session receive
    return ()
  where
   receive = forever $ do
     msg <- hGetLine clientHandle
     atomically $ sendMessage client (Command msg)
   session = join $ atomically $ do
     k <- readTVar clientKicked
     case k of
       Just reason -> return $
         hPutStrLn clientHandle $ "You have been kicked: " ++ reason
       Nothing -> do
         msg <- readTChan clientMessage
         return $ do
             continue <- handleMessage serv client msg
             when continue $ session

 race :: IO a -> IO b -> IO (Either a b)
 race io_a io_b =
   withAsync io_a $ \a ->
   withAsync io_b $ \b ->
     waitEither a b	

 withAsync :: IO a -> (Async a -> IO b) -> IO b
 withAsync io operation = bracket (async io) cancel operation

 async :: IO a -> IO (Async a)
 async action = do
   var <- newEmptyTMVarIO
   t <- forkFinally action (atomically . putTMVar var)
   return (Async t var)
 
 cancel :: Async a -> IO ()
 cancel (Async t var) = throwTo t ThreadKilled

 waitEither :: Async a -> Async b -> IO (Either a b)
 waitEither a b = atomically $
   fmap Left (waitSTM a)
     `orElse`
   fmap Right (waitSTM b)

 waitSTM :: Async a -> STM a
 waitSTM a = do
   r <- waitCatchSTM a
   case r of
     Left e  -> throwSTM e
     Right a -> return a

 waitCatchSTM :: Async a -> STM (Either SomeException a)
 waitCatchSTM (Async _ var) = readTMVar var
 
 removeClient :: Session -> ClientName -> IO ()
 removeClient session@Session{..} name = atomically $ do
   modifyTVar' clients $ Map.delete name
   broadcast session $ Notice (name ++ " has disconnected")
		
 broadcast :: Session -> Message -> STM ()
 broadcast Session{..} msg = do
   clientmap <- readTVar clients
   mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)  
	
 handleMessage :: Session -> Client -> Message -> IO Bool
 handleMessage session client@Client{..} message =
   case message of
      Notice msg         -> allOutput msg
      Private recipient sender msg      -> privateOutput recipient sender msg  
      Broadcast sender msg -> chatOutput sender msg 
      Command msg ->
        case words msg of
            "/kick" : who : why -> do
                atomically $ checkAdmin session client who (unwords why)
                hPutStrLn clientHandle " "
                return True
            "/private" : who : what -> do
                atomically $ tell session client who $ Private who clientName (unwords what)
                hPutStrLn clientHandle " "
                return True
	    "/check" : who -> do
                atomically $ showUser session client (unwords who)
                hPutStrLn clientHandle " "
                return True
	    "/complaint" : what -> do
                atomically $ complaint session client (unwords what)
                hPutStrLn clientHandle " "
                return True   
            "/admin" : password -> do
                atomically $ checkPasswd (unwords password) client
                hPutStrLn clientHandle " "
                return True 
	    ["/all"] -> do
		hPutStrLn clientHandle "All users: "
		atomically $ showAllUsers session client
                hPutStrLn clientHandle " "
                return True   		 
            ["/help"] -> do
                hPutStrLn clientHandle "                   Short information:    "
		hPutStrLn clientHandle "/admin password        - If password is valid, you will become a moderator."
		hPutStrLn clientHandle "/all                   - Show all users."
		hPutStrLn clientHandle "/check name            - Check: is user 'name' in the network?"
		hPutStrLn clientHandle "/complaint what        - Send complaint to moderator."
		hPutStrLn clientHandle "/help                  - Shows information about the possible actions. "
		hPutStrLn clientHandle "/kick name reason      - Disconnects user 'name', specifying the reason, if you are a moderator."
                hPutStrLn clientHandle "/private name message  - Sends message to the user: 'name'" 
                hPutStrLn clientHandle "/quit                  - Disconnects the client"
		hPutStrLn clientHandle " "
                hPutStrLn clientHandle "message                - Sends message to all clients."
                hPutStrLn clientHandle " "	   
                return True
            ["/quit"] ->
                return False
            ('/':_):_ -> do
                atomically $ sendMessage client $ Notice $ "Unrecognised command: " ++ msg
                hPutStrLn clientHandle " "
                return True
            _ -> do
                atomically $ broadcast session $ Broadcast clientName msg
                return True
  where
    allOutput msg = do 
                            hPutStrLn clientHandle $ "<" ++ msg ++ ">"
                            hPutStrLn clientHandle " "
                            return True
    privateOutput recipient sender msg = if (sender == clientName)
                       then do 
                            hPutStrLn clientHandle $ "{" ++ "You to"  ++ recipient ++ "}: " ++ msg
                            return True	 
                       else do 
                            hPutStrLn clientHandle $ "{" ++ sender ++ "}: " ++ msg
                            return True	 					  
    chatOutput sender msg = if (sender == clientName)
                       then do 
                            hPutStrLn clientHandle $ "<You>: " ++ msg
                            return True
                       else do 
                            hPutStrLn clientHandle $ "<" ++ sender ++ ">: " ++ msg 
                            return True	

--atomically: STM a -> IO a STM(программная транзакционная память)-подход к решению проблем параллельной обработки
--unwords - осуществляет конкатенацию списка строк в одну строку, вставляя пробелы между отдельными строками из исходного списка		
