{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import qualified System.IO.Streams as S
import System.IO.Streams.Process
import Control.Applicative
import Data.ByteString (putStr, ByteString)
-- import Data.ByteString.Char8 (unpack)
import System.Exit
import Data.Monoid
import Data.EitherR
import Control.Monad.Error
import Data.Yaml
import System.IO.Temp
import System.Directory
import Data.Text (pack, unpack)
import Control.Monad.Writer
import Prelude hiding (putStr)

path' = "/home/chemist/develop/haskell/radio/"

configure = "cabal configure"
build = "cabal build"
install = "cabal install"

data Log = Log 
  { stdout :: ByteString
  , stderr :: ByteString
  } deriving (Show, Eq)
  
instance Monoid Log where
    mempty = Log mempty mempty
    mappend (Log x y) (Log x1 y1) = Log (x <> x1) (y <> y1)

type Cis = ErrorT String (WriterT Log IO)
    
runCis :: Cis a -> IO (Either String a, Log)
runCis = runWriterT . runErrorT

type Command = String

newtype Step = Step Command deriving (Show, Eq)
  
instance FromJSON Step where
    parseJSON (String x) = pure $ Step (unpack x) 
    
instance ToJSON Step where
    toJSON (Step x) = String ((pack x))
  
type Task = [Step]
type Version = String

data Msg = Mail String Bool deriving (Show, Eq)

instance FromJSON Msg where
    parseJSON (Object x) = Mail <$> x .: "email" <*> x .: "status"
    
instance ToJSON Msg where
    toJSON (Mail s b) = object [ "email" .= s, "status" .= b ]

data Repo = Git String 
          | Nil deriving (Show, Eq)

data Project = Project
 { task    :: Task
 , launch  :: Task
 , notify  :: Msg
 , cvs     :: Repo
 , directory :: String
 } deriving (Show, Eq)
 
instance ToJSON Project where
    toJSON x = object [ "before_install" .= (task x), "after" .= (launch x), "notification" .= (notify x) ]
    
instance FromJSON Project where
    parseJSON (Object x) = Project <$> x .: "before_install" <*> x .: "after" <*> x .: "notification" <*> pure Nil <*> pure ""

instance Error Step 

runStep :: Step -> Cis ()
runStep (Step comm) = do
    (_, stdout, stderr, handle) <- liftIO $ runInteractiveCommand comm
    out <- liftIO $ S.toList stdout
    err <- liftIO $ S.toList stderr
    exitCode <- liftIO $ waitForProcess handle
    tell $ Log (msg out) (msg err)
    let st = Step comm 
    case exitCode of
         ExitSuccess -> return ()
         _ ->           throwError $ "error in " <> comm
    

msg :: [ByteString] -> ByteString
msg [] = ""
msg x = foldl1 (<>) x

main :: IO ()
main = do
    current <- getCurrentDirectory 
    r <- runCis $ do
        pr <- initProject (Git "git@github.com:chemist/cis.git")
        liftIO $ setCurrentDirectory $ directory pr
        mapM runStep $ task pr
    print r
    setCurrentDirectory current
    
initProject :: Repo -> Cis Project
initProject (Git repo) = do
    current <- liftIO $ getCurrentDirectory
    tmp <- liftIO $ createTempDirectory current "gt"
    liftIO $ print tmp
    (_, stdout, stderr, handle) <- liftIO $ runInteractiveCommand $ "git clone " <> repo <> " " <> tmp
    out <- liftIO $ S.toList stdout
    err <- liftIO $ S.toList stderr
    exitCode <- liftIO $ waitForProcess handle
    tell $ Log (msg out) (msg err)
    when (exitCode /= ExitSuccess) $ throwError $ "cant do git clone " <> repo
    parseProject (Git repo) tmp
    
parseProject :: Repo -> FilePath -> Cis Project
parseProject r f = do
    maybeProject <- liftIO $ decodeFile $ f <> "/.cis.yml" :: Cis (Maybe Project)
    newProject <- maybe (throwError "cant parse project file") mergeProject maybeProject
    return newProject
    where
    mergeProject pr = return $ Project (task pr) (launch pr) (notify pr) r f
    
                                                           
    

