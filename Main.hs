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
import Prelude hiding (putStr)

path' = "/home/chemist/develop/haskell/radio/"

configure = "cabal configure"
build = "cabal build"
install = "cabal install"

type Command = String

data Step = Step 
  { command :: Command
  , output :: ByteString
  , error  :: ByteString
  } deriving (Show, Eq)
  
instance FromJSON Step where
    parseJSON (String x) = pure $ Step (unpack x) "" ""
    
instance ToJSON Step where
    toJSON x = String ((pack $ command x))
  
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

runStep :: Command -> ErrorT Step IO Step
runStep comm = do
    (_, stdout, stderr, handle) <- liftIO $ runInteractiveCommand comm
    out <- liftIO $ S.toList stdout
    err <- liftIO $ S.toList stderr
    exitCode <- liftIO $ waitForProcess handle
    let st = Step comm (msg out) (msg err)
    case exitCode of
         ExitSuccess -> return st
         _ ->           throwError st
    

msg :: [ByteString] -> ByteString
msg [] = ""
msg x = foldl1 (<>) x

main :: IO ()
main = do
    current <- getCurrentDirectory 
    setCurrentDirectory path'
    r <- runErrorT $ mapM runStep [configure, build]
    print r
    setCurrentDirectory current
    reloadApp
    
   

reloadApp :: IO ()
reloadApp = print "reload here"
    
initProject :: Repo -> ErrorT String IO Project
initProject (Git repo) = do
    current <- liftIO $ getCurrentDirectory
    tmp <- liftIO $ createTempDirectory current "gt"
    liftIO $ print tmp
    (_, stdout, stderr, handle) <- liftIO $ runInteractiveCommand $ "git clone " <> repo <> " " <> tmp
    out <- liftIO $ S.toList stdout
    err <- liftIO $ S.toList stderr
    exitCode <- liftIO $ waitForProcess handle
    liftIO $ print exitCode
    liftIO $ print out
    liftIO $ print err
    when (exitCode /= ExitSuccess) $ throwError $ "cant do git clone " <> repo
    return $ Project [] [] (Mail "" False) (Git repo) tmp
    
parseProject :: Project -> ErrorT String IO Project
parseProject pr = do
    maybeProject <- liftIO $ decodeFile $ (directory pr) <> "/.civ.yml" :: ErrorT String IO (Maybe Project)
    newProject <- maybe (throwError "cant parse project file") (nextStep pr) maybeProject
    return newProject
    where
    nextStep pr pr' = return $ Project (task pr') (launch pr') (notify pr') (cvs pr) (directory pr)
    
                                                           
    

