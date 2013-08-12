{-# LANGUAGE OverloadedStrings #-}
module Project where
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import qualified System.IO.Streams as S
import System.IO.Streams.Process
import Control.Applicative
import Data.ByteString (putStr, ByteString)
-- import Data.ByteString.Char8 (unpack)
import System.Exit
import Data.Monoid
import Control.Monad.Error
import Data.Yaml
import System.IO.Temp
import System.Directory
import Data.Text (pack, unpack)
import Control.Monad.Writer
import Data
import Prelude hiding (putStr)

path' = "/home/chemist/develop/haskell/radio/"

configure = "cabal configure"
build = "cabal build"
install = "cabal install"

main :: IO ()
main = do
    current <- getCurrentDirectory 
    (r, log) <- runCis $ do
        pr <- initProject (Git "git@github.com:chemist/cis.git" "cis")
        liftIO $ print pr
        liftIO $ setCurrentDirectory $ directory pr
        mapM runStep $ task pr
        mapM runStep $ launch pr
    print r
    putStr $ stdout log
    setCurrentDirectory current
    
runCis :: Cis a -> IO (Either String a, Log)
runCis = runWriterT . runErrorT

initProject :: Repo -> Cis Project
initProject (Git repo name) = do
    current <- liftIO $ getCurrentDirectory
    tmp <- liftIO $ createTempDirectory current "gt"
    liftIO $ print tmp
    (_, stdout, stderr, handle) <- liftIO $ runInteractiveCommand $ "git clone " <> repo <> " " <> tmp
    out <- liftIO $ S.toList stdout
    err <- liftIO $ S.toList stderr
    exitCode <- liftIO $ waitForProcess handle
    tell $ Log (msg out) (msg err)
    when (exitCode /= ExitSuccess) $ throwError $ "cant do git clone " <> repo
    parseProject (Git repo name) tmp
    
parseProject :: Repo -> FilePath -> Cis Project
parseProject r f = do
    maybeProject <- liftIO $ decodeFile $ f <> "/.cis.yml" :: Cis (Maybe Project)
    newProject <- maybe (throwError "cant parse project file") mergeProject maybeProject
    return newProject
    where
    mergeProject pr = return $ Project (task pr) (launch pr) (notify pr) r f

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

    
    
