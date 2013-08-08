{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import qualified System.IO.Streams as S
import System.IO.Streams.Process
import Control.Applicative
import Data.ByteString (putStr, ByteString)
import Data.ByteString.Char8 (unpack)
import System.Exit
import Data.Monoid
import Data.EitherR
import Control.Monad.Error
import Prelude hiding (putStr)

path = "/home/chemist/develop/haskell/radio/"

configure = "cabal configure"
build = "cabal build"
install = "cabal install"

type Command = String

data Step = Step 
  { command :: Command
  , output :: ByteString
  , error  :: ByteString
  } deriving (Show)
  
type Task = [Step]

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
    setCurrentDirectory path
    r <- runErrorT $ mapM runStep [configure, build]
    print r
    setCurrentDirectory current
    reloadApp
    
   

reloadApp :: IO ()
reloadApp = print "reload here"
    
    

