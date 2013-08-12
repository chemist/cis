{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data where

import Data.Yaml
import Data.Monoid
import Control.Monad.Error
import Data.ByteString (putStr, ByteString)
import Control.Monad.Writer
import Control.Applicative
import Data.Text (pack, unpack)
import Data.Typeable(Typeable)
import           Data.SafeCopy (base, deriveSafeCopy)


data Log = Log 
  { stdout :: ByteString
  , stderr :: ByteString
  } deriving (Show, Eq)
  
instance Monoid Log where
    mempty = Log mempty mempty
    mappend (Log x y) (Log x1 y1) = Log (x <> x1) (y <> y1)

type Cis = ErrorT String (WriterT Log IO)

type Command = String

newtype Step = Step Command deriving (Show, Eq, Ord)
 
instance FromJSON Step where
    parseJSON (String x) = pure $ Step (unpack x) 
    
instance ToJSON Step where
    toJSON (Step x) = String ((pack x))
  
type Task = [Step]
type Version = String

data Msg = Mail String Bool deriving (Show, Eq, Ord)

instance FromJSON Msg where
    parseJSON (Object x) = Mail <$> x .: "email" <*> x .: "status"
    
instance ToJSON Msg where
    toJSON (Mail s b) = object [ "email" .= s, "status" .= b ]


data Repo = Git 
  { repo :: String 
  , name :: String
  }
          | Nil deriving (Show, Eq, Ord)

data Project = Project
 { task    :: Task
 , launch  :: Task
 , notify  :: Msg
 , cvs     :: Repo
 , directory :: String
 } deriving (Show, Eq, Ord, Typeable)
 
instance ToJSON Project where
    toJSON x = object [ "before_install" .= (task x), "after" .= (launch x), "notification" .= (notify x) ]
    
instance FromJSON Project where
    parseJSON (Object x) = Project <$> (x .: "before_install" <|> pure []) <*> x .: "after" <*> x .: "notification" <*> pure Nil <*> pure ""

instance Error Step 


