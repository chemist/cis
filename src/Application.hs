{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Data.Lens.Common
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.AcidState
import Control.Monad.Reader (asks)
import Data.Typeable(Typeable)
import           Data.SafeCopy (base, deriveSafeCopy)
import Data.Map
import Snap
import Data

------------------------------------------------------------------------------
data CisState = CisState 
  { _mymap :: Map String Project
  } deriving (Show, Ord, Eq, Typeable)

makeLenses ''CisState

deriveSafeCopy 0 'base ''Step
deriveSafeCopy 0 'base ''Msg
deriveSafeCopy 0 'base ''Repo
deriveSafeCopy 0 'base ''Project
deriveSafeCopy 0 'base ''CisState

addProject :: (String, Project) -> Update CisState ()
addProject (key, val) = modify (over mymap (insert key val))

getProjects :: Query CisState (Map String Project)
getProjects = asks _mymap

makeAcidic ''CisState ['addProject, 'getProjects]


data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _acid :: Snaplet (Acid CisState)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist
    
instance HasAcid App CisState where
    getAcidStore  = view (acid.snapletValue)
    
------------------------------------------------------------------------------
type AppHandler = Handler App App 


