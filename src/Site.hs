{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
 where

------------------------------------------------------------------------------
import           Control.Applicative hiding (empty)
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Maybe
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.AcidState
import           Snap.Util.FileServe
import           Heist
import Data.Map (empty)
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import Data
import Project
import Control.Monad.IO.Class
import Control.Monad


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

withAuth :: Handler App App () -> Handler App App ()
withAuth = requireUser auth $ redirect "/"

handleProject :: Handler App App ()
handleProject = method GET getH <|> method POST putH
    where getH = writeText . T.pack . show =<< query GetProjects
          putH = do
              name' <- getParam "name"
              repo' <- getParam "repo"
              when (isNothing name') $ writeText "check param" >> mzero
              when (isNothing repo') $ writeText "check param" >> mzero
              pr <- liftIO $ runCis $ initProject $ Git (unpack $ fromJust repo') (unpack $ fromJust name')
              case pr of
                   (Left e,_) -> do
                       writeText "error"
                       writeText $ T.pack e
                   (Right p,_) -> do
                       update (AddProject (name . cvs $ p, p))
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/project", withAuth handleProject)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    ac <- nestSnaplet "acid" acid $ 
           acidInit (CisState empty)
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a ac

