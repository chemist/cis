module Web where

import Snap.Http.Server
import Snap.Core

main :: IO ()
main = quickHttpServe web 

web :: Snap ()
web = return ()
