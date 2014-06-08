module Main ( main ) where

import ReceiveIn                 ( app, maxWaitMilliSecs )
import Web.Scotty                ( scottyOpts, Options(..))
import Network.Wai.Handler.Warp  ( setTimeout, setPort, defaultSettings )

main :: IO ()
main = scottyOpts delayedServeInOpts $ app
  where
    delayedServeInOpts = Options 1 $ -- Verbose
        -- Leave a 5 second margin for Scotty to respond after max-timeout
        setTimeout (maxWaitMilliSecs `div` 1000 + 5) $
        setPort    5000 $
        defaultSettings

