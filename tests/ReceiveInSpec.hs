{-# LANGUAGE OverloadedStrings #-}

module ReceiveInSpec ( spec ) where

import Test.Hspec
import ReceiveIn              ( app      )
import Control.Monad.IO.Class ( liftIO   )
import Control.DeepSeq        ( deepseq  )
import Control.Exception      ( throwIO  )
import Control.Monad          ( unless   )
import Data.Monoid            ( (<>)     )

import qualified Data.ByteString            as BS
import qualified Network.HTTP.Types         as HT
import qualified Network.Wai                as W
import qualified Network.Wai.Test           as WT
import qualified Web.Scotty                 as Scotty

getApp :: IO W.Application
getApp = Scotty.scottyApp app

assertFailure :: String -> WT.Session ()
assertFailure msg = msg `deepseq` liftIO (throwIO (WT.WaiTestFailure msg))

assertAllReasonableMethods :: HT.ResponseHeaders -> WT.Session ()
assertAllReasonableMethods headers = do
    let headerName = "Access-Control-Allow-Methods"
    case lookup headerName headers of
        Nothing -> assertFailure $ "No header: " <> show headerName
        Just val -> do
            let allowed method = method `BS.isInfixOf` val
            let allMethodsAllowed =
                    allowed "GET"    && allowed "POST"  &&
                    allowed "DELETE" && allowed "PUT"   &&
                    allowed "HEAD"   && allowed "PATCH"
            unless allMethodsAllowed $
                assertFailure "Not all methods were allowed."

getDefaultOptionReq :: W.Request
getDefaultOptionReq =
    flip WT.setRawPathInfo "/" $
      WT.defaultRequest
          { W.requestMethod = HT.renderStdMethod HT.OPTIONS
          }

spec :: Spec
spec = do
    describe "redirectCall" $ do
        it "should preserve the url passed" $ do
            receiveInApp <- getApp
            flip WT.runSession receiveInApp $ do
                let url = "http://google.com/?q=something"
                let req = WT.setRawPathInfo WT.defaultRequest ("/0/" <> url)
                res <- WT.srequest (WT.SRequest req "")
                WT.assertHeader "Location" url res


    describe "preflight OPTIONS" $ do
        it "should return Access-Control-Allow-Origin as '*'" $ do
            receiveInApp <- getApp
            flip WT.runSession receiveInApp $ do
                let req = getDefaultOptionReq
                res <- WT.srequest (WT.SRequest req "")
                WT.assertHeader "Access-Control-Allow-Origin" "*" res

        it "should return all request forats in Access-Control-Allow-Methods" $ do
            receiveInApp <- getApp
            flip WT.runSession receiveInApp $ do
                let req = getDefaultOptionReq
                res <- WT.srequest (WT.SRequest req "")
                assertAllReasonableMethods (WT.simpleHeaders res)

        it "should behave the same if the request is sent for time-out path" $ do
            receiveInApp <- getApp
            flip WT.runSession receiveInApp $ do
                let req = WT.setRawPathInfo getDefaultOptionReq "/0/url?q=a"
                res <- WT.srequest (WT.SRequest req "")
                WT.assertHeader "Access-Control-Allow-Origin" "*" res
                assertAllReasonableMethods (WT.simpleHeaders res)

