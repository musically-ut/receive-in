{-# LANGUAGE OverloadedStrings #-}
module ReceiveIn ( app, maxWaitMilliSecs ) where

import qualified Data.ByteString.Lazy as BL

import Control.Concurrent         ( threadDelay )
import Control.Monad.IO.Class     ( liftIO      )

import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as TE

import Data.Monoid               ( mconcat, (<>) )
import Network.HTTP.Types        ( status307, status204, StdMethod( OPTIONS ))

import Network.Wai               ( Request
                                 , rawQueryString
                                 )

import Web.Scotty                ( addroute, matchAny, html, notFound
                                 , param, params, regex, request, ScottyM
                                 , setHeader, status
                                 )


app :: ScottyM ()
app = do
    addroute OPTIONS "/" $ do
        status status204
        setHeader "Access-Control-Allow-Origin" "*"
        setHeader "Access-Control-Allow-Methods"
                  "GET, POST, PATCH, DELETE, PUT, CONNECT, OPTIONS, HEAD, TRACE"
        -- TODO (UU): Enable this for efficiency in this inefficiency
        -- introducing server. Heh.
        --
        -- Cache for 20 days
        -- setHeader "Access-Control-Max-Age" "1728000"

    -- /:timeoutMillisecs/:url
    matchAny (regex "^/([0-9]+)/(.*)$") $ do
        rawTimeoutMillisecs <- param "1"
        rawUrl              <- param "2"
        req                 <- request

        let mbQueryString = TE.decodeUtf8' . BL.fromStrict . rawQueryString $ req
        case mbQueryString of
          Left err -> html $ mconcat [ "<pre> While parsing URL:"
                                     , T.pack . show $ err
                                     , "</pre>"
                                     ]
          Right qs -> do
              let timeoutMillisecs = min maxWaitMilliSecs rawTimeoutMillisecs
                  url         = rawUrl <> qs
              liftIO $ putStrLn $ mconcat
                  [ "Will redirect to : `" , T.unpack url, "'"
                  , " after " , show timeoutMillisecs , " milliseconds."
                  ]

              liftIO $ threadDelay (timeoutMillisecs * 1000)
              status status307
              -- TODO (UU): Should append the Origin or Referrer to the path if
              -- the URL doesn't start with `http(s)?`. If working with
              -- Referrer, then be strip out the path if the URL expected to
              -- access root.
              setHeader "Location" url
              setHeader "Access-Control-Allow-Origin" "*"

    notFound $ do
        passedParams <- params
        html $ mconcat $
            ("The URI was mal-formed. The following params were passed: \n"):
            ("<table>\n"):
            [ mconcat $
                [ "\t<tr><td> ", name , " </td>"
                , "<td> "      , value, " </td></tr>\n"
                ] | (name, value) <- passedParams
            ] ++
            [ "</table>" ]

-- The maximum time `threadDelay` can wait is 15 minutes
maxWaitMilliSecs :: Int
maxWaitMilliSecs = 15 * 60 * 1000

