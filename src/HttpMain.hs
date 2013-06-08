{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad (msum)
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Happstack.Lite

import Views as V

main :: IO ()
main = do
  putStrLn $ "HTTP server starting on port " ++ show p
  serve conf app
  where p = 3000
        conf = Just $ defaultServerConfig{port = p}

app :: ServerPart Response
app = msum [
  dir "static" static,
  home
  ]

home :: ServerPart Response
home = ok V.homePage

static :: ServerPart Response
static = serveDirectory DisableBrowsing [] "static"