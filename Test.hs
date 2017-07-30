#!/usr/bin/env stack
-- stack runghc --package optparse-simple --package shell-conduit --package transformers --package time --package extra --package text-icu --package unordered-containers --package hashable --package aeson --package http-client --package http-client-tls

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy       as L
import           Data.Conduit.Shell
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text.IO               as T
import qualified Network.HTTP.Client.TLS    as H
import           Options.Applicative.Simple (Parser, auto, empty, flag', help, long,
                                             metavar, option, optional, showDefault,
                                             simpleOptions, strOption, switch, value,
                                             (<|>))
import           System.Exit                (die)
import           System.FilePath.Posix      ((</>))

import           Parsing                    (Duration, parseOrg)
import           PreProcess                 (DurationMap, IssueId, preProcess)
import           Process                    (filterProcess)

main :: IO ()
main = do
    (options, ()) <- simpleOptions "0.1"
                                   "Export .org file to YT."
                                   ""
                                   optionsParser
                                   empty
    run $ deploymentScript options

deploymentScript :: Options -> Segment ()
deploymentScript Options{..} = do
    m <- preProcess <$> (liftIO $ parseOrg =<< T.readFile orgFile)
    echo $ show m
    manager <- liftIO $ H.newTlsManager
    m' <- liftIO $ filterProcess manager userName m
    echo $ show m'


-- | CLI-options for deployer.
data Options = Options
    { --itIsProductionCluster :: Bool
      orgFile  :: String
    , userName :: String
    --, numberOfNodes         :: Int
    --, noBuild               :: Bool
    }

optionsParser :: Parser Options
optionsParser = Options <$>
    -- ( flag' True  (long "prod")
    --   <|> flag' False (long "dev") )
    -- <*>
      strOption (
         long       "org"
      <> metavar    "FILE" )
    <*>
      strOption (
         long       "user"
      <> metavar    "USERNAME" )
    -- <*>
    --   option auto (
    --      long       "nodes"
    --   <> metavar    "NUMBER"
    --   <> help       "Number of nodes in a cluster, from 1 to 100." )
    -- <*>
    --   switch (
    --      long       "no-build"
    --   <> help       "Don't build cluster, assume it's built" )
