#!/usr/bin/env stack
-- stack runghc --package optparse-simple --package shell-conduit --package transformers --package time --package extra --package text-icu --package unordered-containers --package hashable --package aeson --package http-client --package http-client-tls

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy       as L
import           Data.Conduit.Shell
import qualified Data.HashMap.Strict        as HM
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
import           Process                    (deleteWorkItem, filterProcess,
                                             importWorkItems)

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
    manager <- liftIO $ H.newTlsManager
    m' <- liftIO $ filterProcess manager authToken userName m
    liftIO $ putStrLn $ "For import to YT: " <> show m'
    flip mapM_ (HM.toList m') $ \(issueId, (wids, durMap)) -> do
        echo $ "Processing issue " <> issueId
        flip mapM_ wids $ \wid -> do
            echo $ "Deleting work item #" <> wid
            liftIO $ deleteWorkItem manager authToken issueId wid
        echo $ "Importing work items for issue " <> issueId
        liftIO $ importWorkItems manager authToken userName issueId durMap


-- | CLI-options for deployer.
data Options = Options
    { orgFile   :: String
    , authToken :: String
    , userName  :: String
    }

optionsParser :: Parser Options
optionsParser = Options <$>
      strOption (
         long       "org"
      <> metavar    "FILE" )
    <*>
      strOption (
         long       "token"
      <> metavar    "AUTH_TOKEN" )
    <*>
      strOption (
         long       "user"
      <> metavar    "USERNAME" )
