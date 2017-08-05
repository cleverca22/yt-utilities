#!/usr/bin/env stack
-- stack runghc --package optparse-simple --package shell-conduit --package transformers --package time --package extra --package text-icu --package unordered-containers --package hashable --package aeson --package http-client --package http-client-tls

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}

import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString            as BS
import           Data.Conduit.Shell
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Text.IO               as T
import           Data.Time                  (Day, UTCTime (utctDay), defaultTimeLocale,
                                             fromGregorian, parseTimeM)
import qualified Network.HTTP.Client.TLS    as H
import           Options.Applicative.Simple (Parser, auto, eitherReader, empty, flag',
                                             help, long, metavar, option, optional,
                                             showDefault, simpleOptions, strOption,
                                             switch, switch, value, (<|>))
import           System.Exit                (die)
import           System.FilePath.Posix      ((</>))
import           System.IO                  (hSetEncoding, stderr, stdin, stdout, utf8)

import           Parsing                    (Duration, parseOrg)
import           PreProcess                 (DurationMap, IssueId, preProcess)
import           Process                    (deleteWorkItem, filterProcess,
                                             formatCsvFilterProcess, importWorkItems)

main :: IO ()
main = do
    mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
    (options, ()) <-
        simpleOptions
        "0.1"
        "Export custom .org file time-tracking data to YT."
        ""
        optionsParser
        empty
    run $ deploymentScript options

deploymentScript :: Options -> Segment ()
deploymentScript Options{..} = do
    let since = fromMaybe (fromGregorian 1990 1 1) sinceDay
    m <- preProcess since <$> (liftIO $ parseOrg =<< decodeUtf8 <$> BS.readFile orgFile)
    manager <- liftIO $ H.newTlsManager
    m' <- liftIO $ filterProcess manager authToken userName m
    echo $ "For import to YT: " <> show m'

    let onRealRun (issueId, (wids, durMap)) = do
            echo $ "Processing issue " <> issueId
            flip mapM_ wids $ \wid -> do
                echo $ "Deleting work item #" <> wid
                deleteWorkItem manager authToken issueId wid
            echo $ "Importing work items for issue " <> issueId
            importWorkItems manager authToken userName issueId durMap

    let onDryRun = liftIO $ do
            putStrLn "DRY RUN OUTPUT FOLLOWS"
            putStrLn ""
            T.putStr $ formatCsvFilterProcess m'

    if not dryRun
        then flip mapM_ (HM.toList m') onRealRun
        else onDryRun

-- | CLI-options for deployer.
data Options = Options
    { orgFile   :: String
    , authToken :: String
    , userName  :: String
    , sinceDay  :: Maybe Day
    , dryRun    :: Bool
    }

optionsParser :: Parser Options
optionsParser =
    Options <$>
    strOption (long "org" <> metavar "FILE") <*>
    strOption (long "token" <> metavar "AUTH_TOKEN") <*>
    strOption (long "user" <> metavar "USERNAME") <*>
    optional
        (option (eitherReader parseDate)
            (long "since" <> metavar "YYYY-MM-DD" <>
             help "The first day timestamps will be considered valid on")) <*>
    switch (long "dry-run" <> help "Do not export to YT, print info to stdout instead")
  where
    parseDate x = utctDay <$> parseTimeM True defaultTimeLocale "%F" x
