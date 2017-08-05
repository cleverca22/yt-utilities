#!/usr/bin/env stack
-- stack runghc --package optparse-simple --package shell-conduit --package transformers --package time --package extra --package text-icu --package unordered-containers --package hashable --package aeson --package http-client --package http-client-tls --package boxes

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
import qualified Data.Text                  as T
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
import qualified Text.PrettyPrint.Boxes     as B

import           Parsing                    (Duration, formatDuration, parseOrg)
import           PreProcess                 (DurationMap, IssueId, preProcess)
import           Process                    (deleteWorkItem, filterProcess,
                                             importWorkItems)

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
            T.putStrLn $ formatDryRunProcess m

    if dryRun then onDryRun else do
        m' <- liftIO $ filterProcess manager authToken userName m
        echo $ "For import to YT: " <> show m'
        flip mapM_ (HM.toList m') onRealRun

-- | Formats result of 'filterProcess' as csv to be displayed in dry
-- run mode.
formatDryRunProcess :: HM.HashMap IssueId DurationMap -> Text
formatDryRunProcess hm =
    T.intercalate "\n" $
    [ prettyTable
    , T.pack (replicate longestTableLine '-')
    , "Total time: " <> T.pack (formatDuration totalDuration) <>
      ", hours: " <> T.pack (show $ fromIntegral totalDuration / 60) <> "h"
    ]
  where
    longestTableLine = maximum (map T.length $ T.lines prettyTable)
    prettyTable = T.pack $ B.render $
         B.hsep 2 B.left $
         map (B.vcat B.left)
         [ map (\(a,_,_,_) -> B.text a) allThings
         , map (\(_,a,_,_) -> B.text a) allThings
         , map (\(_,_,a,_) -> B.text a) allThings
         , map (\(_,_,_,a) -> B.text a) allThings
         ]

    totalDuration :: Duration
    totalDuration = sum $ map (sum . map (sum . HM.elems) . HM.elems) $ HM.elems hm
    allThings :: [(String,String,String,String)]
    allThings = concatMap dumpIssue $ HM.toList hm
    dumpIssue :: (IssueId, DurationMap) -> [(String,String,String,String)]
    dumpIssue (issueId, durMap) =
        map
        (\(a,b,c) -> (T.unpack issueId,a,b,c))
        (dumpDurMap durMap)
    dumpDurMap :: DurationMap -> [(String,String,String)]
    dumpDurMap =
        concatMap (\(d, tdmap) -> map (\(a,b) -> (show d, a,b)) (dumpTdMap tdmap)) .
        HM.toList
    dumpTdMap :: HM.HashMap Text Duration -> [(String,String)]
    dumpTdMap =
        map (\(t,dur) -> (formatDuration dur, T.unpack t)) .
        HM.toList

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
