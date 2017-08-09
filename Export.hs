#!/usr/bin/env stack
-- stack runghc --package optparse-simple --package shell-conduit --package transformers --package time --package extra --package text-icu --package unordered-containers --package hashable --package aeson --package http-client --package http-client-tls --package boxes --package microlens

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}

import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString            as BS
import           Data.Conduit.Shell         hiding (view)
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.List                  (sort, sortOn)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Text.IO               as T
import           Data.Time                  (Day, UTCTime (utctDay), defaultTimeLocale,
                                             formatTime, fromGregorian, parseTimeM)
import           Lens.Micro                 (to, _1, _2, _3, _4)
import           Lens.Micro.Extras          (view)
import qualified Network.HTTP.Client.TLS    as H
import           Numeric                    (showFFloat)
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
import           Process                    (IssueInfo (..), deleteWorkItem,
                                             filterProcess, getIssues, importWorkItems,
                                             sendMessageToSlack)

slackChannel :: String
slackChannel = "team-updates"

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
    let runFilter = liftIO $ do
            m' <- filterProcess manager authToken userName m
            T.putStrLn $ formatExportMapByDate mempty (snd <$> m')
            return m'

    case runMode of
        Local ->
            liftIO $ T.putStrLn $ "Local data: \n" <> formatExportMap False mempty m
        Local2YT -> do
            issueInfos <- getIssues manager authToken $ HM.keys m
            liftIO $ T.putStrLn $ "Local data: \n" <> formatExportMapByDate issueInfos m
        Dry -> runFilter >> echo ("Dry run, no export to YT":: Text)
        Export -> do
            m' <- runFilter
            echo ("Exporting to YT:" :: Text)
            flip mapM_ (HM.toList m') onRealRun
            case slackToken of
                Just apiToken -> do
                    echo ("Retrieving info for issues..." :: Text)
                    issueInfos <- getIssues manager authToken $ HM.keys m'
                    let ytPrinted = formatExportMapByDate issueInfos (snd <$> m')
                    echo ("Posting to slack..." :: Text)
                    sendMessageToSlack manager apiToken slackChannel ytPrinted
                _ -> return ()


formatExportMapByDate :: HM.HashMap IssueId IssueInfo -> HM.HashMap IssueId DurationMap -> Text
formatExportMapByDate issueInfos hm = T.intercalate "\n" $ map print' days
  where
    days = sort $ HS.toList $ foldr (\dm -> HS.union $ HS.fromList $ HM.keys dm) mempty hm
    filtered day = (HM.filterWithKey $ \k _ -> k == day) <$> hm
    print' day = T.pack (formatTime defaultTimeLocale "%b %d:\n" day)
                    <> formatExportMap True issueInfos (filtered day)

-- | Formats result of 'filterProcess' as csv to be displayed in dry
-- run mode.
formatExportMap :: Bool -> HM.HashMap IssueId IssueInfo -> HM.HashMap IssueId DurationMap -> Text
formatExportMap ytExport issueInfos hm =
    T.intercalate "\n" $
    if ytExport
      then [ prettyTable ]
      else
        [ prettyTable
        , T.pack (replicate longestTableLine '-')
        , "Total time:"
        , T.pack (formatDuration totalDuration)
        , T.pack (showFFloat (Just 2) (fromIntegral totalDuration / 60) "") <> "h"
        ]
  where
    longestTableLine = if T.null prettyTable then 0 else maximum (map T.length $ T.lines prettyTable)
    prettyTable = T.pack $ B.render $
         B.hsep 2 B.left $
         map (B.vcat B.left)
         tableContents

    tableContents
      | ytExport =
         [ replicate (length allThings) " *"
         , map (B.text . view _1) allThings
         , map (B.text . view _4) allThings
         ]
      | otherwise =
         [ map (B.text . view _1) allThings
         , map (B.text . view _2) allThings
         , map (B.text . view _3) allThings
         , map (B.text . view _4) allThings
         ]
    totalDuration :: Duration
    totalDuration = sum $ map (sum . map (sum . HM.elems) . HM.elems) $ HM.elems hm
    allThings :: [(String,String,String,String)]
    allThings = sortF $ concatMap dumpIssue $ HM.toList hm
    sortF =
        sortOn (view _2) .
        sortOn (view _1) .
        sortOn (view _4)
    dumpIssue :: (IssueId, DurationMap) -> [(String,String,String,String)]
    dumpIssue (issueId, durMap) =
        map
        (\(a,b,c) -> (issueId',a,b,c))
        (dumpDurMap issuePrefix durMap)
      where
        issueId' = T.unpack $
            case issueId `HM.lookup` issueInfos of
                Just (IssueInfo {..}) -> iiIssueId
                _                     -> issueId
        issuePrefix =
            case issueId `HM.lookup` issueInfos of
                Just (IssueInfo {..}) -> T.unpack iiTitle <> ": "
                _                     -> ""
    dumpDurMap :: String -> DurationMap -> [(String,String,String)]
    dumpDurMap issuePrefix =
        concatMap (\(d, tdmap) -> map (\(a,b) -> (show d, a,b)) (dumpTdMap issuePrefix tdmap)) .
        HM.toList
    dumpTdMap :: String -> HM.HashMap Text Duration -> [(String,String)]
    dumpTdMap issuePrefix =
        map (\(t,dur) -> (formatDuration dur, issuePrefix <> T.unpack t)) .
        HM.toList

data Mode = Dry | Export | Local | Local2YT
  deriving (Show, Read)

-- | CLI-options for deployer.
data Options = Options
    { orgFile    :: String
    , authToken  :: String
    , userName   :: String
    , sinceDay   :: Maybe Day
    , runMode    :: Mode
    , slackToken :: Maybe String
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
    --switch (long "dry-run" <> help "Do not export to YT, print info to stdout instead")
    option auto (long "mode" <> help "Mode: Dry | Export | Local | Local2YT" <> value Dry) <*>
    optional (strOption $ long "slack-token" <> metavar "SLACK_TOKEN")
  where
    parseDate x = utctDay <$> parseTimeM True defaultTimeLocale "%F" x
