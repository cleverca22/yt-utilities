{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Process where

import           Control.Exception         (Exception, throw)
import           Control.Monad             (foldM)
import           Data.Aeson                (FromJSON (..), eitherDecode, genericParseJSON)
import           Data.Bifunctor            (bimap, second)
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time                 (Day, utctDay)
import           Data.Time.Clock.POSIX     (getPOSIXTime, posixSecondsToUTCTime)
import           GHC.Generics              (Generic)
import qualified Network.HTTP.Client       as H
import qualified Network.HTTP.Types.Header as H

import           AesonOptions              (defaultOptions)
import           Parsing                   (Duration, parseOrg)
import           PreProcess                (DurationMap, IssueId, preProcess)

instance FromJSON Author where
    parseJSON = genericParseJSON defaultOptions

instance FromJSON WorkItem where
    parseJSON = genericParseJSON defaultOptions

data WorkItem =
  WorkItem
    { wiId          :: String
    , wiDate        :: Integer
    , wiDuration    :: Word
    , wiDescription :: Maybe Text
    , wiAuthor      :: Author
    }
  deriving (Show, Generic)
data Author =
  Author
    { aLogin :: String }
  deriving (Show, Generic)

ytTZShift :: Integer
ytTZShift = 3 * 60 * 60 * 1000

processWIs :: [WorkItem] -> HM.HashMap UserName DurationMap'
processWIs = fmap (toMap . map toTriple) . foldr (\wi -> HM.alter (Just . (wi:) . fromMaybe []) $ aLogin (wiAuthor wi)) mempty
  where
    toTriple wi = (toDay (wiDate wi), wiId wi, fromMaybe "" (wiDescription wi), wiDuration wi)
    toDay t = utctDay $ posixSecondsToUTCTime $ fromIntegral $ (t + ytTZShift) `div` 1000

toMap :: [(Day, WorkitemId, Text, Duration)] -> DurationMap'
toMap = foldr insert mempty
  where
    insert (day, wid, desc, dur) = HM.alter insert1 day
      where
        insert1 (Just (wids, m)) = Just $ (wid:wids, HM.alter insert2 desc m)
        insert1 _                = Just $ ([wid], HM.singleton desc dur)
        insert2 (Just dur') = Just $ dur' + dur
        insert2 _           = Just dur

headers :: [H.Header]
headers = [ ("Authorization", "Bearer perm:Z2VvcmdlZWU=.VGVzdDE=.nPgExkZGKX4qNkr2RxdHHlmVNRxeKu")
          , ("Accept", "application/json")
          ]

type UserName = String
type WorkitemId = String

data ProcessError = ParseError String
  deriving (Generic, Show)

instance Exception ProcessError

type DurationMap' = HM.HashMap Day ([WorkitemId], HM.HashMap Text Duration)

getIssueWIs :: H.Manager -> IssueId -> IO (HM.HashMap UserName DurationMap')
getIssueWIs manager (T.unpack -> issueId) = do
    request <- H.parseRequest $ "https://issues.serokell.io/rest/issue/"<>issueId<>"/timetracking/workitem"
    let request' = request
          { H.requestHeaders = headers }
    response <- H.httpLbs request' manager
    case processWIs <$> eitherDecode (H.responseBody response) of
        Left err  -> throw $ ParseError err
        Right res -> return res

filterProcess :: H.Manager -> UserName -> HM.HashMap IssueId DurationMap -> IO ([WorkitemId], HM.HashMap IssueId DurationMap)
filterProcess manager user = foldM processDo mempty . HM.toList
  where
    processDo' :: DurationMap' -> Day -> HM.HashMap Text Duration -> ([WorkitemId], DurationMap) -> ([WorkitemId], DurationMap)
    processDo' ytDurMap day items (widsAcc, durMap) =
        case day `HM.lookup` ytDurMap of
            Just (ytWids, items') ->
                if items == items'
                  then (widsAcc, durMap)
                  else (widsAcc ++ ytWids, durMapUpdated)
            _ -> (widsAcc, durMapUpdated)
      where
        durMapUpdated = HM.insert day items durMap
    processDo (wids, accum) (issueId, durMap) = do
        ytDurMap <- (fromMaybe mempty . HM.lookup user) <$> getIssueWIs manager issueId
        putStrLn $ "Fetched durations for issue " <> show issueId <> ": " <> show ytDurMap
        let (wids', durMap') = HM.foldrWithKey (processDo' ytDurMap) mempty durMap
        return ( wids ++ wids'
               , if HM.null durMap'
                   then accum
                   else HM.insert issueId durMap' accum
               )

