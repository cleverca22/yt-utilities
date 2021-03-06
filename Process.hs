{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Process
       ( deleteWorkItem
       , filterProcess
       , importWorkItems
       , sendMessageToSlack
       , IssueInfo (..)
       , getIssues
       ) where

import           Control.Exception          (Exception, throw)
import           Control.Monad              (foldM, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON (..), ToJSON (..), eitherDecode,
                                             encode, genericParseJSON, genericToJSON)
import qualified Data.Aeson                 as J
import           Data.Bifunctor             (bimap, second)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Time                  (Day, UTCTime (..))
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           GHC.Generics               (Generic)
import qualified Network.HTTP.Client        as H
import qualified Network.HTTP.Types         as H

import           AesonOptions               (defaultOptions)
import           Parsing                    (Duration, parseOrg)
import           PreProcess                 (DurationMap, IssueId, preProcess)
import           Util                       (safeHead)

instance ToJSON Author where
    toJSON = genericToJSON defaultOptions

instance ToJSON WorkItem where
    toJSON = genericToJSON defaultOptions

instance FromJSON Author where
    parseJSON = genericParseJSON defaultOptions

instance FromJSON WorkItem where
    parseJSON = genericParseJSON defaultOptions

data WorkItem = WorkItem
    { wiId          :: Maybe String
    , wiDate        :: Integer
    , wiDuration    :: Word
    , wiDescription :: Maybe Text
    , wiAuthor      :: Author
    } deriving (Show, Generic)

data Author = Author
    { aLogin :: String
    } deriving (Show, Generic)

ytTZShift :: Integer
ytTZShift = 3 * 60 * 60 * 1000

ytTimeToDay :: Integer -> Day
ytTimeToDay t = utctDay $ posixSecondsToUTCTime $ fromIntegral $ (t + ytTZShift) `div` 1000

dayToYtTime :: Day -> Integer
dayToYtTime day = secs * 1000
  where
    utc = UTCTime day 0
    secs = round $ utcTimeToPOSIXSeconds utc

processWIs :: [WorkItem] -> HM.HashMap UserName DurationMap'
processWIs = fmap (toMap . map toTriple) . foldr (\wi -> HM.alter (Just . (wi:) . fromMaybe []) $ aLogin (wiAuthor wi)) mempty
  where
    toTriple wi =
        ( ytTimeToDay (wiDate wi)
        , fromMaybe (error "no workitemId in response") $ wiId wi
        , fromMaybe "" (wiDescription wi)
        , wiDuration wi)

toMap :: [(Day, WorkitemId, Text, Duration)] -> DurationMap'
toMap = foldr insert mempty
  where
    insert (day, wid, desc, dur) = HM.alter insert1 day
      where
        insert1 (Just (wids, m)) = Just $ (wid:wids, HM.alter insert2 desc m)
        insert1 _                = Just $ ([wid], HM.singleton desc dur)
        insert2 (Just dur') = Just $ dur' + dur
        insert2 _           = Just dur

setHeaders :: String -> H.Request -> H.Request
setHeaders authToken r = r { H.requestHeaders = [ ("Authorization", "Bearer " <> fromString authToken)
          , ("Accept", "application/json")
          , ("Content-Type", "application/json")
          ] }

type UserName = String
type WorkitemId = String

data ProcessError = ParseError String
                  | DeleteWorkItemError String String
                  | ImportWorkItemsError String
                  | SlackSendMsgError String
  deriving (Generic, Show)

instance Exception ProcessError

type DurationMap' = HM.HashMap Day ([WorkitemId], HM.HashMap Text Duration)

ytWorkItems :: String
ytWorkItems = "/workitems"

ytTimeTracking :: String
ytTimeTracking = "/timetracking/workitem"

ytRestImportEndpoint :: String
ytRestImportEndpoint = "https://issues.serokell.io/rest/import/issue/"

ytRestEndpoint :: String
ytRestEndpoint = "https://issues.serokell.io/rest/issue/"

getIssueWIs :: H.Manager -> String -> IssueId -> IO (HM.HashMap UserName DurationMap')
getIssueWIs manager authToken (T.unpack -> issueId) = do
    request <- setHeaders authToken <$> (H.parseRequest $ ytRestEndpoint<>issueId<>ytTimeTracking)
    response <- H.httpLbs request manager
    case processWIs <$> eitherDecode (H.responseBody response) of
        Left err  -> throw $ ParseError err
        Right res -> return res

deleteWorkItem ::
       (MonadIO m) => H.Manager -> String -> IssueId -> WorkitemId -> m ()
deleteWorkItem manager authToken (T.unpack -> issueId) wId = liftIO $ do
    request <- setHeaders authToken <$> (H.parseRequest $ "DELETE " <> ytRestEndpoint
                  <> issueId <> ytTimeTracking <> "/" <> wId)
    response <- H.httpLbs request manager
    when (H.responseStatus response /= H.status200) $ do
        LBS8.putStrLn (H.responseBody response)
        throw $ DeleteWorkItemError issueId wId

sendMessageToSlack :: MonadIO m => H.Manager -> String -> String -> Text -> m ()
sendMessageToSlack manager apiToken channel msg = liftIO $ do
    request <- H.parseRequest $ "POST https://slack.com/api/chat.postMessage"
    let msgData =
            [ ("token", BS8.pack apiToken)
            , ("channel", BS8.pack channel)
            , ("text", T.encodeUtf8 msg)
            , ("as_user", "true")
            ]
        request' = (H.urlEncodedBody msgData request)
                      { H.requestHeaders =
                          [ ("Content-Type", "application/x-www-form-urlencoded") ]
                      }
    response <- H.httpLbs request' manager
    let body = H.responseBody response
    when (BS.null $ snd $ BS.breakSubstring "\"ok\":true" $ LBS.toStrict body) $ do
        LBS8.putStrLn body
        throw $ SlackSendMsgError channel

importWorkItems ::
       (MonadIO m)
    => H.Manager
    -> String
    -> UserName
    -> IssueId
    -> DurationMap
    -> m ()
importWorkItems manager authToken user (T.unpack -> issueId) durMap = liftIO $ do
    let encoded = encode $ flatDurMap user durMap
    putStr $ "Items for issue " <> issueId <> ": "
    LBS8.putStrLn encoded
    request <- setHeaders authToken <$>
                (H.parseRequest $ "PUT " <> ytRestImportEndpoint
                  <> issueId <> ytWorkItems)
    let request' = request { H.requestBody = H.RequestBodyLBS encoded }
    response <- H.httpLbs request' manager
    when (H.responseStatus response /= H.status200) $ do
        LBS8.putStrLn (H.responseBody response)
        throw $ ImportWorkItemsError issueId

flatDurMap user durMap = map flatToWorkItem flattened
  where
    flatToWorkItem (day, desc, dur) =
      WorkItem
        { wiId = Nothing
        , wiDate = dayToYtTime day
        , wiDuration = dur
        , wiDescription = if T.null desc then Nothing else Just desc
        , wiAuthor = Author user
        }
    flattened :: [(Day, Text, Duration)]
    flattened = concat $ map f $ HM.toList $ HM.toList <$> durMap
    f (a, ns) = map (\(k, v) -> (a, k, v)) ns

filterProcess ::
       (MonadIO m)
    => H.Manager
    -> String
    -> UserName
    -> HM.HashMap IssueId DurationMap
    -> m (HM.HashMap IssueId ([WorkitemId], DurationMap))
filterProcess manager authToken user =
    liftIO . foldM processDo mempty . HM.toList
  where
    processDo' ::
           DurationMap'
        -> Day
        -> HM.HashMap Text Duration
        -> ([WorkitemId], DurationMap)
        -> ([WorkitemId], DurationMap)
    processDo' ytDurMap day items (widsAcc, durMap) =
        case day `HM.lookup` ytDurMap of
            Just (ytWids, items') ->
                if items == items'
                    then (widsAcc, durMap)
                    else (widsAcc ++ ytWids, durMapUpdated)
            _ -> (widsAcc, durMapUpdated)
      where
        durMapUpdated = HM.insert day items durMap
    processDo accum (issueId, durMap) = do
        ytDurMap <-
            (fromMaybe mempty . HM.lookup user) <$>
            getIssueWIs manager authToken issueId
        putStrLn $
            "Fetched durations for issue " <> show issueId <> ": " <>
            show ytDurMap
        let (wids', durMap') =
                HM.foldrWithKey (processDo' ytDurMap) mempty durMap
        if HM.null durMap'
            then return accum
            else return $ HM.insert issueId (wids', durMap') accum

data YtIssueInfo = YtIssueInfo
    { iiField :: [IIField]
    }
    deriving (Show, Generic)

data IIField = IIField
    { iifName  :: Text
    , iifValue :: J.Value
    }
    deriving (Show, Generic)

instance ToJSON YtIssueInfo where
    toJSON = genericToJSON defaultOptions
instance FromJSON YtIssueInfo where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON IIField where
    toJSON = genericToJSON defaultOptions
instance FromJSON IIField where
    parseJSON = genericParseJSON defaultOptions

data IssueInfo = IssueInfo
      { iiIssueId :: IssueId
      , iiTitle   :: Text
      }
    deriving (Show, Generic)

processIssue :: YtIssueInfo -> IssueInfo
processIssue YtIssueInfo {..} = IssueInfo issueId' title
  where
    getField k = fromMaybe ("<no "<>k<>">") $
                   textValueF $
                   safeHead $
                   filter ((== k) . iifName) iiField
    textValueF (Just (iifValue -> J.String t)) = Just t
    textValueF _                               = Nothing
    issueId' = getField "projectShortName" <> "-" <>  getField "numberInProject"
    title = getField "summary"

getIssues :: MonadIO m => H.Manager -> String -> [IssueId] -> m (HM.HashMap IssueId IssueInfo)
getIssues manager authToken = liftIO . foldM action mempty
  where
    action acc issueId = do
        request <- setHeaders authToken <$> (H.parseRequest $ ytRestEndpoint<> T.unpack issueId)
        response <- H.httpLbs request manager
        case processIssue <$> eitherDecode (H.responseBody response) of
            Left err   -> throw $ ParseError err
            Right info -> return $ HM.insert issueId info acc
