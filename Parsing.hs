{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Parsing
  ( parseOrg
  , IssueRecordMap
  , TimeRecord (..)
  , Duration
  , ytParsingCtxTag
  ) where

import           Control.Monad       (foldM)
import           Data.Bifunctor      (bimap, second)
import           Data.Char           (isSpace)
import qualified Data.HashMap.Strict as HM
import           Data.List.Extra     (replace)
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.ICU.Regex as R
import           Data.Time           (Day, UTCTime, defaultTimeLocale, parseTimeM)
import           Text.Read           (readMaybe)

type Duration = Word

data TimeRecord = ClockRecord Text UTCTime UTCTime
                | TrackRecord Text Day Duration
  deriving Show

ytParsingCtxTag :: Text
ytParsingCtxTag = "{yt_timetracking}"

type IssueRecordMap = HM.HashMap Text [TimeRecord]

data ParsingCtx =
     ParsingCtx
      { pcYTSection   :: Maybe Int
      , pcIssueId     :: Maybe (Int, Text)
      , pcDescription :: Maybe (Int, Text)
      , pcResult      :: IssueRecordMap
      , issueRegex    :: R.Regex
      , clockRegex    :: R.Regex
      , trackRegex    :: R.Regex
      }

timeFormats :: [String]
timeFormats =
  [ "%F %T"
  , "%F %R"
  ]

dayFormats :: [String]
dayFormats =
  [ "%F"
  ]

dayOfWeek :: [String]
dayOfWeek =
  [ "Mon", "Monday"
  , "Tue", "Tuesday"
  , "Wed", "Wednesday"
  , "Thu", "Thursday"
  , "Fri", "Friday"
  , "Sat", "Saturday"
  , "Sun", "Sunday"
  ]

parseDurationH :: Text -> Maybe Duration
parseDurationH (convertDurPair -> (h, m)) = f <$> h <*> m
  where
    f h m = h * 60 + m

convertDurPair :: Text -> (Maybe Duration, Maybe Duration)
convertDurPair = bimap read' read' .
            second (T.dropWhile (== ':')) .
            T.span (/= ':') .
            T.strip
  where
    read' = readMaybe . T.unpack


parseDayH :: Text -> Maybe Day
parseDayH (T.unpack -> time) = safeHead $ catMaybes $
    map (\f -> parseTimeM True defaultTimeLocale f time) dayFormats

parseTimeH :: Text -> Maybe UTCTime
parseTimeH (T.unpack -> time) = safeHead $ catMaybes $
    map (\f -> parseTimeM True defaultTimeLocale f time') timeFormats
  where
    time' = foldr (flip replace "") time dayOfWeek

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

issueRegexpText :: Text
issueRegexpText = "(?:\\W|^)((?:CSL|SRK|TW|CSLD)-\\d+)(?:\\W|$)"

getGroupsFromFirstMatch :: R.Regex -> Text -> IO [(Int, Int, Text)]
getGroupsFromFirstMatch reg text = do
    R.setText reg text
    isM <- R.findNext reg
    if isM
       then do
           gC <- R.groupCount reg
           flip mapM [0..gC] $ \g -> do
               st <- fromIntegral <$> R.start_ reg g
               ed <- fromIntegral <$> R.end_ reg g
               return $ (st, ed, T.take (ed - st) $ T.drop st $ text)
       else return []

trackRegexpText :: Text
trackRegexpText = "TRACK:\\s*(\\S*)\\s*(\\S*)"

clockRegexpText :: Text
clockRegexpText = "CLOCK:\\s*\\[([^\\]]+)\\]\\s*-{1,}\\s*\\[([^\\]]+)\\]"

emptyParsingCtx :: R.Regex -> R.Regex -> R.Regex -> ParsingCtx
emptyParsingCtx = ParsingCtx Nothing Nothing Nothing mempty

parseOrg :: Text -> IO IssueRecordMap
parseOrg orgFile = do
    issueReg <- R.regex [R.CaseInsensitive] issueRegexpText
    clockReg <- R.regex [R.CaseInsensitive] clockRegexpText
    trackReg <- R.regex [R.CaseInsensitive] trackRegexpText
    fmap pcResult $
      foldM parseOrgLine (emptyParsingCtx issueReg clockReg trackReg) $
      T.lines orgFile

insertToMap :: Text -> TimeRecord -> IssueRecordMap -> IssueRecordMap
insertToMap issueId rec m =
  case issueId `HM.lookup` m of
    Just l -> HM.insert issueId (rec:l) m
    _      -> HM.insert issueId [rec] m

processHeader :: Int -> ParsingCtx -> Text -> IO ParsingCtx
processHeader depth (invalidateCtxOnDepth depth -> ctx) line = do
    putStrLn $ "Parsing header of depth " <> show depth <> ": " <> show line
    case pcIssueId ctx of
      Nothing -> do
          mIssueId <- matchIssueId (issueRegex ctx) line
          case mIssueId of
            Just issueId ->
                return ctx { pcIssueId = Just (depth, issueId) }
            _ -> return ctx
      Just _ ->
          case pcDescription ctx of
            Just _ -> return ctx
            _ ->
              let desc = T.dropWhile (\c -> isSpace c || c == '*') line
               in return ctx { pcDescription = Just (depth, desc) }

invalidateCtxOnDepth :: Int -> ParsingCtx -> ParsingCtx
invalidateCtxOnDepth depth = invalidateIssueId . invalidateDescription
  where
    invalidateIssueId ctx' = ctx' { pcIssueId = invModify (pcIssueId ctx') }
    invalidateDescription ctx' = ctx' { pcDescription = invModify (pcDescription ctx') }
    invModify x@(Just (d, _))
      | d >= depth = Nothing
      | otherwise = x
    invModify x = x

parseOrgLine :: ParsingCtx -> Text -> IO ParsingCtx
parseOrgLine ctx@ParsingCtx{..} line =
    case headerDepthM of
      Just headerDepth -> do
          case pcYTSection of
            Nothing -> processHeaderInNonYT headerDepth ctx
            Just ytDepth ->
              if headerDepth <= ytDepth
                 then processHeaderInNonYT headerDepth $
                        ctx { pcYTSection = Nothing
                            , pcIssueId = Nothing
                            , pcDescription = Nothing
                            }
                 else processHeader headerDepth ctx line
      _ -> case pcIssueId of
             Just (depth, issueId) -> do
                mRec <- parseTimeRecord clockRegex trackRegex (snd <$> pcDescription) line
                case mRec of
                  Just rec -> return ctx { pcResult = insertToMap issueId rec pcResult }
                  _ -> return ctx
             _ -> return ctx
  where
    processHeaderInNonYT headerDepth ctx' = do
        let isYTSectionMarker = not . T.null $ snd $ T.breakOn ytParsingCtxTag line
        if isYTSectionMarker
           then processHeader headerDepth (ctx' { pcYTSection = Just headerDepth }) line
           else return ctx'
    headerDepthM :: Maybe Int
    headerDepthM = if "*" `T.isPrefixOf` line
                     then Just $ T.length $ T.takeWhile (== '*') line
                     else Nothing

matchIssueId :: R.Regex -> Text -> IO (Maybe Text)
matchIssueId = getFstMatch

getFstMatch :: R.Regex -> Text -> IO (Maybe Text)
getFstMatch reg line = f <$> getGroupsFromFirstMatch reg line
  where
    f (_:x:_) = Just $ get_3 x
    f _       = Nothing

getPairMatch :: R.Regex -> Text -> IO (Maybe (Text, Text))
getPairMatch reg line = f <$> getGroupsFromFirstMatch reg line
  where
    f (_:x:y:_) = Just (get_3 x, get_3 y)
    f _         = Nothing

get_3 :: (a, b, c) -> c
get_3 (_, _, a) = a

parseTimeRecord :: R.Regex -> R.Regex -> Maybe Text -> Text -> IO (Maybe TimeRecord)
parseTimeRecord clockReg trackReg mDesc line =
    safeHead . catMaybes <$>
      sequence [ clockF <$> getPairMatch clockReg line
               , trackF <$> getPairMatch trackReg line ]
  where
    desc = fromMaybe "" mDesc
    clockF (Just (parseTimeH -> from, parseTimeH -> to)) =
        ClockRecord desc <$> from <*> to
    clockF _ = Nothing
    trackF (Just (parseDayH -> from, parseDurationH -> to)) =
        TrackRecord desc <$> from <*> to
    trackF _ = Nothing

