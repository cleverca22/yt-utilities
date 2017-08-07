{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Parsing
       ( parseOrg
       , IssueRecordMap
       , TimeRecord (..)
       , Duration
       , ytParsingCtxTag
       , formatDuration
       ) where

import           Control.Monad       (foldM, guard)
import           Data.Bifunctor      (bimap, second)
import           Data.Char           (isSpace)
import qualified Data.HashMap.Strict as HM
import           Data.List.Extra     (replace)
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.Monoid         (First (..), (<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.ICU.Regex as R
import           Data.Time           (Day, UTCTime, defaultTimeLocale, parseTimeM)
import           Text.Read           (readMaybe)

import           Util                (safeHead)

type Duration = Word

-- | Youtrack project ids.
projectNames :: [String]
projectNames = [ "CSL", "CSLA", "CSLD", "CSE", "CSM"
               --, "CSLTA" , "BOS", "CSLREQ", "DTP", "HSK", "IH", "RSC", "POS", "PV" -- projects exist, but are not actual and/or have timetracking disabled
               , "CSLTC", "DAEF", "DEVOPS", "IMRF", "LW", "MT", "SRK", "SU", "TW", "VD"
               ]

-- | Keywords that are commonly used in org-mode.
keywords :: [String]
keywords = ["TODO","DONE","STARTED","WIP","WAITING","IN,PROGRESS","CANCELED"
           ,"TD","ST","DN","WT","CL"]

data TimeRecord = ClockRecord Text UTCTime UTCTime
                | TrackRecord Text Day Duration
  deriving Show

-- | Presense of this substring in the header name includes it into
-- working scope of the parser. Everything else is skipped. You can
-- put it into org tag.
ytParsingCtxTag :: Text
ytParsingCtxTag = "ytexport"

type IssueRecordMap = HM.HashMap Text [TimeRecord]

data ParsingCtx = ParsingCtx
    {
      pcYTSection   :: Maybe Int
      -- ^ If we encountered 'ytParsingCtxTag'ged header we set this
      -- value to @Just d@ where @d@ is the depth of header.
    , pcIssueId     :: Maybe (Int, Text, Text)
      -- ^ Depth, title and description of first seen header that has projectName id inside.
    , pcDescription :: Maybe (Int, Text)
      -- ^ Depth/title of last header seen after we've set 'pcIssueId'
      -- to @Just@. Basically any subheader after top projectName
      -- issue header.

    , pcResult      :: IssueRecordMap
      -- ^ Parsing result we accumulate.

    -- Set of regexps we use for parsing.
    -- They don't change. why do we have them in state? TODO
    , issueRegex    :: R.Regex
    , clockRegex    :: R.Regex
    , trackRegex    :: R.Regex
    }

timeFormats :: [String]
timeFormats = ["%F %T", "%F %R"]

dayFormats :: [String]
dayFormats = ["%F"]

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

-- | Converts duration into 5w4d2h3m format.
formatDuration :: Duration -> String
formatDuration minutesN =
    mconcat [ mP weeksN "w"
            , mP daysN "d"
            , mP hoursN "h"
            , mP minutesN4 "m"
            ]
  where
    mP x p = if x == 0 then "" else show x <> p

    weeksN = minutesN `div` (5 * 8 * 60)
    minutesN2 = minutesN `mod` (5 * 8 * 60)
    daysN = minutesN2 `div` (8 * 60)
    minutesN3 = minutesN2 `mod` (8 * 60)
    hoursN = minutesN3 `div` 60
    minutesN4 = minutesN3 `mod` 60

parseDayH :: Text -> Maybe Day
parseDayH (T.unpack -> time) = safeHead $ catMaybes $
    map (\f -> parseTimeM True defaultTimeLocale f time) dayFormats

parseTimeH :: Text -> Maybe UTCTime
parseTimeH (T.unpack -> time) = safeHead $ catMaybes $
    map (\f -> parseTimeM True defaultTimeLocale f time') timeFormats
  where
    time' = foldr (flip replace "") time dayOfWeek

-- Something like
-- "***** TODO [#A] SRK-X Description with som/e 5 words :maybe?              :tag1:tag2:"
issueRegexpText :: Text
issueRegexpText =
    "^(?:\\**)\\s*(?:"<>todokws<>")?\\s*(?:\\[#\\w\\])?\\s*((?:"<>projects<>")-\\d+)?\\s*((?:.)*?)\\s*(?:(?::\\w+)+:)?$"
  where
    projects = T.intercalate "|" (map T.pack projectNames)
    todokws = T.intercalate "|" (map T.pack keywords)

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
insertToMap issueId record m =
  case issueId `HM.lookup` m of
    Just l -> HM.insert issueId (record:l) m
    _      -> HM.insert issueId [record] m

-- | Parse header inside YT-related subtree.
processHeader :: Int -> ParsingCtx -> Text -> IO ParsingCtx
processHeader depth (invalidateCtxOnDepth depth -> ctx) line = do
    putStrLn $ "Parsing header of depth " <> show depth <> ": " <> show line
    curLineM <- getPairMatch (issueRegex ctx) line
    case (pcIssueId ctx, curLineM) of
      -- We're inside the subtree that is identified as 'some youtrack
      -- task to export'.
      (Just _,Just (_, descr)) -> do
          putStrLn $ "matched descr: " <> T.unpack descr
          pure $ ctx { pcDescription = Just (depth, descr) }
      -- We're about to check whether this subtree is related to yt
      -- clocking or it's a intermediate node.
      (Nothing, Just (issueId,issueTopDesc))
          | not (T.null issueId) -> do
              putStrLn $ "matched " <> show (depth, issueId, issueTopDesc)
              pure ctx { pcIssueId = Just (depth, issueId, issueTopDesc) }
          | otherwise -> putStrLn "issueId is empty" >> pure ctx
      _ -> pure ctx

invalidateCtxOnDepth :: Int -> ParsingCtx -> ParsingCtx
invalidateCtxOnDepth depth = invalidateIssueId . invalidateDescription
  where
    invalidateIssueId ctx' = ctx' { pcIssueId = invModify3 (pcIssueId ctx') }
    invalidateDescription ctx' = ctx' { pcDescription = invModify2 (pcDescription ctx') }
    -- yeah lenses
    invModify3 (Just x@(d, _, _)) = x <$ guard (d < depth)
    invModify3 _                  = Nothing
    invModify2 (Just x@(d, _)) = x <$ guard (d < depth)
    invModify2 _               = Nothing

-- | Parse single orgmode line.
parseOrgLine :: ParsingCtx -> Text -> IO ParsingCtx
parseOrgLine ctx@ParsingCtx{..} line = do
  --putStrLn $ "Processing " <> T.unpack line
  case (pcYTSection, headerDepthM) of
    -- We're not yet in the YT scope, viewing at a header.
    (Nothing, Just headerDepth) -> processHeaderInNonYT headerDepth ctx
    -- We're in yt scope and processing the header.
    (Just ytDepth, Just headerDepth) ->
          if headerDepth <= ytDepth
             -- We exit YT scope if current header has depth less or
             -- equal to yt's scope header.
             then processHeaderInNonYT headerDepth $
                    ctx { pcYTSection = Nothing
                        , pcIssueId = Nothing
                        , pcDescription = Nothing
                        }
             -- Otherwise we process subtree header.
             else processHeader headerDepth ctx line
    -- We're in YT mode but process something other then header --
    -- most importantly time-track lines.
    (Just ytDepth, Nothing) -> do
        let recordName =
                getFirst $ mconcat $ map First $
                [snd <$> pcDescription,(\(_,_,r) -> r) <$> pcIssueId]

        mRec <- parseTimeRecord clockRegex trackRegex recordName line
        case (pcIssueId,mRec) of
            -- Found a CLOCK/TRACK inside YT scope inside subtree
            -- which doesn't have YT id in the name.
            (Nothing,Just s) -> error $
                "Found a valid CLOCK inside yt subtree " <>
                "that is not related to YT task" <>
                show s
            -- We've found a decent track inside YT context inside
            -- header with YT id so we add it to result.
            (Just (depth, issueId, _), Just record) ->
                 pure ctx { pcResult = insertToMap issueId record pcResult }
            -- Everything else, we do not care.
            _ -> pure ctx
    -- Skipping
    (Nothing, Nothing) -> pure ctx
  where
    processHeaderInNonYT headerDepth ctx' = do
        let isYTSectionMarker = not . T.null $ snd $ T.breakOn ytParsingCtxTag line
        if isYTSectionMarker
           then processHeader headerDepth (ctx' { pcYTSection = Just headerDepth }) line
           else return ctx'
    headerDepthM :: Maybe Int
    headerDepthM
        |"*" `T.isPrefixOf` line = Just $ T.length $ T.takeWhile (== '*') line
        | otherwise = Nothing

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
get_3 (_, _, c) = c

parseTimeRecord ::
       R.Regex -> R.Regex -> Maybe Text -> Text -> IO (Maybe TimeRecord)
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
