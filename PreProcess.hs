{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module PreProcess
       ( DurationMap
       , preProcess
       , toMap
       , IssueId
       ) where

import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import           Data.Time           (Day, DiffTime, diffTimeToPicoseconds, utctDay,
                                      utctDayTime)
import           Parsing             (Duration, IssueRecordMap, TimeRecord (..))

type IssueId = Text

type DurationMap = HM.HashMap Day (HM.HashMap Text Duration)

instance Hashable Day where
    hashWithSalt s = hashWithSalt s . fromEnum

preProcess :: IssueRecordMap -> HM.HashMap IssueId DurationMap
preProcess = fmap processIssueId

processIssueId :: [TimeRecord] -> DurationMap
processIssueId = toMap . concatMap processRecord

processRecord :: TimeRecord -> [(Day, Text, Duration)]
processRecord (TrackRecord desc day dur) = [(day, desc, dur)]
processRecord (ClockRecord desc start end)
    | startD == endD =
          [ (startD, desc, endM - startM) ]
    | otherwise =
          [ (startD, desc, minutesInDay - startM) ]
          ++ (if endM == 0 then [] else [(endD, desc, endM)])
          ++ map (,desc,minutesInDay) [succ startD .. pred endD]
  where
    startM = toMinutes (utctDayTime start)
    endM = toMinutes (utctDayTime end)
    startD = utctDay start
    endD = utctDay end

minutesInDay :: Duration
minutesInDay = 60*24

toMap :: [(Day, Text, Duration)] -> DurationMap
toMap = foldr insert mempty
  where
    insert (day, desc, dur) = HM.alter insert1 day
      where
        insert1 (Just m) = Just $ HM.alter insert2 desc m
        insert1 _        = Just $ HM.singleton desc dur
        insert2 (Just dur') = Just $ dur' + dur
        insert2 _           = Just dur

toMinutes :: DiffTime -> Duration
toMinutes t = fromIntegral $ diffTimeToPicoseconds t `div` 60 `div` 1000000000000
