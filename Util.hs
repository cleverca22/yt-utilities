module Util where

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

