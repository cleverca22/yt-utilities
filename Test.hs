#!/usr/bin/env stack
-- stack runghc --package optparse-simple --package shell-conduit --package transformers --package time --package extra --package text-icu --package unordered-containers

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Monad.IO.Class     (liftIO)
import           Data.Conduit.Shell
import           Data.Monoid                ((<>))
import qualified Data.Text.IO               as T
import           Options.Applicative.Simple (Parser, auto, empty, flag', help, long,
                                             metavar, option, optional, showDefault,
                                             simpleOptions, strOption, switch, value,
                                             (<|>))
import           System.Exit                (die)
import           System.FilePath.Posix      ((</>))

import           Parsing                    (parseOrg)

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
    echo =<< show <$> (liftIO $ parseOrg =<< T.readFile orgFile)

-- | CLI-options for deployer.
data Options = Options
    { --itIsProductionCluster :: Bool
      orgFile :: String
    , text    :: String
    --, numberOfNodes         :: Int
    --, noBuild               :: Bool
    }

optionsParser :: Parser Options
optionsParser = Options <$>
    -- ( flag' True  (long "prod")
    --   <|> flag' False (long "dev") )
    -- <*>
      strOption (
         long       "org"
      <> metavar    "FILE" )
      <*> strOption (
         long       "text"
      <> metavar    "TIME" )
    -- <*>
    --   option auto (
    --      long       "nodes"
    --   <> metavar    "NUMBER"
    --   <> help       "Number of nodes in a cluster, from 1 to 100." )
    -- <*>
    --   switch (
    --      long       "no-build"
    --   <> help       "Don't build cluster, assume it's built" )
