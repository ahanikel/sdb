{-# LANGUAGE RecordWildCards #-}

module Commands where

import Display
import Store
import TarArchive
import Segment
import Index
import Graph
import Binaries

import qualified Data.List            as L
import           Data.Maybe
import           System.Environment   ( getArgs )
import           System.IO

data Config = Config { cfgOptions :: [Option]
                     , cfgArgs    :: [String]
                     , cfgCommand :: Command
                     }

data Option = OptDisplayHex
            | OptDisplayBinary
            | OptAll
  deriving (Show, Eq)

data Command = Command { cmdName :: String
                       , cmdSyno :: String
                       , cmdDesc :: String
                       , cmdExec :: Config -> IO ()
                       }

commands = [ cmdTars
           , cmdEntries
           , cmdSegments
           , cmdSegment
           , cmdIndex
           , cmdGraph
           , cmdBinaries
           ]

cmdTars     = Command "tars"
                      "sdb tars [--all] <dir>"
                      "Display the tar files / --all files of <dir>."
                      ( \Config {..} ->
                          case cfgArgs of
                            [path] -> mapM_ putStrLn =<< tars path (OptAll `elem` cfgOptions)
                            _      -> usage' cfgCommand
                      )

cmdEntries  = Command "entries"
                      "sdb entries <tarFile>"
                      "Display the entries in <tarFile>."
                      ( \Config {..} ->
                          case cfgArgs of
                            [path] -> mapM_ putStrLn =<< entries path
                            _      -> usage' cfgCommand
                      )

cmdSegments = Command "segments"
                      "sdb segments <tarFile>"
                      "Display the list of segments in <tarFile>"
                      ( \Config {..} ->
                          case cfgArgs of
                            [path] -> mapM_ (putStrLn . display) =<< segments path
                            _      -> usage' cfgCommand
                      )

cmdSegment  = Command "segment"
                      "sdb segment <tarFile> <segmentId>"
                      "Display the segment <segmentId> from <tarFile>"
                      ( \Config {..} ->
                          case cfgArgs of
                            [path, segmentId] -> either (hPutStrLn stderr)
                                                        (putStrLn . display)
                                                 =<< segment path segmentId
                            _      -> usage' cfgCommand
                      )

cmdIndex    = Command "index"
                      "sdb index <tarFile>"
                      "Display the segment index from <tarFile>."
                      ( \Config {..} ->
                          case cfgArgs of
                            [path] -> mapM_ (putStrLn . display) =<< index path
                            _      -> usage' cfgCommand
                      )

cmdGraph    = Command "graph"
                      "sdb graph <tarFile>"
                      "Display the segment graph from <tarFile>."
                      ( \Config {..} ->
                          case cfgArgs of
                            [path] -> mapM_ (putStrLn . display) =<< graph path
                            _      -> usage' cfgCommand
                      )

cmdBinaries = Command "binaries"
                      "sdb binaries <tarFile>"
                      "Display the binary index from <tarFile>"
                      ( \Config {..} ->
                          case cfgArgs of
                            [path] -> mapM_ (putStrLn . display) =<< binaries path
                            _      -> usage' cfgCommand
                      )


optionsMap = [ ("--hex"   , OptDisplayHex)
             , ("--all"   , OptAll)
             , ("--binary", OptDisplayBinary)
             ]

run :: IO ()
run = do
  args <- getArgs
  case args of
    (cmd : args') -> do
      let (options, cfgArgs) = L.partition byDashes args'
          cfgOptions         = catMaybes $ map (flip lookup optionsMap) options
      maybe usage id $ do
          cfgCommand        <- L.find (byName cmd) commands
          return $ cmdExec cfgCommand Config {..}
    _ -> usage

  where
          byName             = \name -> (name ==) . cmdName
          byDashes           = ("--" `L.isPrefixOf`)

usage = mapM_ usage' commands

usage' cmd = hPutStrLn stderr $ cmdSyno cmd ++ " # " ++ cmdDesc cmd
