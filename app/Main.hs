module Main where

import Segment
import Store
import TarArchive
import Index
import Graph
import Binaries

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("tars"    : args')       -> tars args'
    ["entries",  tarFile]     -> entries tarFile
    ["segments", tarFile]     -> segments tarFile
    ["segment",  tarFile, id] -> segment tarFile id
    ["index",    tarFile]     -> index tarFile
    ["graph",    tarFile]     -> graph tarFile
    ["binaries", tarFile]     -> binaries tarFile
    _                         -> usage

usage = putStrLn $ unlines [ "sdb tars     <dir> [--all]  # List tarfiles in dir (default '.')."
                           , "sdb entries  <tarFile>      # List entries in tarfile."
                           , "sdb segments <tarFile>      # List segments in tarfile."
                           , "sdb segment  <tarFile> <id> # display records in segment."
                           , "sdb index    <tarFile>      # display tarfile index."
                           , "sdb graph    <tarFile>      # display graph file in tar."
                           , "sdb binaries <tarFile>      # display binary references in tar."
                           ]
