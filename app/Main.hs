module Main where

import Segment
import Store
import TarArchive
import Index
import Graph

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
