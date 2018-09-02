module Display where

class Display c where
  display :: c -> String
