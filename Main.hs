module Main where

import System.Console.Terminfo.Base
import System.Console.Terminfo.Effects

boldText :: Capability (TermOutput)
boldText = do
  --out  <- tiGetOutput1 "bold"
  bold <- withBold
  return (bold (termText "bold text"))

main :: IO ()
main = do
  term <- setupTermFromEnv
  case getCapability term boldText of
    Just to -> runTermOutput term to
    _       -> putStrLn "E: no term support"
  
