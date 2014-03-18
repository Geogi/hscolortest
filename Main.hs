module Main where

import System.Console.Terminfo.Base
import System.Console.Terminfo.Effects

boldText :: Capability (TermOutput)
boldText = do
  bold <- withBold
  return (termText "normal text " <#> bold (termText "bold text") <#> termText " normal text\n")

main :: IO ()
main = do
  term <- setupTermFromEnv
  case getCapability term boldText of
    Just to -> runTermOutput term to
    _       -> putStrLn "E: no term support"
  
