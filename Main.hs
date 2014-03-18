module Main where

import Control.Monad

import System.Console.Terminfo.Base
import System.Console.Terminfo.Effects
import System.Console.Terminfo.Color

boldText :: Capability (TermOutput)
boldText = do
  bold      <- withBold
  color     <- withForegroundColor
  boldcolor <- mplus withBold withForegroundColor
  return (termText "normal text " <#> bold (termText "bold text") <#> termText " normal text\n" <#>
          boldcolor Blue (termText "bold and blue") <#> termText " normal again")

main :: IO ()
main = do
  term <- setupTermFromEnv
  case getCapability term boldText of
    Just to -> runTermOutput term to
    _       -> putStrLn "E: no term support"
  
