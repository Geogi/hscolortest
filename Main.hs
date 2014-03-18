module Main where

import Control.Monad

import System.Console.Terminfo.Base
import System.Console.Terminfo.Effects
import System.Console.Terminfo.Color

withBoldRed :: TermStr s => Capability (s -> s)
withBoldRed = do
  bold  <- boldOn
  color <- withForegroundColor
  stop  <- allAttributesOff
  return (\str -> bold <#> color Red str <#> stop)

boldText :: Capability (TermOutput)
boldText = do
  bold  <- withBold
  color <- withForegroundColor
  under <- withUnderline
  boldred <- withBoldRed
  standout <- withStandout
  return (termText "normal text " <#> bold (termText "bold text") <#>
          termText " normal text\n" <#> under (termText "underlined") <#>
          color Blue (termText " blue\n") <#>
          standout (termText "standout") <#> boldred (termText " bb\n"))

main :: IO ()
main = do
  term <- setupTermFromEnv
  case getCapability term boldText of
    Just to -> runTermOutput term to
    _       -> putStrLn "E: no term support"
  
