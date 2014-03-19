module Main where

import Format
import RedError
import Trace

noone :: Int -> OrangeComp Int
noone i |
  ftrace (fconcat (format "noone " Bold) (format (show i) Blue))
  False = undefined
noone 1 = throwError (RedError "NO:" "ones!")
noone i = return 1

main :: IO ()
main = do
  fprint (format "some text\n" Blue)
  fprint (fcompose (format "more text\n" Red) Bold)
  case noone 1 of
    Left e  -> fprintln e
    Right i -> undefined
