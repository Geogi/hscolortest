module Main where

import Format
import RedError

noone :: Int -> OrangeComp Int
noone 1 = throwError (RedError "NO:" "ones!")
noone i = return 1

main :: IO ()
main = do
  fprint (format "some text\n" Blue)
  fprint (compose (format "more text\n" Red) Bold)
  case noone 1 of
    Left e  -> fprintln e
    Right i -> undefined
