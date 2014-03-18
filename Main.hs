module Main where

import Format

main :: IO ()
main = do
  fprint (format "some text\n" Blue)
  fprint (compose (format "more text\n" Red) Bold)
